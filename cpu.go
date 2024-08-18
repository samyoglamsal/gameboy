/* Samyog Lamsal 2024 */

package cpu

/* Struct to represent a register. For example the struct for AF would have high = A and low = F */
type CPU struct {
    a, b, c, d, e, f, h, l uint8
    sp, pc uint16
    memory [0xFFFF]uint8
    interrupt bool
}

// Used for reading immediate values. Increases PC
func (cpu *CPU) ReadMemory() uint8 {
    next_byte := cpu.memory[cpu.pc]
    cpu.pc++

    return next_byte
}

// Pushing 16 bit values onto stack. Always put MSB first
func (cpu *CPU) Push(data uint8) {
    cpu.sp--
    cpu.memory[cpu.sp] = data
}

// Pops 16 bit values from stack. Returned as 2 uint16 values with LSB first to mirror the endianness of the GB
func (cpu *CPU) Pop() (uint8, uint8) {
    lsb := cpu.memory[cpu.sp]
    cpu.sp++
    msb := cpu.memory[cpu.sp]
    cpu.sp++

    return lsb, msb
}

func (cpu *CPU) Add8(first uint8, second uint8, carry bool) uint8 {
    var result uint16 = uint16(first) + uint16(second)
    if carry && (cpu.f >> 4 & 0x1) == 0x1 {
        result++ 
    }

    cpu.SetZ(result & 0xFF == 0x00)
    cpu.SetN(false)
    cpu.SetH(result & 0x10 == 0x10)
    cpu.SetC(result & 0x100 == 0x100)

    return uint8(result)
}

func (cpu *CPU) Add16(first uint16, second uint16) uint16 {
    var result uint32 = uint32(first) + uint32(second)

    cpu.SetN(false)
    cpu.SetH(result & 0x1000 == 0x1000)
    cpu.SetC(result & 0x10000 == 0x10000)

    return uint16(result)
}

func (cpu *CPU) Add16Signed(first uint16, second int8) uint16 {
    var result int32 = int32(first) + int32(second)

    cpu.SetZ(false)
    cpu.SetN(false)
    cpu.SetH((result & 0x10) == 0x10)
    cpu.SetC((result & 0x100) == 0x100)

    return uint16(result)
}

func (cpu *CPU) Sub8(first uint8, second uint8, carry bool) uint8 {
    var result int16 = int16(first) - int16(second)
    if carry && (cpu.f >> 4 & 0x1) == 0x1 {
        result--
    }

    cpu.SetZ(result & 0xFF == 0)
    cpu.SetN(true)
    cpu.SetH(result & 0x10 == 0x10)
    cpu.SetC(result & 0x100 == 0x100)

    return uint8(result)
}

func (cpu *CPU) Increment(value uint8) uint8 {
    updated_value := value + 1

    cpu.SetZ(updated_value == 0)
    cpu.SetN(false)
    cpu.SetH(updated_value & 0x10 == 0x10)

    return updated_value
}

func (cpu *CPU) Decrement(value uint8) uint8 {
    updated_value := value - 1

    cpu.SetZ(updated_value == 0)
    cpu.SetN(true)
    cpu.SetH(updated_value & 0x10 == 0x10)

    return updated_value
}

func (cpu *CPU) And(value uint8) uint8 {
    result := cpu.a & value

    cpu.SetZ(result == 0)
    cpu.SetN(false)
    cpu.SetH(true)
    cpu.SetC(false)

    return result
}

func (cpu *CPU) Or(value uint8) uint8 {
    result := cpu.a | value

    cpu.SetZ(result == 0)
    cpu.SetN(false)
    cpu.SetH(false)
    cpu.SetC(false)

    return result
}

func (cpu *CPU) Xor(value uint8) uint8 {
    result := cpu.a ^ value

    cpu.SetZ(result == 0)
    cpu.SetN(false)
    cpu.SetH(false)
    cpu.SetC(false)

    return result
}

func (cpu *CPU) GetZ() bool {
    return (cpu.f >> 7) & 0x1 == 0x1
}

func (cpu *CPU) SetZ(on bool) {
    if on {
        cpu.f |= 0x80
    } else {
        cpu.f &= 0x7F
    }

}

func (cpu *CPU) GetN() bool {
    return (cpu.f >> 6) & 0x1 == 0x1
}

func (cpu *CPU) SetN(on bool) {
    if on {
        cpu.f |= 0x40
    } else {
        cpu.f &= 0xBF
    }
}

func (cpu *CPU) GetH() bool {
    return (cpu.f >> 5) & 0x1 == 0x1
}

func (cpu *CPU) SetH(on bool) {
    if on {
        cpu.f &= 0x20
    } else {
        cpu.f &= 0xDF
    }
}

func (cpu *CPU) GetC() bool {
    return (cpu.f >> 4) & 0x1 == 0x1
}

func (cpu *CPU) SetC(on bool) {
    if on {
        cpu.f |= 0x10
    } else {
        cpu.f &= 0xEF
    }
}

func (cpu *CPU) ExecuteInstruction() {
    opcode := cpu.ReadMemory()

    switch opcode {
        case 0x00: // NOP
        case 0x01: // LD BC, u16 
            lsb, msb := cpu.ReadMemory(), cpu.ReadMemory()
            cpu.b, cpu.c = msb, lsb
        case 0x02: // LD (BC), A
            memory_address := ConcatBytes(cpu.b, cpu.c)
            cpu.memory[memory_address] = cpu.a
        case 0x03: // INC BC
            tmp := ConcatBytes(cpu.b, cpu.c) + 1
            cpu.b, cpu.c = MSB(tmp), LSB(tmp)
        case 0x04: // INC B
            cpu.b = cpu.Increment(cpu.b)
        case 0x05: // DEC B
            cpu.b = cpu.Decrement(cpu.b)
        case 0x06: // LD B, u8
            cpu.b = cpu.ReadMemory()
        case 0x07: // TODO: RLCA. Like RLC A but faster
        case 0x08: // LD (u16), SP
            lsb, msb := cpu.ReadMemory(), cpu.ReadMemory()
            memory_address := ConcatBytes(msb, lsb)
            cpu.memory[memory_address], cpu.memory[memory_address+ 1] = LSB(cpu.sp), MSB(cpu.sp)
        case 0x09: // ADD HL, BC
            hl, bc := ConcatBytes(cpu.h, cpu.l), ConcatBytes(cpu.b, cpu.c)
            sum := cpu.Add16(hl, bc)
            cpu.h, cpu.l = MSB(sum), LSB(sum)
        case 0x0A: // LD A, (BC)
            memory_address := ConcatBytes(cpu.b, cpu.c)
            cpu.a = cpu.memory[memory_address]
        case 0x0B: // DEC BC
            tmp := ConcatBytes(cpu.b, cpu.c) - 1
            cpu.b, cpu.c = MSB(tmp), LSB(tmp)
        case 0x0C: // INC C
            cpu.c = cpu.Increment(cpu.c)
        case 0x0D: // DEC C
            cpu.c = cpu.Decrement(cpu.c)
        case 0x0E: // LD C, u8
            cpu.c = cpu.ReadMemory()
        case 0x0F: // TODO: RRCA
        
        case 0x10: // TODO: STOP
        case 0x11: // LD DE, u16
            lsb, msb := cpu.ReadMemory(), cpu.ReadMemory()
            cpu.d, cpu.e = msb, lsb
        case 0x12: // LD (DE), A
            memory_address := ConcatBytes(cpu.d, cpu.e)
            cpu.memory[memory_address] = cpu.a
        case 0x13: // INC DE
            tmp := ConcatBytes(cpu.d, cpu.e) + 1
            cpu.d, cpu.e = MSB(tmp), LSB(tmp)
        case 0x14: // INC D
            cpu.d = cpu.Increment(cpu.d)
        case 0x15: // DEC D
            cpu.d = cpu.Decrement(cpu.d)
        case 0x16: // LD D, u8
            cpu.d = cpu.ReadMemory()
        case 0x17: // TODO: RLA
        case 0x18: // JR i8
            immediate := int32(cpu.ReadMemory())
            cpu.sp = uint16(int32(cpu.sp) + immediate)
        case 0x19: // ADD HL, DE
            hl, de := ConcatBytes(cpu.h, cpu.l), ConcatBytes(cpu.d, cpu.e)
            sum := cpu.Add16(hl, de)
            cpu.h, cpu.l = MSB(sum), LSB(sum)
        case 0x1A: // LD A, (DE)
            memory_address := ConcatBytes(cpu.d, cpu.e)
            cpu.a = cpu.memory[memory_address]
        case 0x1B: // DEC DE
            tmp := ConcatBytes(cpu.d, cpu.e) - 1
            cpu.d, cpu.e = MSB(tmp), LSB(tmp)
        case 0x1C: // INC E
            cpu.e = cpu.Increment(cpu.e)
        case 0x1D: // DEC E
            cpu.e = cpu.Decrement(cpu.e)
        case 0x1E: // LD E, u8
            cpu.e = cpu.ReadMemory()
        case 0x1F: // TODO: RRA

        case 0x20: // JR NZ, i8
            immediate := int32(cpu.ReadMemory())
            if !cpu.GetZ() {
                cpu.sp = uint16(int32(cpu.sp) + immediate)
            }
        case 0x21: // LD HL, u16
            lsb, msb := cpu.ReadMemory(), cpu.ReadMemory()
            cpu.h, cpu.l = msb, lsb
        case 0x22: // LD (HL+) A
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.memory[memory_address] = cpu.a
            memory_address++
            cpu.h, cpu.l = MSB(memory_address), LSB(memory_address)
        case 0x23: // INC HL
            tmp := ConcatBytes(cpu.h, cpu.l) + 1
            cpu.h, cpu.l = MSB(tmp), LSB(tmp)
        case 0x24: // INC H
            cpu.h = cpu.Increment(cpu.h)
        case 0x25: // DEC H
            cpu.h = cpu.Decrement(cpu.h)
        case 0x26: // LD H, u8
            cpu.h = cpu.ReadMemory()
        case 0x28: // JR Z, i8
            immediate := int32(cpu.ReadMemory())
            if cpu.GetZ() {
                cpu.sp = uint16(int32(cpu.sp) + immediate)
            }
        case 0x29: // ADD HL, HL
            hl := ConcatBytes(cpu.h, cpu.l)
            sum := cpu.Add16(hl, hl)
            cpu.h, cpu.l = MSB(sum), LSB(sum)
        case 0x2A: // LD A, (HL+)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.a = cpu.memory[memory_address]
            memory_address++
            cpu.h, cpu.l = MSB(memory_address), LSB(memory_address)
        case 0x2B: // DEC HL
            tmp := ConcatBytes(cpu.h, cpu.l) - 1
            cpu.h, cpu.l = MSB(tmp), LSB(tmp)
        case 0x2C: // INC L
            cpu.l = cpu.Increment(cpu.l)
        case 0x2D: // DEC L
            cpu.l = cpu.Decrement(cpu.l)
        case 0x2E: // LD L, u8
            cpu.l = cpu.ReadMemory()    
        case 0x2F: // CPL
            cpu.a = ^cpu.a
            cpu.SetN(true)
            cpu.SetH(true)

        case 0x30: // JR NC, i8
            immediate := int32(cpu.ReadMemory())
            if !cpu.GetC() {
                cpu.sp = uint16(int32(cpu.sp) + immediate)
            }
        case 0x31: // LD SP, u16
            lsb, msb := cpu.ReadMemory(), cpu.ReadMemory()
            cpu.sp = ConcatBytes(msb, lsb)
        case 0x32: // LD (HL-), A
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.memory[memory_address] = cpu.a
            memory_address--
            cpu.h, cpu.l = MSB(memory_address), LSB(memory_address)
        case 0x33: // INC SP
            cpu.sp++
        case 0x34: // INC (HL)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.memory[memory_address] = cpu.Increment(cpu.memory[memory_address])
        case 0x35: // DEC (HL)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.memory[memory_address] = cpu.Decrement(cpu.memory[memory_address])
        case 0x36: // LD (HL), u8
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.memory[memory_address] = cpu.ReadMemory()
        case 0x37: // SCF
            cpu.SetN(false)
            cpu.SetH(false)
            cpu.SetC(true)
        case 0x38: // JR C, i8
            immediate := int32(cpu.ReadMemory())
            if cpu.GetC() {
                cpu.sp = uint16(int32(cpu.sp) + immediate)
            }
        case 0x39: // ADD HL, SP
            hl := ConcatBytes(cpu.h, cpu.l)
            sum := cpu.Add16(hl, cpu.sp)
            cpu.h, cpu.l = MSB(sum), LSB(sum)
        case 0x3A: // LD A, (HL-)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.a = cpu.memory[memory_address]
            memory_address--
            cpu.h, cpu.l = MSB(memory_address), LSB(memory_address)
        case 0x3B: // DEC SP
            cpu.sp--
        case 0x3C: // INC A
            cpu.a = cpu.Increment(cpu.a)
        case 0x3D: // DEC A
            cpu.a = cpu.Decrement(cpu.a)
        case 0x3E: // LD A, u8
            cpu.a = cpu.ReadMemory()
        case 0x3F: // CCF
            cpu.SetN(false)
            cpu.SetH(false)
            cpu.SetC((cpu.f >> 4) & 1 == 0)
            
        case 0x40: // LD B, B
        case 0x41: // LD B, C
            cpu.b = cpu.c
        case 0x42: // LD B, D
            cpu.b = cpu.d
        case 0x43: // LD B, E
            cpu.b = cpu.e
        case 0x44: // LD B, H
            cpu.b = cpu.h
        case 0x45: // LD B, L
            cpu.b = cpu.l
        case 0x46: // LD B, (HL)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.b = cpu.memory[memory_address]
        case 0x47: // LD B, A
            cpu.b = cpu.a 
        case 0x48: // LD C, B
            cpu.c = cpu.b
        case 0x49: // LD C, C
        case 0x4A: // LD C, D
            cpu.c = cpu.d
        case 0x4B: // LD C, E
            cpu.c = cpu.e
        case 0x4C: // LD C, H
            cpu.c = cpu.h
        case 0x4D: // LD C, L
            cpu.c = cpu.l
        case 0x4E: // LD C, (HL)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.c = cpu.memory[memory_address]
        case 0x4F: // LD C, A
            cpu.c = cpu.a

        case 0x50: // LD D, B
            cpu.d = cpu.b
        case 0x51: // LD D, C
            cpu.d = cpu.c
        case 0x52: // LD D, D
        case 0x53: // LD D, E
            cpu.d = cpu.e
        case 0x54: // LD D, H
            cpu.d = cpu.h
        case 0x55: // LD D, L
            cpu.d = cpu.l
        case 0x56: // LD D, (HL)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.d = cpu.memory[memory_address]
        case 0x57: // LD D, A
            cpu.d = cpu.a
        case 0x58: // LD E, B
            cpu.e = cpu.b
        case 0x59: // LD E, C
            cpu.e = cpu.c
        case 0x5A: // LD E, D
            cpu.e = cpu.d
        case 0x5B: // LD E, E
        case 0x5C: // LD E, H
            cpu.e = cpu.h
        case 0x5D: // LD E, L
            cpu.e = cpu.l
        case 0x5E: // LD E, (HL)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.e = cpu.memory[memory_address]
        case 0x5F: // LD E, A
            cpu.e = cpu.a

        case 0x60: // LD H, B
            cpu.h = cpu.b
        case 0x61: // LD H, C
            cpu.h = cpu.c
        case 0x62: // LD H, D
            cpu.h = cpu.d
        case 0x63: // LD H, E
            cpu.h = cpu.e
        case 0x64: // LD H, H
        case 0x65: // LD H, L
            cpu.h = cpu.l
        case 0x66: // LD H, (HL)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.h = cpu.memory[memory_address]
        case 0x67: // LD H, A
            cpu.h = cpu.a
        case 0x68: // LD L, B
            cpu.l = cpu.b
        case 0x69: // LD L, C
            cpu.l = cpu.c
        case 0x6A: // LD L, D
            cpu.l = cpu.d
        case 0x6B: // LD L, E
            cpu.l = cpu.e
        case 0x6C: // LD L, H
            cpu.l = cpu.h
        case 0x6D: // LD L, L
        case 0x6E: // LD L, (HL)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.l = cpu.memory[memory_address]
        case 0x6F: // LD L, A
            cpu.l = cpu.a

        case 0x70: // LD (HL), B
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.memory[memory_address] = cpu.b
        case 0x71: // LD (HL), C
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.memory[memory_address] = cpu.c
        case 0x72: // LD (HL), D
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.memory[memory_address] = cpu.d
        case 0x73: // LD (HL), E
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.memory[memory_address] = cpu.e
        case 0x74: // LD (HL), H
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.memory[memory_address] = cpu.h
        case 0x75: // LD (HL), L
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.memory[memory_address] = cpu.l
        case 0x77: // LD (HL), A
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.memory[memory_address] = cpu.a
        case 0x78: // LD A, B
            cpu.a = cpu.b
        case 0x79: // LD A, C
            cpu.a = cpu.c
        case 0x7A: // LD A, D
            cpu.a = cpu.d
        case 0x7B: // LD A, E
            cpu.a = cpu.e
        case 0x7C: // LD A, H
            cpu.a = cpu.h
        case 0x7D: // LD A, L
            cpu.a = cpu.l
        case 0x7E: // LD A, (HL)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.a = cpu.memory[memory_address]
        case 0x7F: // LD A, A

        case 0x80: // ADD A, B
            cpu.a = cpu.Add8(cpu.a, cpu.b, false)
        case 0x81: // ADD A, C
            cpu.a = cpu.Add8(cpu.a, cpu.c, false)
        case 0x82: // ADD A, D
            cpu.a = cpu.Add8(cpu.a, cpu.d, false)
        case 0x83: // ADD A, E
            cpu.a = cpu.Add8(cpu.a, cpu.e, false)
        case 0x84: // ADD A, H
            cpu.a = cpu.Add8(cpu.a, cpu.h, false)
        case 0x85: // ADD A, L
            cpu.a = cpu.Add8(cpu.a, cpu.l, false)
        case 0x86: // ADD A, (HL)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.a = cpu.Add8(cpu.a, cpu.memory[memory_address], false)
        case 0x87: // ADD A, A
            cpu.a = cpu.Add8(cpu.a, cpu.a, false)
        case 0x88: // ADC A, B
            cpu.a = cpu.Add8(cpu.a, cpu.b, true)
        case 0x89: // ADC A, C
            cpu.a = cpu.Add8(cpu.a, cpu.c, true)
        case 0x8A: // ADC A, D
            cpu.a = cpu.Add8(cpu.a, cpu.d, true)
        case 0x8B: // ADC A, E
            cpu.a = cpu.Add8(cpu.a, cpu.e, true)
        case 0x8C: // ADC A, H
            cpu.a = cpu.Add8(cpu.a, cpu.h, true)
        case 0x8D: // ADC A, L
            cpu.a = cpu.Add8(cpu.a, cpu.l, true)
        case 0x8E: // ADC A, (HL)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.a = cpu.Add8(cpu.a, cpu.memory[memory_address], true)
        case 0x8F: // ADC A, A
            cpu.a = cpu.Add8(cpu.a, cpu.a, true)

        case 0x90: // SUB A, B
            cpu.a = cpu.Sub8(cpu.a, cpu.b, false)
        case 0x91: // SUB A, C
            cpu.a = cpu.Sub8(cpu.a, cpu.c, false)
        case 0x92: // SUB A, D
            cpu.a = cpu.Sub8(cpu.a, cpu.d, false)
        case 0x93: // SUB A, E
            cpu.a = cpu.Sub8(cpu.a, cpu.e, false)
        case 0x94: // SUB A, H
            cpu.a = cpu.Sub8(cpu.a, cpu.h, false)
        case 0x95: // SUB A, L
            cpu.a = cpu.Sub8(cpu.a, cpu.l, false)
        case 0x96: // SUB A, (HL)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.a = cpu.Sub8(cpu.a, cpu.memory[memory_address], false)
        case 0x97: // SUB A, A
            cpu.a = cpu.Sub8(cpu.a, cpu.a, false)
        case 0x98: // SBC A, B
            cpu.a = cpu.Sub8(cpu.a, cpu.b, true)
        case 0x99: // SBC A, C
            cpu.a = cpu.Sub8(cpu.a, cpu.c, true)
        case 0x9A: // SBC A, D
            cpu.a = cpu.Sub8(cpu.a, cpu.d, true)
        case 0x9B: // SBC A, E
            cpu.a = cpu.Sub8(cpu.a, cpu.e, true)
        case 0x9C: // SBC A, H
            cpu.a = cpu.Sub8(cpu.a, cpu.h, true)
        case 0x9D: // SBC A, L
            cpu.a = cpu.Sub8(cpu.a, cpu.l, true)
        case 0x9E: // SBC A, (HL)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.a = cpu.Sub8(cpu.a, cpu.memory[memory_address], true)
        case 0x9F: // SBC A, A
            cpu.a = cpu.Sub8(cpu.a, cpu.a, true)

        case 0xA0: // AND A, B
            cpu.a = cpu.And(cpu.b)
        case 0xA1: // AND A, C
            cpu.a = cpu.And(cpu.c)
        case 0xA2: // AND A, D
            cpu.a = cpu.And(cpu.d)
        case 0xA3: // AND A, E
            cpu.a = cpu.And(cpu.e)
        case 0xA4: // AND A, H
            cpu.a = cpu.And(cpu.h)
        case 0xA5: // AND A, L
            cpu.a = cpu.And(cpu.l)
        case 0xA6: // AND A, (HL)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.a = cpu.And(cpu.memory[memory_address])
        case 0xA7: // AND A, A
            cpu.a = cpu.And(cpu.a)
        case 0xA8: // XOR A, B
            cpu.a = cpu.Xor(cpu.b)
        case 0xA9: // XOR A, C
            cpu.a = cpu.Xor(cpu.c)
        case 0xAA: // XOR A, D
            cpu.a = cpu.Xor(cpu.d)
        case 0xAB: // XOR A, E
            cpu.a = cpu.Xor(cpu.e)
        case 0xAC: // XOR A, H
            cpu.a = cpu.Xor(cpu.h)
        case 0xAD: // XOR A, L
            cpu.a = cpu.Xor(cpu.l)
        case 0xAE: // XOR A, (HL)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.a = cpu.Xor(cpu.memory[memory_address])
        case 0xAF: // XOR A, A
            cpu.a = cpu.Xor(cpu.a)
            
        case 0xB0: // OR A, B
            cpu.a = cpu.Or(cpu.b)
        case 0xB1: // OR A, C
            cpu.a = cpu.Or(cpu.c)
        case 0xB2: // OR A, D
            cpu.a = cpu.Or(cpu.d)
        case 0xB3: // OR A, E
            cpu.a = cpu.Or(cpu.e)
        case 0xB4: // OR A, H
            cpu.a = cpu.Or(cpu.h)
        case 0xB5: // OR A, L
            cpu.a = cpu.Or(cpu.l)
        case 0xB6: // OR A, (HL)
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.a = cpu.Or(cpu.memory[memory_address])
        case 0xB7: // OR A, A
            cpu.a = cpu.Or(cpu.a)
        case 0xB8: // CP A, B
            cpu.Sub8(cpu.a, cpu.b, false)
        case 0xB9: // CP A, C
            cpu.Sub8(cpu.a, cpu.c, false)
        case 0xBA: // CP A, D
            cpu.Sub8(cpu.a, cpu.d, false)
        case 0xBB: // CP A, E
            cpu.Sub8(cpu.a, cpu.e, false)
        case 0xBC: // CP A, H
            cpu.Sub8(cpu.a, cpu.h, false)
        case 0xBD: // CP A, L
            cpu.Sub8(cpu.a, cpu.l, false)
        case 0xBE:
            memory_address := ConcatBytes(cpu.h, cpu.l)
            cpu.Sub8(cpu.a, cpu.memory[memory_address], false)
        case 0xBF: // CP A, A
            cpu.Sub8(cpu.a, cpu.a, false)

        case 0xC0: // RET NZ
            if !cpu.GetZ() {
                lsb, msb := cpu.Pop()
                cpu.pc = ConcatBytes(msb, lsb)
            }
        case 0xC1: // POP BC
            lsb, msb := cpu.Pop()
            cpu.b, cpu.c = msb, lsb
        case 0xC2: // JP NZ, u16
            lsb, msb := cpu.ReadMemory(), cpu.ReadMemory()
            immediate := ConcatBytes(msb, lsb)
            if !cpu.GetZ() {
                cpu.sp = immediate
            }
        case 0xC3: // JP u16
            lsb, msb := cpu.ReadMemory(), cpu.ReadMemory()
            memory_address := ConcatBytes(msb, lsb)
            cpu.sp = memory_address
        case 0xC4: // CALL NZ, u16
            lsb, msb := cpu.ReadMemory(), cpu.ReadMemory()
            memory_address := ConcatBytes(msb, lsb)
            if !cpu.GetZ() {
                cpu.Push(MSB(cpu.sp))
                cpu.Push(LSB(cpu.sp))
                cpu.sp = memory_address
            }
        case 0xC5: // PUSH BC
            cpu.Push(cpu.b)
            cpu.Push(cpu.c)
        case 0xC6: // ADD A, u8
            cpu.a = cpu.Add8(cpu.a, cpu.ReadMemory(), false)
        case 0xC7: // RET 00h
            cpu.Push(MSB(cpu.sp))
            cpu.Push(LSB(cpu.sp))
            cpu.sp = 0x0000
        case 0xC8: // RET Z
            if cpu.GetZ() {
                lsb, msb := cpu.Pop()
                cpu.pc = ConcatBytes(msb, lsb)
            }
        case 0xC9: // RET
            lsb, msb := cpu.Pop()
            cpu.pc = ConcatBytes(msb, lsb)
        case 0xCA: // JP Z, u16
            lsb, msb := cpu.ReadMemory(), cpu.ReadMemory()
            immediate := ConcatBytes(msb, lsb)
            if cpu.GetZ() {
                cpu.sp = immediate
            }
        case 0xCC: // CALL Z, u16
            lsb, msb := cpu.ReadMemory(), cpu.ReadMemory()
            memory_address := ConcatBytes(msb, lsb)
            if cpu.GetZ() {
                cpu.Push(MSB(cpu.sp))
                cpu.Push(LSB(cpu.sp))
                cpu.sp = memory_address
            }
        case 0xCD: // CALL u16
            lsb, msb := cpu.ReadMemory(), cpu.ReadMemory()
            memory_address := ConcatBytes(msb, lsb)
            cpu.Push(MSB(cpu.sp))
            cpu.Push(LSB(cpu.sp))
            cpu.sp = memory_address
        case 0xCE: // ADC A, u8
            cpu.a = cpu.Add8(cpu.a, cpu.ReadMemory(), true)
        case 0xCF: // RST 08h
            cpu.Push(MSB(cpu.sp))
            cpu.Push(LSB(cpu.sp))
            cpu.sp = 0x0008

        case 0xD0: // RET NC
            if !cpu.GetC() {
                lsb, msb := cpu.Pop()
                cpu.pc = ConcatBytes(msb, lsb)
            }
        case 0xD1: // POP DE
            lsb, msb := cpu.Pop()
            cpu.d, cpu.e = msb, lsb
        case 0xD2: // JP NC, u16
            lsb, msb := cpu.ReadMemory(), cpu.ReadMemory()
            immediate := ConcatBytes(msb, lsb)
            if !cpu.GetC() {
                cpu.sp = immediate
            }
        case 0xD4: // CALL NC, u16
            lsb, msb := cpu.ReadMemory(), cpu.ReadMemory()
            memory_address := ConcatBytes(msb, lsb)
            if !cpu.GetC() {
                cpu.Push(MSB(cpu.sp))
                cpu.Push(LSB(cpu.sp))
                cpu.sp = memory_address
            }
        case 0xD5: // PUSH DE
            cpu.Push(cpu.d)
            cpu.Push(cpu.e)
        case 0xD6: // SUB A, u8
            cpu.a = cpu.Sub8(cpu.a, cpu.ReadMemory(), false)
        case 0xD7: // RST 10h
            cpu.Push(MSB(cpu.sp))
            cpu.Push(LSB(cpu.sp))
            cpu.sp = 0x0010
        case 0xD8: // RET C
            if cpu.GetC() {
                lsb, msb := cpu.Pop()
                cpu.pc = ConcatBytes(msb, lsb)
            }
        case 0xD9: // RETI
            lsb, msb := cpu.Pop()
            cpu.pc = ConcatBytes(msb, lsb)
            cpu.interrupt = true
        case 0xDA: // JP C, u16
            lsb, msb := cpu.ReadMemory(), cpu.ReadMemory()
            immediate := ConcatBytes(msb, lsb)
            if cpu.GetC() {
                cpu.sp = immediate
            }
        case 0xDC: // CALL C, u16
            lsb, msb := cpu.ReadMemory(), cpu.ReadMemory()
            memory_address := ConcatBytes(msb, lsb)
            if cpu.GetC() {
                cpu.Push(MSB(cpu.sp))
                cpu.Push(LSB(cpu.sp))
                cpu.sp = memory_address
            }
        case 0xDE: // SBC A, u8
            cpu.a = cpu.Sub8(cpu.a, cpu.ReadMemory(), true)
        case 0xDF: // RST 18h
            cpu.Push(MSB(cpu.sp))
            cpu.Push(LSB(cpu.sp))
            cpu.sp = 0x0018

        case 0xE0: // LD (0xFF00 + immediate), A
            memory_address := ConcatBytes(0xFF, cpu.ReadMemory())
            cpu.memory[memory_address] = cpu.a
        case 0xE1: // POP HL
            lsb, msb := cpu.Pop()
            cpu.h, cpu.l = msb, lsb
        case 0xE2: // LD (0xFF00 + C), A
            memory_address := ConcatBytes(0xFF, cpu.c)
            cpu.memory[memory_address] = cpu.a 
        case 0xE5: // PUSH HL
            cpu.Push(cpu.h)
            cpu.Push(cpu.l)
        case 0xE6: // AND A, u8
            cpu.a = cpu.And(cpu.ReadMemory())
        case 0xE7: // RET 20h
            cpu.Push(MSB(cpu.sp))
            cpu.Push(LSB(cpu.sp))
            cpu.sp = 0x0020
        case 0xE8: // ADD SP, i8
            immediate := int8(cpu.ReadMemory())
            cpu.sp = cpu.Add16Signed(cpu.sp, immediate)
        case 0xE9: // JP HL
            cpu.sp = ConcatBytes(cpu.h, cpu.l)
        case 0xEA: // LD (u16), A
            lsb, msb := cpu.ReadMemory(), cpu.ReadMemory()
            memory_address := ConcatBytes(msb, lsb)
            cpu.memory[memory_address] = cpu.a
        case 0xEE: // XOR A, u8
            cpu.a = cpu.Xor(cpu.ReadMemory())
        case 0xEF: // RST 28h
            cpu.Push(MSB(cpu.sp))
            cpu.Push(LSB(cpu.sp))
            cpu.sp = 0x0028

        case 0xF0: // LD A, (0xFF00 + immediate)
            memory_address := ConcatBytes(0xFF, cpu.ReadMemory())
            cpu.a = cpu.memory[memory_address]
        case 0xF1: // POP AF
            lsb, msb := cpu.Pop()
            cpu.a, cpu.f = msb, lsb
        case 0xF2: // LD A, (0xFF00 + C)
            memory_address := ConcatBytes(0xFF, cpu.c)
            cpu.a = cpu.memory[memory_address]
        case 0xF5: // PUSH AF
            cpu.Push(cpu.a)
            cpu.Push(cpu.f)
        case 0xF6: // OR A, u8
            cpu.a = cpu.Or(cpu.ReadMemory())
        case 0xF7: // RST 30h
            cpu.Push(MSB(cpu.sp))
            cpu.Push(LSB(cpu.sp))
            cpu.sp = 0x0030
        case 0xF8: // LD HL, SP + i8
            immediate := int8(cpu.ReadMemory())
            sum := cpu.Add16Signed(cpu.sp, immediate)
            cpu.h, cpu.l = MSB(sum), LSB(sum)
        case 0xF9: // LD SP, HL
            cpu.sp = ConcatBytes(cpu.h, cpu.l)
        case 0xFA: // LD A, (u16)
            lsb, msb := cpu.ReadMemory(), cpu.ReadMemory()
            memory_address := ConcatBytes(msb, lsb)
            cpu.a = cpu.memory[memory_address] 
        case 0xFE: // CP A, u8
            cpu.Sub8(cpu.a, cpu.ReadMemory(), false)
        case 0xFF: // RST 38h
            cpu.Push(MSB(cpu.sp))
            cpu.Push(LSB(cpu.sp))
            cpu.sp = 0x0038
    }
}
