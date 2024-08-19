/* Samyog Lamsal 2024 */

package cpu

/* Struct to represent a register. For example the struct for AF would have high = A and low = F */
type CPU struct {
    a, b, c, d, e, f, h, l uint8
    sp, pc uint16
    memory [0x10000]uint8
    interrupt bool
}

func NewCPU() *CPU {
    cpu := &CPU{
        sp: 0xFFFF,
        pc: 0x0000,
        interrupt: false,
    }

    return cpu
}

// Used for reading immediate values. Increases PC
func (cpu *CPU) ReadMemory() uint8 {
    next_byte := cpu.memory[cpu.pc]
    cpu.pc++

    return next_byte
}

