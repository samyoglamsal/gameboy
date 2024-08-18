package cpu

/* HELPERS */
func LSB(value uint16) uint8 {
    return uint8(value & 0xFF)
}

func MSB(value uint16) uint8 {
    return uint8(value >> 8)
}

func ConcatBytes(high uint8, low uint8) uint16 {
    return uint16(high) << 8 | uint16(low)
}
