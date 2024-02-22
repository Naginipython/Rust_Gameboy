use super::CPU;

impl CPU {
    // Math 8-bit
    pub fn xor(&mut self, value: u8) -> u8 {
        let new_value = self.registers.a ^ value;
        // Flag: zero: dependent, negative: unset, half-carry: unset, carry: unset
        self.registers.set_flags(new_value == 0, false, false, false);
        new_value
    }
    pub fn cp(&mut self, value: u8) {
        let (new_value, is_carry) = self.registers.a.overflowing_sub(value);
        // Flags: zero: set if result is 0, negative: set, half-carry: set if borrow from bit 4, carry: set if borrow
        let is_half_carry = (self.registers.a ^ value ^ new_value) & 0b0001_0000 != 0;
        self.registers.set_flags(new_value == 0, true, is_half_carry, is_carry);
    }
    pub fn inc(&mut self, value: u8) -> u8 {
        let new_value = value.wrapping_add(1);
        // Flag: zero: dependent, negative: unset, half-carry: dependent, carry: unmodified
        // (A ^ B ^ result) & 0x08 != 0
        let is_half_carry = (value ^ 1 ^ new_value) & 0b0001_0000 != 0;
        self.registers.set_flags(new_value == 0, false, is_half_carry, self.registers.check_flag_c());
        new_value
    }
    pub fn dec(&mut self, value: u8) -> u8 {
        let new_value = value.wrapping_sub(1);
        // Flag: zero: dependent, negative: set, half-carry: dependent, carry: unmodified
        // (A ^ B ^ result) & 0x10 != 0
        let is_half_carry = (value ^ 1 ^ new_value) & 0b0001_0000 != 0;
        self.registers.set_flags(new_value == 0, true, is_half_carry, self.registers.check_flag_c());
        new_value
    }

    // Math 16-bit
    
    // etc
    pub fn rl(&mut self, value: u8) -> u8 {
        let to_rotate = ((self.registers.flags as u16) << 8) | value as u16;
        let rotate = to_rotate.rotate_left(1);
        self.registers.set_flags(
            rotate == 0, 
            false, 
            false, 
            (rotate >> 12) & 0b0001  == 1 // the 8 bytes, then half the flag bytes (4)
        );
        rotate as u8
    }
    pub fn rla(&mut self) {
        let to_rotate = ((self.registers.flags as u16) << 8) | self.registers.a as u16;
        let rotate = to_rotate.rotate_left(1);
        self.registers.set_flags(
            false, 
            false, 
            false, 
            (rotate >> 12) & 0b0001  == 1 // the 8 bytes, then half the flag bytes (4)
        );
        self.registers.a = rotate as u8;
    }
    pub fn bit(&mut self, value: u8, mask: u8) {
        let bit = value & mask;
        let carry_check = (self.registers.flags & 0b0001_0000)>>4;
        // gets the original value of z, to be set if bit is 0
        // possible error? do I need to reset if bit of register is 1?
        self.registers.set_flags(bit == 0, false, true, carry_check == 1)
    }
    pub fn swap(&mut self, value: u8) -> u8 {
        // swap upper & lower nibles of n
        let lower = value & 0xF;
        let upper = value >> 4;
        let new_value = (lower << 4) | upper;
        self.registers.set_flags(new_value == 0, false, false, false);
        new_value
    }
}