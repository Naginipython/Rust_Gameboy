use std::fmt::{self, Display, Formatter, Write};

#[derive(Default)]
pub struct Registers {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub flags: u8,
    pub h: u8,
    pub l: u8,
    pub sp: u16,
}

impl Registers {
    pub fn get_af(&self) -> u16 {
        (self.a as u16) << 8 | u8::from(self.flags) as u16
    }
    pub fn set_af(&mut self, value: u16) {
        self.a = ((value & 0xFF00) >> 8) as u8;
        self.flags = (value & 0xFF) as u8;
    }
    pub fn get_bc(&self) -> u16 {
        (self.b as u16) << 8 | self.c as u16
    }
    pub fn set_bc(&mut self, value: u16) {
        self.b = ((value & 0xFF00) >> 8) as u8;
        self.c = (value & 0xFF) as u8;
    }
    pub fn get_de(&self) -> u16 {
        (self.d as u16) << 8 | self.c as u16
    }
    pub fn set_de(&mut self, value: u16) {
        self.d = ((value & 0xFF00) >> 8) as u8;
        self.e = (value & 0xFF) as u8;
    }
    pub fn get_hl(&self) -> u16 {
        (self.h as u16) << 8 | self.l as u16
    }
    pub fn set_hl(&mut self, value: u16) {
        self.h = ((value & 0xFF00) >> 8) as u8;
        self.l = (value & 0xFF) as u8;
    }
    // Flag Methods
    pub fn set_flags(&mut self, is_zero: bool, is_subtract: bool, is_half_carry: bool, is_carry: bool) {
        self.flags = 0;
        if is_zero { self.flags |= 0b1000_0000; }
        if is_subtract { self.flags |= 0b0100_0000; }
        if is_half_carry { self.flags |= 0b0010_0000; }
        if is_carry { self.flags |= 0b0001_0000; }
        println!("Flag result: {:08b}", self.flags)
    }
    pub fn check_flag_z(&self) -> bool {
        (self.flags >> 7) == 1
    }
    pub fn check_flag_c(&self) -> bool {
        (self.flags & 0b0001_0000) >> 4 == 1
    }
}

impl Display for Registers {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mut s = format!("a: {:#02X}, ", self.a);
        s.write_str(&format!("b: {:#04X}, ", self.b)).unwrap();
        s.write_str(&format!("c: {:#04X}, ", self.c)).unwrap();
        s.write_str(&format!("d: {:#04X}, ", self.d)).unwrap();
        s.write_str(&format!("e: {:#04X}, ", self.e)).unwrap();
        s.write_str(&format!("h: {:#04X}, ", self.h)).unwrap();
        s.write_str(&format!("l: {:#04X}, ", self.l)).unwrap();
        s.write_str(&format!("f: {:08b}, ", self.flags)).unwrap();
        s.write_str(&format!("sp: {:#04X}", self.sp)).unwrap();
        write!(f, "{}", s)
    }
}