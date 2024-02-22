pub struct Memory {
    pub ram: [u8; 0xFFFF],
}
impl Memory {
    pub fn new() -> Self {
        Self {
            ram: [0; 0xFFFF],
        }
    }
    pub fn write_byte(&mut self, address: u16, value: u8) {
        self.ram[address as usize] = value;
    }
    pub fn read_byte(&self, address: u16) -> u8 {
        self.ram[address as usize]
    }
}