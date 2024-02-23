use std::{error::Error, fs::File, io::Read};

use cpu::CPU;
use memory::Memory;

pub mod memory;
pub mod cpu;
pub mod ppu;

#[derive(Default)]
pub struct Gameboy {
    memory: Memory,
    cpu: CPU,
}
impl Gameboy {
    pub fn new() -> Self {
        Self {
            memory: Memory::new(),
            cpu: CPU::new(),
        }
    }
    pub fn load_from_rom(&mut self, path: &str) -> Result<(), Box<dyn Error>> {
        let mut file = File::open(path)?;
        let mut rom = Vec::<u8>::new();
        file.read_to_end(&mut rom)?;

        for (i, byte) in rom.iter().enumerate() {
            // println!("{byte:#04X}");
            self.memory.write_byte(i as u16, *byte)
        }
        
        Ok(())
    }
    pub fn run_next(&mut self) {
        self.cpu.step(&mut self.memory);
    }
}