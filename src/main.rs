use rust_gameboy2::Gameboy;

fn main() {
    // read ROM
    let mut gb = Gameboy::new();
    gb.load_from_rom("DMG_ROM.bin").expect("Error w/rom");
    loop {
        gb.run_next();
    }
}
