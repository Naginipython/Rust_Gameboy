const WIDTH: usize = 256;
const HEIGHT: usize = 256;

struct PPU { // 256*256
    mode: u8,
    buffer: [u8; WIDTH*HEIGHT]
}
impl Default for PPU {
    fn default() -> Self {
        Self {
            mode: 0,
            buffer: [0; WIDTH*HEIGHT],
        }
    }
}

impl PPU {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }
}