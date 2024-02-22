#[derive(Debug)]
pub enum Instruction {
    // Loads
    Ld(Target, Source),
    AddrLd(AddrTarget, AddrSource),
    LdFF00(FF00Target),
    Ld16(PairTarget),
    Push(PairTarget),
    Pop(PairTarget),
    // ALU
    Xor(Target),
    Cp(Target),
    Inc(Target),
    Dec(Target),
    Inc16(PairTarget),
    Dec16(PairTarget),
    // Jumps & Subroutines
    Call,
    CallCond(FlagTarget),
    // JP,
    // JPCond(FlagTarget),
    // JR,
    JRCond(FlagTarget),
    Ret,
    // Rotates
    RLA,
    // prefixed
    Rl(Target),
    Bit(Target, u8),
}
#[derive(Debug)]
pub enum Target {
    A, B, C, D, E, H, L, HL, N
}
#[derive(Debug)]
pub enum Source {
    A, B, C, D, E, H, L, HL, N
}
#[derive(Debug)]
pub enum PairTarget {
    AF, BC, DE, HL, SP
}
#[derive(Debug)]
pub enum FF00Target {
    U8A, AU8, CA, AC
}
#[derive(Debug)]
pub enum AddrTarget {
    BC, DE, HLPlus, HLMinus, A
}
#[derive(Debug)]
pub enum AddrSource {
    BC, DE, HLPlus, HLMinus, A
}
#[derive(Debug)]
pub enum FlagTarget {
    NZ, Z, NC, C
}

impl Instruction {
    pub fn decode(byte: u8, prefixed: bool) -> Option<Instruction> {
        if prefixed {
            Instruction::decode_prefixed(byte)
        } else {
            Instruction::decode_not_prefixed(byte)
        }
    }
    fn decode_not_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            // ---------- 8-Bit Loads ----------
            // LD A, r8||(HL). Puts register value into A
            0x7F => Some(Instruction::Ld(Target::A, Source::A)),
            0x78 => Some(Instruction::Ld(Target::A, Source::B)),
            0x79 => Some(Instruction::Ld(Target::A, Source::C)),
            0x7A => Some(Instruction::Ld(Target::A, Source::D)),
            0x7B => Some(Instruction::Ld(Target::A, Source::E)),
            0x7C => Some(Instruction::Ld(Target::A, Source::H)),
            0x7D => Some(Instruction::Ld(Target::A, Source::L)),
            0x7E => Some(Instruction::Ld(Target::A, Source::HL)),
            // LD B, r8||(HL). Puts register value into B
            0x47 => Some(Instruction::Ld(Target::B, Source::A)),
            0x40 => Some(Instruction::Ld(Target::B, Source::B)),
            0x41 => Some(Instruction::Ld(Target::B, Source::C)),
            0x42 => Some(Instruction::Ld(Target::B, Source::D)),
            0x43 => Some(Instruction::Ld(Target::B, Source::E)),
            0x44 => Some(Instruction::Ld(Target::B, Source::H)),
            0x45 => Some(Instruction::Ld(Target::B, Source::L)),
            0x46 => Some(Instruction::Ld(Target::B, Source::HL)),
            // LD C, r8||(HL). Puts register value into C
            0x4F => Some(Instruction::Ld(Target::C, Source::A)),
            0x48 => Some(Instruction::Ld(Target::C, Source::B)),
            0x49 => Some(Instruction::Ld(Target::C, Source::C)),
            0x4A => Some(Instruction::Ld(Target::C, Source::D)),
            0x4B => Some(Instruction::Ld(Target::C, Source::E)),
            0x4C => Some(Instruction::Ld(Target::C, Source::H)),
            0x4D => Some(Instruction::Ld(Target::C, Source::L)),
            0x4E => Some(Instruction::Ld(Target::C, Source::HL)),
            // LD D, r8||(HL). Puts register value into D
            0x57 => Some(Instruction::Ld(Target::D, Source::A)),
            0x50 => Some(Instruction::Ld(Target::D, Source::B)),
            0x51 => Some(Instruction::Ld(Target::D, Source::C)),
            0x52 => Some(Instruction::Ld(Target::D, Source::D)),
            0x53 => Some(Instruction::Ld(Target::D, Source::E)),
            0x54 => Some(Instruction::Ld(Target::D, Source::H)),
            0x55 => Some(Instruction::Ld(Target::D, Source::L)),
            0x56 => Some(Instruction::Ld(Target::D, Source::HL)),
            // LD E, r8||(HL). Puts register value into E
            0x5F => Some(Instruction::Ld(Target::E, Source::A)),
            0x58 => Some(Instruction::Ld(Target::E, Source::B)),
            0x59 => Some(Instruction::Ld(Target::E, Source::C)),
            0x5A => Some(Instruction::Ld(Target::E, Source::D)),
            0x5B => Some(Instruction::Ld(Target::E, Source::E)),
            0x5C => Some(Instruction::Ld(Target::E, Source::H)),
            0x5D => Some(Instruction::Ld(Target::E, Source::L)),
            0x5E => Some(Instruction::Ld(Target::E, Source::HL)),
            // LD H, r8||(HL). Puts register value into H
            0x67 => Some(Instruction::Ld(Target::H, Source::A)),
            0x60 => Some(Instruction::Ld(Target::H, Source::B)),
            0x61 => Some(Instruction::Ld(Target::H, Source::C)),
            0x62 => Some(Instruction::Ld(Target::H, Source::D)),
            0x63 => Some(Instruction::Ld(Target::H, Source::E)),
            0x64 => Some(Instruction::Ld(Target::H, Source::H)),
            0x65 => Some(Instruction::Ld(Target::H, Source::L)),
            0x66 => Some(Instruction::Ld(Target::H, Source::HL)),
            // LD L, r8||(HL). Puts register value into L
            0x6F => Some(Instruction::Ld(Target::L, Source::A)),
            0x68 => Some(Instruction::Ld(Target::L, Source::B)),
            0x69 => Some(Instruction::Ld(Target::L, Source::C)),
            0x6A => Some(Instruction::Ld(Target::L, Source::D)),
            0x6B => Some(Instruction::Ld(Target::L, Source::E)),
            0x6C => Some(Instruction::Ld(Target::L, Source::H)),
            0x6D => Some(Instruction::Ld(Target::L, Source::L)),
            0x6E => Some(Instruction::Ld(Target::L, Source::HL)),
            // LD (HL), r8||(HL). Puts register value into the memory HL points to
            0x77 => Some(Instruction::Ld(Target::HL, Source::A)),
            0x70 => Some(Instruction::Ld(Target::HL, Source::B)),
            0x71 => Some(Instruction::Ld(Target::HL, Source::C)),
            0x72 => Some(Instruction::Ld(Target::HL, Source::D)),
            0x73 => Some(Instruction::Ld(Target::HL, Source::E)),
            0x74 => Some(Instruction::Ld(Target::HL, Source::H)),
            0x75 => Some(Instruction::Ld(Target::HL, Source::L)),
            // LD r, n. Puts n into r
            0x3E => Some(Instruction::Ld(Target::A, Source::N)),
            0x06 => Some(Instruction::Ld(Target::B, Source::N)),
            0x0E => Some(Instruction::Ld(Target::C, Source::N)),
            0x16 => Some(Instruction::Ld(Target::D, Source::N)),
            0x1E => Some(Instruction::Ld(Target::E, Source::N)),
            0x26 => Some(Instruction::Ld(Target::H, Source::N)),
            0x2E => Some(Instruction::Ld(Target::L, Source::N)),
            0x36 => Some(Instruction::Ld(Target::HL, Source::N)),
            // Special LD for memory space + C/n, to/from A
            0xE0 => Some(Instruction::LdFF00(FF00Target::U8A)),
            0xF0 => Some(Instruction::LdFF00(FF00Target::AU8)),
            0xE2 => Some(Instruction::LdFF00(FF00Target::CA)),
            0xF2 => Some(Instruction::LdFF00(FF00Target::AC)),
            // LD addresses
            0x02 => Some(Instruction::AddrLd(AddrTarget::BC, AddrSource::A)),
            0x12 => Some(Instruction::AddrLd(AddrTarget::DE, AddrSource::A)),
            0x0A => Some(Instruction::AddrLd(AddrTarget::A, AddrSource::BC)),
            0x1A => Some(Instruction::AddrLd(AddrTarget::A, AddrSource::DE)),
            // LD (HL+), A. Puts A into memory address HL points to, increment HL
            0x22 => Some(Instruction::AddrLd(AddrTarget::HLPlus, AddrSource::A)),
            // LD (HL-), A. Puts A into memory address HL points to, decrement HL
            0x32 => Some(Instruction::AddrLd(AddrTarget::HLMinus, AddrSource::A)),
            // LD A, (HL+). Puts memory address HL points to into A, increment HL
            0x2A => Some(Instruction::AddrLd(AddrTarget::A, AddrSource::HLPlus)),
            // LD A, (HL-). Puts memory address HL points to into A, decrement HL
            0x3A => Some(Instruction::AddrLd(AddrTarget::A, AddrSource::HLMinus)),
            
            // ---------- 16-Bit Loads ----------
            // LD r16, u16. Puts value into n
            0x01 => Some(Instruction::Ld16(PairTarget::BC)),
            0x11 => Some(Instruction::Ld16(PairTarget::DE)),
            0x21 => Some(Instruction::Ld16(PairTarget::HL)),
            0x31 => Some(Instruction::Ld16(PairTarget::SP)),
            // LD SP, HL
            // LD HL, SP+n | LDHL SP, n
            // LD (nn), SP
            // PUSH r16
            0xF5 => Some(Instruction::Push(PairTarget::AF)),
            0xC5 => Some(Instruction::Push(PairTarget::BC)),
            0xD5 => Some(Instruction::Push(PairTarget::DE)),
            0xE5 => Some(Instruction::Push(PairTarget::HL)),
            // POP r16
            0xF1 => Some(Instruction::Pop(PairTarget::AF)),
            0xC1 => Some(Instruction::Pop(PairTarget::BC)),
            0xD1 => Some(Instruction::Pop(PairTarget::DE)),
            0xE1 => Some(Instruction::Pop(PairTarget::HL)),
            
            // ---------- 8-Bit ALU ----------
            // ADD A, n
            // ADC A, n
            // SUB n
            // SBC A, n
            // AND n
            // OR n
            // XOR A, r8||(HL)||n, A. XORs a target with register A, result stored in A
            0xAF => Some(Instruction::Xor(Target::A)),
            0xA8 => Some(Instruction::Xor(Target::B)),
            0xA9 => Some(Instruction::Xor(Target::C)),
            0xAA => Some(Instruction::Xor(Target::D)),
            0xAB => Some(Instruction::Xor(Target::E)),
            0xAC => Some(Instruction::Xor(Target::H)),
            0xAD => Some(Instruction::Xor(Target::L)),
            0xAE => Some(Instruction::Xor(Target::HL)),
            0xEE => Some(Instruction::Xor(Target::N)),
            // CP A, r8||(HL)||n
            0xBF => Some(Instruction::Cp(Target::A)),
            0xB8 => Some(Instruction::Cp(Target::B)),
            0xB9 => Some(Instruction::Cp(Target::C)),
            0xBA => Some(Instruction::Cp(Target::D)),
            0xBB => Some(Instruction::Cp(Target::E)),
            0xBC => Some(Instruction::Cp(Target::H)),
            0xBD => Some(Instruction::Cp(Target::L)),
            0xBE => Some(Instruction::Cp(Target::HL)),
            0xFE => Some(Instruction::Cp(Target::N)),
            // INC r8||(HL)
            0x3C => Some(Instruction::Inc(Target::A)),
            0x04 => Some(Instruction::Inc(Target::B)),
            0x0C => Some(Instruction::Inc(Target::C)),
            0x14 => Some(Instruction::Inc(Target::D)),
            0x1C => Some(Instruction::Inc(Target::E)),
            0x24 => Some(Instruction::Inc(Target::H)),
            0x2C => Some(Instruction::Inc(Target::L)),
            0x34 => Some(Instruction::Inc(Target::HL)),
            // DEC r8||(HL)
            0x3D => Some(Instruction::Dec(Target::A)),
            0x05 => Some(Instruction::Dec(Target::B)),
            0x0D => Some(Instruction::Dec(Target::C)),
            0x15 => Some(Instruction::Dec(Target::D)),
            0x1D => Some(Instruction::Dec(Target::E)),
            0x25 => Some(Instruction::Dec(Target::H)),
            0x2D => Some(Instruction::Dec(Target::L)),
            0x35 => Some(Instruction::Dec(Target::HL)),

            // ---------- 16-Bit ALU ----------
            // ADD HL, n
            // ADD SP, n
            // INC r16
            0x03 => Some(Instruction::Inc16(PairTarget::BC)),
            0x13 => Some(Instruction::Inc16(PairTarget::DE)),
            0x23 => Some(Instruction::Inc16(PairTarget::HL)),
            0x33 => Some(Instruction::Inc16(PairTarget::SP)),
            // DEC r16
            0x0B => Some(Instruction::Dec16(PairTarget::BC)),
            0x1B => Some(Instruction::Dec16(PairTarget::DE)),
            0x2B => Some(Instruction::Dec16(PairTarget::HL)),
            0x3B => Some(Instruction::Dec16(PairTarget::SP)),

            // ---------- Misc ----------
            // DAA
            // CPL
            // CCF
            // SCF
            // NOP
            // HALT
            // STOP
            // DI
            // EI

            // ---------- Rotations and Shifts 1 ----------
            // RLCA
            // RLA
            0x17 => Some(Instruction::RLA),
            // RRCA
            // RRA

            // ---------- Jumps ----------
            // JP nn
            // JP cc, nn
            // JP (HL)
            // JR n
            // JR cc, n. If the condition is true, then add n to current address and jump to it
            0x20 => Some(Instruction::JRCond(FlagTarget::NZ)),
            0x28 => Some(Instruction::JRCond(FlagTarget::Z)),
            0x30 => Some(Instruction::JRCond(FlagTarget::NC)),
            0x38 => Some(Instruction::JRCond(FlagTarget::C)),

            // ---------- Calls ----------
            // CALL nn
            0xCD => Some(Instruction::Call),
            // CALL cc, nn
            0xC4 => Some(Instruction::CallCond(FlagTarget::NZ)),
            0xCC => Some(Instruction::CallCond(FlagTarget::Z)),
            0xD4 => Some(Instruction::CallCond(FlagTarget::NC)),
            0xDC => Some(Instruction::CallCond(FlagTarget::C)),

            // ---------- Reset & Returns ----------
            // RST n
            // RET
            0xC9 => Some(Instruction::Ret),
            // RET cc
            // RETI
            _ => None,
        }
    }
    fn decode_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            // ---------- Rotations and Shifts 2 ----------
            // SWAP n
            // RLC n
            // RL r8
            0x17 => Some(Instruction::Rl(Target::A)),
            0x10 => Some(Instruction::Rl(Target::B)),
            0x11 => Some(Instruction::Rl(Target::C)),
            0x12 => Some(Instruction::Rl(Target::D)),
            0x13 => Some(Instruction::Rl(Target::E)),
            0x14 => Some(Instruction::Rl(Target::H)),
            0x15 => Some(Instruction::Rl(Target::L)),
            0x16 => Some(Instruction::Rl(Target::HL)),
            // RRC n
            // RR
            // SLA n
            // SRA n
            // SRL n

            // BIT b, r. Test bit b in register r
            // BIT 0, r
            0x47 => Some(Instruction::Bit(Target::A, 0b0000_0001)),
            0x40 => Some(Instruction::Bit(Target::B, 0b0000_0001)),
            0x41 => Some(Instruction::Bit(Target::C, 0b0000_0001)),
            0x42 => Some(Instruction::Bit(Target::D, 0b0000_0001)),
            0x43 => Some(Instruction::Bit(Target::E, 0b0000_0001)),
            0x44 => Some(Instruction::Bit(Target::H, 0b0000_0001)),
            0x45 => Some(Instruction::Bit(Target::H, 0b0000_0001)),
            0x46 => Some(Instruction::Bit(Target::HL, 0b0000_0001)),
            // BIT 1, r
            0x4F => Some(Instruction::Bit(Target::A, 0b0000_0010)),
            0x48 => Some(Instruction::Bit(Target::B, 0b0000_0010)),
            0x49 => Some(Instruction::Bit(Target::C, 0b0000_0010)),
            0x4A => Some(Instruction::Bit(Target::D, 0b0000_0010)),
            0x4B => Some(Instruction::Bit(Target::E, 0b0000_0010)),
            0x4C => Some(Instruction::Bit(Target::H, 0b0000_0010)),
            0x4D => Some(Instruction::Bit(Target::H, 0b0000_0010)),
            0x4E => Some(Instruction::Bit(Target::HL, 0b0000_0010)),
            // BIT 2, r
            0x57 => Some(Instruction::Bit(Target::A, 0b0000_0100)),
            0x50 => Some(Instruction::Bit(Target::B, 0b0000_0100)),
            0x51 => Some(Instruction::Bit(Target::C, 0b0000_0100)),
            0x52 => Some(Instruction::Bit(Target::D, 0b0000_0100)),
            0x53 => Some(Instruction::Bit(Target::E, 0b0000_0100)),
            0x54 => Some(Instruction::Bit(Target::H, 0b0000_0100)),
            0x55 => Some(Instruction::Bit(Target::H, 0b0000_0100)),
            0x56 => Some(Instruction::Bit(Target::HL, 0b0000_0100)),
            // BIT 3, r
            0x5F => Some(Instruction::Bit(Target::A, 0b0000_1000)),
            0x58 => Some(Instruction::Bit(Target::B, 0b0000_1000)),
            0x59 => Some(Instruction::Bit(Target::C, 0b0000_1000)),
            0x5A => Some(Instruction::Bit(Target::D, 0b0000_1000)),
            0x5B => Some(Instruction::Bit(Target::E, 0b0000_1000)),
            0x5C => Some(Instruction::Bit(Target::H, 0b0000_1000)),
            0x5D => Some(Instruction::Bit(Target::H, 0b0000_1000)),
            0x5E => Some(Instruction::Bit(Target::HL, 0b0000_1000)),
            // BIT 4, r
            0x67 => Some(Instruction::Bit(Target::A, 0b0001_0000)),
            0x60 => Some(Instruction::Bit(Target::B, 0b0001_0000)),
            0x61 => Some(Instruction::Bit(Target::C, 0b0001_0000)),
            0x62 => Some(Instruction::Bit(Target::D, 0b0001_0000)),
            0x63 => Some(Instruction::Bit(Target::E, 0b0001_0000)),
            0x64 => Some(Instruction::Bit(Target::H, 0b0001_0000)),
            0x65 => Some(Instruction::Bit(Target::H, 0b0001_0000)),
            0x66 => Some(Instruction::Bit(Target::HL, 0b0001_0000)),
            // BIT 5, r
            0x6F => Some(Instruction::Bit(Target::A, 0b0010_0000)),
            0x68 => Some(Instruction::Bit(Target::B, 0b0010_0000)),
            0x69 => Some(Instruction::Bit(Target::C, 0b0010_0000)),
            0x6A => Some(Instruction::Bit(Target::D, 0b0010_0000)),
            0x6B => Some(Instruction::Bit(Target::E, 0b0010_0000)),
            0x6C => Some(Instruction::Bit(Target::H, 0b0010_0000)),
            0x6D => Some(Instruction::Bit(Target::H, 0b0010_0000)),
            0x6E => Some(Instruction::Bit(Target::HL, 0b0010_0000)),
            // BIT 6, r
            0x77 => Some(Instruction::Bit(Target::A, 0b0100_0000)),
            0x70 => Some(Instruction::Bit(Target::B, 0b0100_0000)),
            0x71 => Some(Instruction::Bit(Target::C, 0b0100_0000)),
            0x72 => Some(Instruction::Bit(Target::D, 0b0100_0000)),
            0x73 => Some(Instruction::Bit(Target::E, 0b0100_0000)),
            0x74 => Some(Instruction::Bit(Target::H, 0b0100_0000)),
            0x75 => Some(Instruction::Bit(Target::H, 0b0100_0000)),
            0x76 => Some(Instruction::Bit(Target::HL, 0b0100_0000)),
            // BIT 7, r
            0x7F => Some(Instruction::Bit(Target::A, 0b1000_0000)),
            0x78 => Some(Instruction::Bit(Target::B, 0b1000_0000)),
            0x79 => Some(Instruction::Bit(Target::C, 0b1000_0000)),
            0x7A => Some(Instruction::Bit(Target::D, 0b1000_0000)),
            0x7B => Some(Instruction::Bit(Target::E, 0b1000_0000)),
            0x7C => Some(Instruction::Bit(Target::H, 0b1000_0000)),
            0x7D => Some(Instruction::Bit(Target::H, 0b1000_0000)),
            0x7E => Some(Instruction::Bit(Target::HL, 0b1000_0000)),

            // SET b, r
            // RES b, r

            _ => None,
        }
    }
}