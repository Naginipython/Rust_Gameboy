#[cfg(test)]
mod tests {
    use crate::{cpu::{
        instruction::*, 
        Target, 
        CPU
    }, memory::Memory};
    // todo
    // LdFF00(FF00Target),
    // LdPair(PairTarget),
    // Push(PairTarget),
    // Pop(PairTarget),
    // Inc(Target),
    // Dec(Target),
    // Inc16(PairTarget),
    // Dec16(PairTarget),
    // Call,
    // CallCond(FlagTarget),
    // JRCond(FlagTarget),
    // Ret,
    // RLA,
    // prefixed
    // Rl(Target),
    // Bit(Target, u8),

    #[test]
    fn ld_works() {
        let mut cpu = CPU::new();
        let mut mem = Memory::new();
        mem.write_byte(0x1, 2);
        cpu.execute(Instruction::Ld(Target::A, Source::N), &mut mem);
        assert_eq!(cpu.registers.a, 2);
        cpu.execute(Instruction::Ld(Target::B, Source::A), &mut mem);
        assert_eq!(cpu.registers.b, 2);
    }

    #[test]
    fn basic_addrld_works() {
        let mut cpu = CPU::new();
        let mut mem = Memory::new();
        mem.write_byte(0x1, 2);
        cpu.execute(Instruction::Ld(Target::A, Source::N), &mut mem);
        cpu.execute(Instruction::AddrLd(AddrTarget::BC, AddrSource::A), &mut mem);
        // Byte addr where BC points changes
        assert_eq!(mem.read_byte(cpu.registers.get_bc()), 2);

        mem.write_byte(0x1, 3);
        cpu.execute(Instruction::Ld(Target::A, Source::N), &mut mem);
        assert_eq!(cpu.registers.a, 3);
        cpu.execute(Instruction::AddrLd(AddrTarget::A, AddrSource::BC), &mut mem);
        assert_eq!(cpu.registers.a, 2);
    }

    #[test]
    fn hl_addrld_works() {
        let mut cpu = CPU::new();
        let mut mem = Memory::new();
        mem.write_byte(0x1, 2);
        cpu.execute(Instruction::Ld(Target::A, Source::N), &mut mem);
        // test LD (HL+), A
        cpu.execute(Instruction::AddrLd(AddrTarget::HLPlus, AddrSource::A), &mut mem);
        // Byte addr where HL points changes
        assert_eq!(mem.read_byte(cpu.registers.get_hl()), 2);
        assert_eq!(cpu.registers.get_hl(), 1);

        // test LD A, (HL+)
        mem.write_byte(0, 1);
        cpu.execute(Instruction::AddrLd(AddrTarget::A, AddrSource::HLPlus), &mut mem);
        assert_eq!(cpu.registers.a, 1);
        assert_eq!(cpu.registers.get_hl(), 2);

        // test LD (HL-), A
        cpu.execute(Instruction::AddrLd(AddrTarget::HLMinus, AddrSource::A), &mut mem);
        assert_eq!(mem.read_byte(cpu.registers.get_hl()+1), 1);
        assert_eq!(cpu.registers.get_hl(), 1);
    }

    #[test]
    fn xor_works() {
        let mut cpu = CPU::new();
        let mut mem = Memory::new();
        mem.write_byte(0x1, 2);
        cpu.execute(Instruction::Ld(Target::A, Source::N), &mut mem);
        cpu.execute(Instruction::Ld(Target::B, Source::A), &mut mem);
        cpu.execute(Instruction::Xor(Target::B), &mut mem);
        // 2 ^ 2 = 0
        assert_eq!(cpu.registers.a, 0);
        // flags are is_zero?, false, false, false
        assert_eq!(cpu.registers.flags, 0b1000_0000);
        cpu.execute(Instruction::Xor(Target::B), &mut mem);
        // 0 ^ 2 = 2
        assert_eq!(cpu.registers.a, 2);
        assert_eq!(cpu.registers.flags, 0b0000_0000);
    }

    #[test]
    fn cp_works() {
        let mut cpu = CPU::new();
        let mut mem = Memory::new();
        mem.write_byte(0x1, 2);
        cpu.execute(Instruction::Ld(Target::A, Source::N), &mut mem);
        cpu.execute(Instruction::Cp(Target::N), &mut mem);
        // 2-2=0, is_subtracted
        assert_eq!(cpu.registers.flags, 0b1100_0000);
        mem.write_byte(0x1, 3);
        cpu.execute(Instruction::Cp(Target::N), &mut mem);
        // 2-3=255
        assert_eq!(cpu.registers.flags, 0b0111_0000); // is this right? half_carry?

        mem.write_byte(0x1, 16);
        cpu.execute(Instruction::Ld(Target::A, Source::N), &mut mem);
        mem.write_byte(0x1, 1);
        cpu.execute(Instruction::Cp(Target::N), &mut mem);
        // 16-1=15
        assert_eq!(cpu.registers.flags, 0b0110_0000);

        mem.write_byte(0x1, 15);
        cpu.execute(Instruction::Ld(Target::A, Source::N), &mut mem);
        mem.write_byte(0x1, 1);
        cpu.execute(Instruction::Cp(Target::N), &mut mem);
        // 16-1=15
        assert_eq!(cpu.registers.flags, 0b0100_0000);
    }

    #[test]
    fn jrcond_works() {
        let mut cpu = CPU::new();
        let mut mem = Memory::new();
        mem.write_byte(0x1, 5);
        cpu.execute(Instruction::Ld(Target::A, Source::N), &mut mem);
        cpu.execute(Instruction::Cp(Target::N), &mut mem);
        assert_eq!(cpu.registers.flags, 0b1100_0000);
        let step = cpu.execute(Instruction::JRCond(FlagTarget::Z), &mut mem);
        // DMG-1 codes pc to increase first (+2 here), then add to pc for Jump
        assert_eq!(step, 5+2);

        // test jump fails
        mem.write_byte(0x1, 2);
        cpu.execute(Instruction::Cp(Target::N), &mut mem);
        assert_eq!(cpu.registers.flags, 0b0100_0000);
        let step = cpu.execute(Instruction::JRCond(FlagTarget::Z), &mut mem);
        assert_eq!(step, 2);

        // test NZ
        let step = cpu.execute(Instruction::JRCond(FlagTarget::NZ), &mut mem);
        assert_eq!(step, 2+2);

        // test NC
        let step = cpu.execute(Instruction::JRCond(FlagTarget::NC), &mut mem);
        assert_eq!(step, 2+2);

        // test C
        mem.write_byte(0x1, 6);
        cpu.execute(Instruction::Cp(Target::N), &mut mem);
        // a contains 5. 5-6 = 255
        assert_eq!(cpu.registers.flags, 0b0111_0000);
        let step = cpu.execute(Instruction::JRCond(FlagTarget::C), &mut mem);
        assert_eq!(step, 6+2);
    }
}