use std::{fmt::{self, Display, Formatter}, thread, time::Duration};

use crate::memory::Memory;
use instruction::Instruction;
use registers::Registers;

use self::instruction::*;

pub mod instruction;
pub mod registers;
mod more_cpu_methods;
mod cpu_test;

#[derive(Default)]
pub struct CPU {
    pc: u16,
    registers: Registers,
    stack: Vec<u16>
}
impl CPU {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn step(&mut self, mem: &mut Memory) {
        if self.pc > 0x69 && self.pc < 0x95 {
            thread::sleep(Duration::from_millis(1000));
        }
        println!("Prior to current execution {}", self);
        //fetch
        let mut byte = mem.read_byte(self.pc);
        println!("Next Instruction: {byte:#04X}");
        // Prefixed check
        let prefixed = byte == 0xCB;
        if prefixed {
            self.pc += 1;
            byte = mem.read_byte(self.pc);
            println!("Prefixed Instruction: {byte:#04X}");
        }
        
        // decode
        let instr: Option<Instruction> = Instruction::decode(byte, prefixed);
        
        println!("{:?}", instr);
        println!();

        // execute
        self.pc = if let Some(instruction) = instr {
            self.execute(instruction, mem)
        } else {
            panic!("Unknown instruction found for: {byte:#04X}");
        };
    }

    fn n(&self, mem: &Memory) -> u8 {
        mem.read_byte(self.pc+1)
    }
    fn nn(&self, mem: &Memory) -> u16 {
        mem.read_byte(self.pc+1) as u16 | ((mem.read_byte(self.pc+2) as u16) << 8)
    }

    pub fn execute(&mut self, instr: Instruction, mem: &mut Memory) -> u16 {
        match instr {
            // ---------- 8-Bit Loads ----------
            // LD r8||(HL), r8||(HL). Puts r8 or data at address HL into r8 or data at address HL
            Instruction::Ld(target, source) => {
                match source {
                    Source::A => {
                        match target {
                            Target::A => (),
                            Target::B => self.registers.b = self.registers.a,
                            Target::C => self.registers.c = self.registers.a,
                            Target::D => self.registers.d = self.registers.a,
                            Target::E => self.registers.e = self.registers.a,
                            Target::H => self.registers.h = self.registers.a,
                            Target::L => self.registers.l = self.registers.a,
                            Target::HL => mem.write_byte(self.registers.get_hl(), self.registers.a),
                            Target::N => unreachable!("ERROR: Somehow LD n, A")
                        }
                        self.pc.wrapping_add(1)
                    },
                    Source::B => {
                        match target {
                            Target::A => self.registers.a = self.registers.b,
                            Target::B => (),
                            Target::C => self.registers.c = self.registers.b,
                            Target::D => self.registers.d = self.registers.b,
                            Target::E => self.registers.e = self.registers.b,
                            Target::H => self.registers.h = self.registers.b,
                            Target::L => self.registers.l = self.registers.b,
                            Target::HL => mem.write_byte(self.registers.get_hl(), self.registers.b),
                            Target::N => unreachable!("ERROR: Somehow LD n, B")
                        }
                        self.pc.wrapping_add(1)
                    },
                    Source::C => {
                        match target {
                            Target::A => self.registers.a = self.registers.c,
                            Target::B => self.registers.b = self.registers.c,
                            Target::C => (),
                            Target::D => self.registers.d = self.registers.c,
                            Target::E => self.registers.e = self.registers.c,
                            Target::H => self.registers.h = self.registers.c,
                            Target::L => self.registers.l = self.registers.c,
                            Target::HL => mem.write_byte(self.registers.get_hl(), self.registers.c),
                            Target::N => unreachable!("ERROR: Somehow LD n, C")
                        }
                        self.pc.wrapping_add(1)
                    },
                    Source::D => {
                        match target {
                            Target::A => self.registers.a = self.registers.d,
                            Target::B => self.registers.b = self.registers.d,
                            Target::C => self.registers.c = self.registers.d,
                            Target::D => (),
                            Target::E => self.registers.e = self.registers.d,
                            Target::H => self.registers.h = self.registers.d,
                            Target::L => self.registers.l = self.registers.d,
                            Target::HL => mem.write_byte(self.registers.get_hl(), self.registers.d),
                            Target::N => unreachable!("ERROR: Somehow LD n, D")
                        }
                        self.pc.wrapping_add(1)
                    },
                    Source::E => {
                        match target {
                            Target::A => self.registers.a = self.registers.e,
                            Target::B => self.registers.b = self.registers.e,
                            Target::C => self.registers.c = self.registers.e,
                            Target::D => self.registers.d = self.registers.e,
                            Target::E => (),
                            Target::H => self.registers.h = self.registers.e,
                            Target::L => self.registers.l = self.registers.e,
                            Target::HL => mem.write_byte(self.registers.get_hl(), self.registers.e),
                            Target::N => unreachable!("ERROR: Somehow LD n, E")
                        }
                        self.pc.wrapping_add(1)
                    },
                    Source::H => {
                        match target {
                            Target::A => self.registers.a = self.registers.h,
                            Target::B => self.registers.b = self.registers.h,
                            Target::C => self.registers.c = self.registers.h,
                            Target::D => self.registers.d = self.registers.h,
                            Target::E => self.registers.e = self.registers.h,
                            Target::H => (),
                            Target::L => self.registers.l = self.registers.h,
                            Target::HL => mem.write_byte(self.registers.get_hl(), self.registers.h),
                            Target::N => unreachable!("ERROR: Somehow LD n, H")
                        }
                        self.pc.wrapping_add(1)
                    },
                    Source::L => {
                        match target {
                            Target::A => self.registers.a = self.registers.l,
                            Target::B => self.registers.b = self.registers.l,
                            Target::C => self.registers.c = self.registers.l,
                            Target::D => self.registers.d = self.registers.l,
                            Target::E => self.registers.e = self.registers.l,
                            Target::H => self.registers.h = self.registers.l,
                            Target::L => (),
                            Target::HL => mem.write_byte(self.registers.get_hl(), self.registers.l),
                            Target::N => unreachable!("ERROR: Somehow LD n, L")
                        }
                        self.pc.wrapping_add(1)
                    },
                    Source::HL => {
                        match target {
                            Target::A => self.registers.a = mem.read_byte(self.registers.get_hl()),
                            Target::B => self.registers.b = mem.read_byte(self.registers.get_hl()),
                            Target::C => self.registers.c = mem.read_byte(self.registers.get_hl()),
                            Target::D => self.registers.d = mem.read_byte(self.registers.get_hl()),
                            Target::E => self.registers.e = mem.read_byte(self.registers.get_hl()),
                            Target::H => self.registers.h = mem.read_byte(self.registers.get_hl()),
                            Target::L => self.registers.l = mem.read_byte(self.registers.get_hl()),
                            Target::HL => unreachable!("ERROR: Somehow LD (HL), (HL)"),
                            Target::N => unreachable!("ERROR: Somehow LD n, (HL)")
                        }
                        self.pc.wrapping_add(1)
                    },
                    Source::N => {
                        match target {
                            Target::A => self.registers.a = self.n(mem),
                            Target::B => self.registers.b = self.n(mem),
                            Target::C => self.registers.c = self.n(mem),
                            Target::D => self.registers.d = self.n(mem),
                            Target::E => self.registers.e = self.n(mem),
                            Target::H => self.registers.h = self.n(mem),
                            Target::L=> self.registers.l = self.n(mem),
                            Target::HL => mem.write_byte(self.registers.get_hl(), self.n(mem)),
                            Target::N => unreachable!("ERROR: Somehow LD n, n")
                        }
                        self.pc.wrapping_add(2)
                    }, 
                }
            }
            // Special LD for memory space + C/n, to/from A
            Instruction::LdFF00(target) => 
                match target {
                    FF00Target::CA => {
                        mem.write_byte(0xFF00+self.registers.c as u16, self.registers.a);
                        self.pc.wrapping_add(1)
                    },
                    FF00Target::AC => {
                        self.registers.a = mem.read_byte(0xFF00+self.registers.c as u16);
                        self.pc.wrapping_add(1)
                    },
                    FF00Target::U8A => {
                        mem.write_byte(0xFF00+self.n(mem) as u16, self.registers.a);
                        self.pc.wrapping_add(2)
                    },
                    FF00Target::AU8 => {
                        self.registers.a = mem.read_byte(0xFF00+self.n(mem) as u16);
                        self.pc.wrapping_add(2)
                    },
                }
            // Address LD
            Instruction::AddrLd(target, source) => {
                match source {
                    AddrSource::BC => 
                        match target {
                            AddrTarget::A => self.registers.a = mem.read_byte(self.registers.get_bc()),
                            _ => unreachable!("ERROR: Somehow reached AddrLd(BC, !A)")
                        },
                    AddrSource::DE => 
                        match target {
                            AddrTarget::A => self.registers.a = mem.read_byte(self.registers.get_de()),
                            _ => unreachable!("ERROR: Somehow reached AddrLd(DE, !A)")
                        },
                    AddrSource::HLPlus =>  
                        match target {
                            AddrTarget::A => {
                                self.registers.a = mem.read_byte(self.registers.get_bc());
                                self.registers.set_hl(self.registers.get_hl().wrapping_add(1)); // possible issue?
                            },
                            _ => unreachable!("ERROR: Somehow reached AddrLd(HL+, !A)")
                        },
                    AddrSource::HLMinus => 
                        match target {
                            AddrTarget::A => {
                                self.registers.a = mem.read_byte(self.registers.get_bc());
                                self.registers.set_hl(self.registers.get_hl().wrapping_sub(1)); // possible issue?
                            },
                            _ => unreachable!("ERROR: Somehow reached AddrLd(HL-, !A)")
                        },
                    AddrSource::A =>
                        match target {
                            AddrTarget::BC => mem.write_byte(self.registers.get_bc(), self.registers.a),
                            AddrTarget::DE => mem.write_byte(self.registers.get_de(), self.registers.a),
                            AddrTarget::HLPlus => {
                                mem.write_byte(self.registers.get_hl(), self.registers.a);
                                self.registers.set_hl(self.registers.get_hl().wrapping_add(1)); // possible issue?
                            },
                            AddrTarget::HLMinus => {
                                mem.write_byte(self.registers.get_hl(), self.registers.a);
                                self.registers.set_hl(self.registers.get_hl().wrapping_sub(1)); // possible issue?
                            },
                            AddrTarget::A => unreachable!("ERROR: Somehow reached AddrLd(A, A)")
                        }
                }
                self.pc.wrapping_add(1)
            },

            // ---------- 16-Bit Loads ----------
            // LD r16, u16. Puts value 16 into r16
            Instruction::Ld16(target) => {
                match target {
                    PairTarget::BC => self.registers.set_bc(self.nn(mem)),
                    PairTarget::DE => self.registers.set_de(self.nn(mem)),
                    PairTarget::HL => self.registers.set_hl(self.nn(mem)),
                    PairTarget::SP => self.registers.sp = self.nn(mem),
                    PairTarget::AF => unreachable!("I don't know if Ld r16, nn includes AF")
                }
                self.pc.wrapping_add(3)
            }
            // LD SP, HL
            // LD HL, SP+n | LDHL SP, n
            // LD (nn), SP
            // LD (u16), A
            Instruction::Ld16A(target) => {
                match target {
                    Ld16ATarget::NN => mem.write_byte(self.nn(mem), self.registers.a),
                    Ld16ATarget::A => self.registers.a = mem.read_byte(self.nn(mem)),
                }
                self.pc.wrapping_add(3)
            },
            // PUSH r16
            Instruction::Push(target) => {
                match target {
                    PairTarget::AF => self.stack.push(self.registers.get_af()),
                    PairTarget::BC => self.stack.push(self.registers.get_bc()),
                    PairTarget::DE => self.stack.push(self.registers.get_de()),
                    PairTarget::HL => self.stack.push(self.registers.get_hl()),
                    PairTarget::SP => unimplemented!("PUSH r16 doesn't include SP")
                }
                self.pc.wrapping_add(1)
            }
            // POP r16
            Instruction::Pop(target) => {
                match target {
                    PairTarget::AF => self.registers.set_af(self.stack.pop().expect("Popped stack with no values, POP AF")),
                    PairTarget::BC => self.registers.set_bc(self.stack.pop().expect("Popped stack with no values, POP BC")),
                    PairTarget::DE => self.registers.set_de(self.stack.pop().expect("Popped stack with no values, POP DE")),
                    PairTarget::HL => self.registers.set_hl(self.stack.pop().expect("Popped stack with no values, POP HL")),
                    PairTarget::SP => unreachable!("POP r16 doesn't include SP")
                }
                self.pc.wrapping_add(1)
            }
            
            // ---------- 8-Bit ALU ----------
            // ADD A, n
            // ADC A, n
            // SUB n
            // SBC A, n
            // AND n
            // OR n
            // XOR A, r8||(HL)||n. XORs n (target) with register A, result in A
            Instruction::Xor(target) =>
                match target {
                    Target::A => { self.registers.a = self.xor(self.registers.a); self.pc.wrapping_add(1) },
                    Target::B => { self.registers.a = self.xor(self.registers.b); self.pc.wrapping_add(1) },
                    Target::C => { self.registers.a = self.xor(self.registers.c); self.pc.wrapping_add(1) },
                    Target::D => { self.registers.a = self.xor(self.registers.d); self.pc.wrapping_add(1) },
                    Target::E => { self.registers.a = self.xor(self.registers.e); self.pc.wrapping_add(1) },
                    Target::H => { self.registers.a = self.xor(self.registers.h); self.pc.wrapping_add(1) },
                    Target::L => { self.registers.a = self.xor(self.registers.l); self.pc.wrapping_add(1) },
                    Target::HL => { // Possibly misunderstand
                        let read = mem.read_byte(self.registers.get_hl());
                        self.registers.a = self.xor(read);
                        self.pc.wrapping_add(1)
                    },
                    Target::N => { self.registers.a = self.xor(self.n(mem)); self.pc.wrapping_add(2) }
                }
            
            // CP A, r8||(HL)||n. Subtracts _ from A and sets flags, but does not store results
            Instruction::Cp(target) =>
                match target {
                    Target::A => { self.cp(self.registers.a); self.pc.wrapping_add(1) },
                    Target::B => { self.cp(self.registers.b); self.pc.wrapping_add(1) },
                    Target::C => { self.cp(self.registers.c); self.pc.wrapping_add(1) },
                    Target::D => { self.cp(self.registers.d); self.pc.wrapping_add(1) },
                    Target::E => { self.cp(self.registers.e); self.pc.wrapping_add(1) },
                    Target::H => { self.cp(self.registers.h); self.pc.wrapping_add(1) },
                    Target::L => { self.cp(self.registers.l); self.pc.wrapping_add(1) },
                    Target::HL => {
                        let read = mem.read_byte(self.registers.get_hl());
                        self.cp(read);
                        self.pc.wrapping_add(1) 
                    },
                    Target::N => { self.cp(self.n(mem)); self.pc.wrapping_add(2) },
                }
            // INC r8
            Instruction::Inc(target) => {
                match target {
                    Target::A => self.registers.a = self.inc(self.registers.a),
                    Target::B => self.registers.b = self.inc(self.registers.b),
                    Target::C => self.registers.c = self.inc(self.registers.c),
                    Target::D => self.registers.d = self.inc(self.registers.d),
                    Target::E => self.registers.e = self.inc(self.registers.e),
                    Target::H => self.registers.h = self.inc(self.registers.h),
                    Target::L => self.registers.l = self.inc(self.registers.l),
                    Target::HL => { // Possibly misunderstand
                        let read = mem.read_byte(self.registers.get_hl());
                        mem.write_byte(self.registers.get_hl(), self.inc(read))
                    },
                    Target::N => unreachable!("Error: Somehow got Inc N")
                }
                self.pc.wrapping_add(1)
            },
            // DEC r8
            Instruction::Dec(target) => {
                match target {
                    Target::A => self.registers.a = self.dec(self.registers.a),
                    Target::B => self.registers.b = self.dec(self.registers.b),
                    Target::C => self.registers.c = self.dec(self.registers.c),
                    Target::D => self.registers.d = self.dec(self.registers.d),
                    Target::E => self.registers.e = self.dec(self.registers.e),
                    Target::H => self.registers.h = self.dec(self.registers.h),
                    Target::L => self.registers.l = self.dec(self.registers.l),
                    Target::HL => {
                        let read = mem.read_byte(self.registers.get_hl());
                        mem.write_byte(self.registers.get_hl(), self.dec(read))
                    },
                    Target::N => unreachable!("Error: Somehow got Dec N")
                }
                self.pc.wrapping_add(1)
            },

            // ---------- 16-Bit ALU ----------
            // ADD HL, n
            // ADD SP, n
            // INC r16
            Instruction::Inc16(target) => {
                match target {
                    PairTarget::BC => self.registers.set_bc(self.registers.get_bc().wrapping_add(1)),
                    PairTarget::DE => self.registers.set_de(self.registers.get_de().wrapping_add(1)),
                    PairTarget::HL => self.registers.set_hl(self.registers.get_hl().wrapping_add(1)),
                    PairTarget::SP => self.registers.sp = self.registers.sp.wrapping_add(1),
                    PairTarget::AF => unreachable!("Error: Somehow got to Inc AF"),
                };
                self.pc.wrapping_add(1)
            },
            // DEC r16
            Instruction::Dec16(target) => {
                match target {
                    PairTarget::BC => self.registers.set_bc(self.registers.get_bc().wrapping_sub(1)),
                    PairTarget::DE => self.registers.set_de(self.registers.get_de().wrapping_sub(1)),
                    PairTarget::HL => self.registers.set_hl(self.registers.get_hl().wrapping_sub(1)),
                    PairTarget::SP => self.registers.sp = self.registers.sp.wrapping_sub(1),
                    PairTarget::AF => unreachable!("Error: Somehow got to Dec AF"),
                };
                self.pc.wrapping_add(1)
            },

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
            Instruction::RLA => {
                self.rla();
                self.pc.wrapping_add(1)
            },
            // RRCA
            // RRA

            // ---------- Jumps ----------
            // JP nn
            // JP cc, nn
            // JP (HL)
            // JR u8. Notes: Adds 2 to pc, because pc is usually added prior to exe. Also needed to convert n to i8 first, for proper conversion
            Instruction::JR => ((self.pc+2) as i16 + i16::from(self.n(mem) as i8)) as u16,
            // JR cc, n. If the condition is true, then add n to current address and jump to it
            Instruction::JRCond(condition) =>
                if match condition {
                    FlagTarget::NZ => !self.registers.check_flag_z(),
                    FlagTarget::Z => self.registers.check_flag_z(),
                    FlagTarget::NC => !self.registers.check_flag_c(),
                    FlagTarget::C => self.registers.check_flag_c(),
                } {
                    let next: i8 = self.n(mem) as i8;
                    // Note: I add here since pc is supposed to move before execute, after decode
                    ((self.pc+2) as i16 + next as i16) as u16
                } else {
                    self.pc.wrapping_add(2)
                }

            // ---------- Calls ----------
            // CALL nn
            Instruction::Call => {
                self.stack.push(self.pc+3);
                self.nn(mem)
            },
            // CALL cc, nn
            Instruction::CallCond(condition) => {
                if match condition {
                    FlagTarget::NZ => !self.registers.check_flag_z(),
                    FlagTarget::Z => self.registers.check_flag_z(),
                    FlagTarget::NC => !self.registers.check_flag_c(),
                    FlagTarget::C => self.registers.check_flag_c(),
                } {
                    self.stack.push(self.pc+3);
                    self.nn(mem)
                } else {
                    self.pc.wrapping_add(3)
                }
            }

            // ---------- Reset & Returns ----------
            // RST n
            // RET
            Instruction::Ret => {
                self.stack.pop().expect("Error popping from stack in RET")
            }
            // RET cc
            // RETI

            // ---------- PREFIX ----------
            // ---------- Rotations and Shifts 2 ----------
            // SWAP n
            // RLC n
            // RL r8
            Instruction::Rl(target) => {
                match target {
                    Target::A => self.registers.a = self.rl(self.registers.a),
                    Target::B => self.registers.b = self.rl(self.registers.b),
                    Target::C => self.registers.c = self.rl(self.registers.c),
                    Target::D => self.registers.d = self.rl(self.registers.d),
                    Target::E => self.registers.e = self.rl(self.registers.e),
                    Target::H => self.registers.h = self.rl(self.registers.h),
                    Target::L => self.registers.l = self.rl(self.registers.l),
                    Target::HL => { // possibly misunderstand
                        let read = mem.read_byte(self.registers.get_hl());
                        mem.write_byte(self.registers.get_hl(), self.rl(read));
                    },
                    Target::N => unreachable!("ERROR: Somehow got to Rl n"),
                }
                self.pc.wrapping_add(1)
            },
            // RRC n
            // RR
            // SLA n
            // SRA n
            // SRL n
            // BIT b, r. Test bit b in register r
            Instruction::Bit(target, mask) => {
                match target {
                    Target::A => self.bit(self.registers.a, mask),
                    Target::B => self.bit(self.registers.b, mask),
                    Target::C => self.bit(self.registers.c, mask),
                    Target::D => self.bit(self.registers.d, mask),
                    Target::E => self.bit(self.registers.e, mask),
                    Target::H => self.bit(self.registers.h, mask),
                    Target::L => self.bit(self.registers.l, mask),
                    _ => panic!("unimplemented BIT b, r")
                }
                self.pc.wrapping_add(1)
            },
            // SET b, r
            // RES b, r
        }
    }   
}

impl Display for CPU {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let s = format!("CPU:\npc: {:#06X}\nStack: {:#04X?}", self.pc, self.stack);
        write!(f, "{}\n{}", s, self.registers)
    }
}