use crate::{
    bus::Bus,
    mos6502::{AddrModes, Flags, MOS6502, STACK_TOP},
};

pub struct Opcode {
    pub cycles: u8,
    pub addressing_mode: AddrModes,
    pub mnemonic: &'static str,
    pub action: fn(mos6502: &mut MOS6502, bus: &mut Bus),
}

impl Opcode {
    pub fn new(
        mnemonic: &'static str,
        addressing_mode: AddrModes,
        action: fn(mos6502: &mut MOS6502, bus: &mut Bus),
        cycles: u8,
    ) -> Self {
        Self {
            cycles,
            addressing_mode,
            mnemonic,
            action,
        }
    }
}

pub fn deassembly(bus: &mut Bus) -> Vec<(usize, String)> {
    let mut instructions_per_address = Vec::with_capacity(0xFFFF);

    let mut i = 0;

    while i < bus.rom.len() {
        let opcode_index = bus.read(i as u16, crate::bus::BusDevice::Rom);
        let opcode = match index_opcode(opcode_index) {
            Some(op) => op,
            None => {
                i += 1;
                continue;
            }
        };
        let address = i;
        i += 1;

        match opcode.addressing_mode {
            AddrModes::IMP | AddrModes::ACC => {
                instructions_per_address.push((address, opcode.mnemonic.to_owned()));
            }
            AddrModes::IMM => {
                let value = bus.read(i as u16, crate::bus::BusDevice::Rom);
                i += 1;
                instructions_per_address
                    .push((address, format!("{} #${:#04X}", opcode.mnemonic, value)));
            }
            AddrModes::ZP => {
                let value = bus.read(i as u16 & 0x00FF, crate::bus::BusDevice::Rom);
                i += 1;
                instructions_per_address
                    .push((address, format!("{} ${:#04X}", opcode.mnemonic, value)));
            }
            AddrModes::ZPX => {
                let value = bus.read(i as u16 & 0x00FF, crate::bus::BusDevice::Rom);
                i += 1;
                instructions_per_address
                    .push((address, format!("{} ${:#04X}, X", opcode.mnemonic, value)));
            }
            AddrModes::ZPY => {
                let value = bus.read(i as u16 & 0x00FF, crate::bus::BusDevice::Rom);
                i += 1;
                instructions_per_address
                    .push((address, format!("{} ${:#04X}, Y", opcode.mnemonic, value)));
            }
            AddrModes::RLT => {
                let value = bus.read(i as u16 & 0x00FF, crate::bus::BusDevice::Rom);
                i += 1;
                instructions_per_address
                    .push((address, format!("{} ${:#04X}", opcode.mnemonic, value)));
            }
            AddrModes::ABS => {
                let lo = bus.read(i as u16 & 0x00FF, crate::bus::BusDevice::Rom) as u16;
                i += 1;
                let hi = bus.read(i as u16 & 0x00FF, crate::bus::BusDevice::Rom) as u16;
                i += 1;
                let value = (hi << 8) | (lo & 0x00FF);
                instructions_per_address
                    .push((address, format!("{} ${:#06X}", opcode.mnemonic, value)));
            }
            AddrModes::ABX => {
                let lo = bus.read(i as u16 & 0x00FF, crate::bus::BusDevice::Rom) as u16;
                i += 1;
                let hi = bus.read(i as u16 & 0x00FF, crate::bus::BusDevice::Rom) as u16;
                i += 1;
                let value = (hi << 8) | (lo & 0x00FF);
                instructions_per_address
                    .push((address, format!("{} ${:#06X}, X", opcode.mnemonic, value)));
            }
            AddrModes::ABY => {
                let lo = bus.read(i as u16 & 0x00FF, crate::bus::BusDevice::Rom) as u16;
                i += 1;
                let hi = bus.read(i as u16 & 0x00FF, crate::bus::BusDevice::Rom) as u16;
                i += 1;
                let value = (hi << 8) | (lo & 0x00FF);
                instructions_per_address
                    .push((address, format!("{} ${:#06X}, Y", opcode.mnemonic, value)));
            }
            AddrModes::IND => {
                let lo = bus.read(i as u16 & 0x00FF, crate::bus::BusDevice::Rom) as u16;
                i += 1;
                let hi = bus.read(i as u16 & 0x00FF, crate::bus::BusDevice::Rom) as u16;
                i += 1;
                let value = (hi << 8) | (lo & 0x00FF);
                instructions_per_address
                    .push((address, format!("{} (${:#06X})", opcode.mnemonic, value)));
            }
            AddrModes::INX => {
                let value = bus.read(i as u16 & 0x00FF, crate::bus::BusDevice::Ram);
                i += 1;
                instructions_per_address
                    .push((address, format!("{} (${:#04X}, X)", opcode.mnemonic, value)));
            }
            AddrModes::INY => {
                let value = bus.read(i as u16 & 0x00FF, crate::bus::BusDevice::Ram);
                i += 1;
                instructions_per_address
                    .push((address, format!("{} (${:#04X}), Y", opcode.mnemonic, value)));
            }
        };
    }
    instructions_per_address
}

pub fn index_opcode(opcode_index: u8) -> Option<Opcode> {
    match opcode_index {
        0x69 => Some(Opcode::new("ADC", AddrModes::IMM, adc, 2)),
        0x65 => Some(Opcode::new("ADC", AddrModes::ZP, adc, 2)),
        0x75 => Some(Opcode::new("ADC", AddrModes::ZPX, adc, 4)),
        0x6D => Some(Opcode::new("ADC", AddrModes::ABS, adc, 4)),
        0x7D => Some(Opcode::new("ADC", AddrModes::ABX, adc, 4)),
        0x79 => Some(Opcode::new("ADC", AddrModes::ABY, adc, 4)),
        0x61 => Some(Opcode::new("ADC", AddrModes::INX, adc, 6)),
        0x71 => Some(Opcode::new("ADC", AddrModes::INY, adc, 5)),

        0x29 => Some(Opcode::new("AND", AddrModes::IMM, and, 3)),
        0x25 => Some(Opcode::new("AND", AddrModes::ZP, and, 4)),
        0x35 => Some(Opcode::new("AND", AddrModes::ZPX, and, 4)),
        0x2D => Some(Opcode::new("AND", AddrModes::ABS, and, 4)),
        0x3D => Some(Opcode::new("AND", AddrModes::ABX, and, 4)),
        0x39 => Some(Opcode::new("AND", AddrModes::ABY, and, 4)),
        0x21 => Some(Opcode::new("AND", AddrModes::INX, and, 6)),
        0x31 => Some(Opcode::new("AND", AddrModes::INY, and, 5)),

        0x0A => Some(Opcode::new("ASL", AddrModes::ACC, asl, 2)),
        0x06 => Some(Opcode::new("ASL", AddrModes::ZP, asl, 5)),
        0x16 => Some(Opcode::new("ASL", AddrModes::ZPX, asl, 6)),
        0x0E => Some(Opcode::new("ASL", AddrModes::ABS, asl, 6)),
        0x1E => Some(Opcode::new("ASL", AddrModes::ABX, asl, 7)),

        0x90 => Some(Opcode::new("BCC", AddrModes::RLT, bcc, 2)),
        0xB0 => Some(Opcode::new("BCS", AddrModes::RLT, bcs, 2)),
        0xF0 => Some(Opcode::new("BEQ", AddrModes::RLT, beq, 2)),

        0x24 => Some(Opcode::new("BIT", AddrModes::ZP, bit, 3)),
        0x2C => Some(Opcode::new("BIT", AddrModes::ABS, bit, 4)),

        0x30 => Some(Opcode::new("BMI", AddrModes::RLT, bmi, 2)),
        0xD0 => Some(Opcode::new("BNE", AddrModes::RLT, bne, 2)),
        0x10 => Some(Opcode::new("BPL", AddrModes::RLT, bpl, 2)),

        0x00 => Some(Opcode::new("BRK", AddrModes::IMP, brk, 7)),

        0x50 => Some(Opcode::new("BVC", AddrModes::RLT, bvc, 2)),
        0x70 => Some(Opcode::new("BVS", AddrModes::RLT, bvs, 2)),

        0x18 => Some(Opcode::new("CLC", AddrModes::IMP, clc, 2)),
        0xD8 => Some(Opcode::new("CLD", AddrModes::IMP, cld, 2)),
        0x58 => Some(Opcode::new("CLI", AddrModes::IMP, cli, 2)),
        0xB8 => Some(Opcode::new("CLV", AddrModes::IMP, clv, 2)),

        0xC9 => Some(Opcode::new("CMP", AddrModes::IMM, cmp, 2)),
        0xC5 => Some(Opcode::new("CMP", AddrModes::ZP, cmp, 3)),
        0xD5 => Some(Opcode::new("CMP", AddrModes::ZPX, cmp, 4)),
        0xCD => Some(Opcode::new("CMP", AddrModes::ABS, cmp, 4)),
        0xDD => Some(Opcode::new("CMP", AddrModes::ABX, cmp, 4)),
        0xD9 => Some(Opcode::new("CMP", AddrModes::ABY, cmp, 4)),
        0xC1 => Some(Opcode::new("CMP", AddrModes::INX, cmp, 6)),
        0xD1 => Some(Opcode::new("CMP", AddrModes::INY, cmp, 5)),

        0xE0 => Some(Opcode::new("CPX", AddrModes::IMM, cpx, 2)),
        0xE4 => Some(Opcode::new("CPX", AddrModes::ZP, cpx, 3)),
        0xEC => Some(Opcode::new("CPX", AddrModes::ABS, cpx, 4)),

        0xC0 => Some(Opcode::new("CPY", AddrModes::IMM, cpy, 2)),
        0xC4 => Some(Opcode::new("CPY", AddrModes::ZP, cpy, 3)),
        0xCC => Some(Opcode::new("CPY", AddrModes::ABS, cpy, 4)),

        0xC6 => Some(Opcode::new("DEC", AddrModes::ZP, dec, 5)),
        0xD6 => Some(Opcode::new("DEC", AddrModes::ZPX, dec, 6)),
        0xCE => Some(Opcode::new("DEC", AddrModes::ABS, dec, 6)),
        0xDE => Some(Opcode::new("DEC", AddrModes::ABX, dec, 7)),

        0xCA => Some(Opcode::new("DEX", AddrModes::IMP, dex, 2)),

        0x88 => Some(Opcode::new("DEY", AddrModes::IMP, dey, 2)),

        0x49 => Some(Opcode::new("EOR", AddrModes::IMM, eor, 2)),
        0x45 => Some(Opcode::new("EOR", AddrModes::ZP, eor, 3)),
        0x55 => Some(Opcode::new("EOR", AddrModes::ZPX, eor, 4)),
        0x4D => Some(Opcode::new("EOR", AddrModes::ABS, eor, 4)),
        0x5D => Some(Opcode::new("EOR", AddrModes::ABX, eor, 4)),
        0x59 => Some(Opcode::new("EOR", AddrModes::ABY, eor, 4)),
        0x41 => Some(Opcode::new("EOR", AddrModes::INX, eor, 6)),
        0x51 => Some(Opcode::new("EOR", AddrModes::INY, eor, 5)),

        0xE6 => Some(Opcode::new("INC", AddrModes::ZP, inc, 5)),
        0xF6 => Some(Opcode::new("INC", AddrModes::ZPX, inc, 6)),
        0xEE => Some(Opcode::new("INC", AddrModes::ABS, inc, 6)),
        0xFE => Some(Opcode::new("INC", AddrModes::ABX, inc, 7)),

        0xE8 => Some(Opcode::new("INX", AddrModes::IMP, inx, 2)),

        0xC8 => Some(Opcode::new("INY", AddrModes::IMP, iny, 2)),

        0x4C => Some(Opcode::new("JMP", AddrModes::ABS, jmp, 3)),
        0x6C => Some(Opcode::new("JMP", AddrModes::IND, jmp, 5)),

        0x20 => Some(Opcode::new("JSR", AddrModes::ABS, jsr, 6)),

        0xA9 => Some(Opcode::new("LDA", AddrModes::IMM, lda, 2)),
        0xA5 => Some(Opcode::new("LDA", AddrModes::ZP, lda, 3)),
        0xB5 => Some(Opcode::new("LDA", AddrModes::ZPX, lda, 4)),
        0xAD => Some(Opcode::new("LDA", AddrModes::ABS, lda, 4)),
        0xBD => Some(Opcode::new("LDA", AddrModes::ABX, lda, 4)),
        0xB9 => Some(Opcode::new("LDA", AddrModes::ABY, lda, 4)),
        0xA1 => Some(Opcode::new("LDA", AddrModes::INX, lda, 6)),
        0xB1 => Some(Opcode::new("LDA", AddrModes::INY, lda, 5)),

        0xA2 => Some(Opcode::new("LDX", AddrModes::IMM, ldx, 2)),
        0xA6 => Some(Opcode::new("LDX", AddrModes::ZP, ldx, 3)),
        0xB6 => Some(Opcode::new("LDX", AddrModes::ZPY, ldx, 4)),
        0xAE => Some(Opcode::new("LDX", AddrModes::ABS, ldx, 4)),
        0xBE => Some(Opcode::new("LDX", AddrModes::ABY, ldx, 4)),

        0xA0 => Some(Opcode::new("LDY", AddrModes::IMM, ldy, 5)),
        0xA4 => Some(Opcode::new("LDY", AddrModes::ZP, ldy, 5)),
        0xB4 => Some(Opcode::new("LDY", AddrModes::ZPX, ldy, 5)),
        0xAC => Some(Opcode::new("LDY", AddrModes::ABS, ldy, 5)),
        0xBC => Some(Opcode::new("LDY", AddrModes::ABX, ldy, 5)),

        0x4A => Some(Opcode::new("LSR", AddrModes::ACC, lsr, 2)),
        0x46 => Some(Opcode::new("LSR", AddrModes::ZP, lsr, 5)),
        0x56 => Some(Opcode::new("LSR", AddrModes::ZPX, lsr, 6)),
        0x4E => Some(Opcode::new("LSR", AddrModes::ABS, lsr, 6)),
        0x5E => Some(Opcode::new("LSR", AddrModes::ABX, lsr, 7)),

        0x09 => Some(Opcode::new("ORA", AddrModes::IMM, ora, 2)),
        0x05 => Some(Opcode::new("ORA", AddrModes::ZP, ora, 3)),
        0x15 => Some(Opcode::new("ORA", AddrModes::ZPX, ora, 4)),
        0x0D => Some(Opcode::new("ORA", AddrModes::ABS, ora, 4)),
        0x1D => Some(Opcode::new("ORA", AddrModes::ABX, ora, 4)),
        0x19 => Some(Opcode::new("ORA", AddrModes::ABY, ora, 4)),
        0x01 => Some(Opcode::new("ORA", AddrModes::INX, ora, 6)),
        0x11 => Some(Opcode::new("ORA", AddrModes::INY, ora, 5)),

        0x48 => Some(Opcode::new("PHA", AddrModes::IMP, pha, 3)),
        0x08 => Some(Opcode::new("PHP", AddrModes::IMP, php, 3)),

        0x68 => Some(Opcode::new("PLA", AddrModes::IMP, pla, 4)),
        0x28 => Some(Opcode::new("PLP", AddrModes::IMP, plp, 4)),

        0x2A => Some(Opcode::new("ROL", AddrModes::ACC, rol, 2)),
        0x26 => Some(Opcode::new("ROL", AddrModes::ZP, rol, 5)),
        0x36 => Some(Opcode::new("ROL", AddrModes::ZPX, rol, 6)),
        0x2E => Some(Opcode::new("ROL", AddrModes::ABS, rol, 6)),
        0x3E => Some(Opcode::new("ROL", AddrModes::ABX, rol, 7)),

        0x6A => Some(Opcode::new("ROR", AddrModes::ACC, ror, 2)),
        0x66 => Some(Opcode::new("ROR", AddrModes::ACC, ror, 5)),
        0x76 => Some(Opcode::new("ROR", AddrModes::ACC, ror, 6)),
        0x6E => Some(Opcode::new("ROR", AddrModes::ACC, ror, 6)),
        0x7E => Some(Opcode::new("ROR", AddrModes::ACC, ror, 7)),

        0x40 => Some(Opcode::new("RTI", AddrModes::IMP, rti, 6)),
        0x60 => Some(Opcode::new("RTS", AddrModes::IMP, rts, 6)),

        0xE9 => Some(Opcode::new("SBC", AddrModes::IMM, sbc, 2)),
        0xE5 => Some(Opcode::new("SBC", AddrModes::ZP, sbc, 3)),
        0xF5 => Some(Opcode::new("SBC", AddrModes::ZPX, sbc, 4)),
        0xED => Some(Opcode::new("SBC", AddrModes::ABS, sbc, 4)),
        0xFD => Some(Opcode::new("SBC", AddrModes::ABX, sbc, 4)),
        0xF9 => Some(Opcode::new("SBC", AddrModes::ABY, sbc, 4)),
        0xE1 => Some(Opcode::new("SBC", AddrModes::INX, sbc, 6)),
        0xF1 => Some(Opcode::new("SBC", AddrModes::INY, sbc, 5)),

        0x38 => Some(Opcode::new("SEC", AddrModes::IMP, sec, 2)),
        0xF8 => Some(Opcode::new("SED", AddrModes::IMP, sed, 2)),
        0x78 => Some(Opcode::new("SEI", AddrModes::IMP, sei, 2)),

        0x85 => Some(Opcode::new("STA", AddrModes::ZP, sta, 3)),
        0x95 => Some(Opcode::new("STA", AddrModes::ZPX, sta, 3)),
        0x8D => Some(Opcode::new("STA", AddrModes::ABS, sta, 3)),
        0x9D => Some(Opcode::new("STA", AddrModes::ABX, sta, 3)),
        0x99 => Some(Opcode::new("STA", AddrModes::ABY, sta, 3)),
        0x81 => Some(Opcode::new("STA", AddrModes::INX, sta, 3)),
        0x91 => Some(Opcode::new("STA", AddrModes::INY, sta, 3)),

        0x86 => Some(Opcode::new("STX", AddrModes::ZP, stx, 3)),
        0x96 => Some(Opcode::new("STX", AddrModes::ZPX, stx, 4)),
        0x8E => Some(Opcode::new("STX", AddrModes::ABS, stx, 4)),

        0x84 => Some(Opcode::new("STY", AddrModes::ZP, sty, 3)),
        0x94 => Some(Opcode::new("STY", AddrModes::ZPX, sty, 4)),
        0x8C => Some(Opcode::new("STY", AddrModes::ABS, sty, 4)),

        0xAA => Some(Opcode::new("TAX", AddrModes::IMP, tax, 2)),
        0xA8 => Some(Opcode::new("TAY", AddrModes::IMP, tay, 2)),
        0xBA => Some(Opcode::new("TSX", AddrModes::IMP, tsx, 2)),
        0x8A => Some(Opcode::new("TXA", AddrModes::IMP, txa, 2)),
        0x9A => Some(Opcode::new("TXS", AddrModes::IMP, txs, 2)),
        0x98 => Some(Opcode::new("TYA", AddrModes::IMP, tya, 2)),

        _ => None,
    }
}

pub fn adc(mos6502: &mut MOS6502, bus: &mut Bus) {
    let fetched = mos6502.fetch(bus);
    let (res, o) = mos6502.ac.clone().overflowing_add(fetched);
    let (res, o2) = res.overflowing_add(mos6502.get_flag(Flags::C));

    mos6502.set_flag(Flags::V, o || o2);
    mos6502.set_flag(Flags::C, fetched as u16 + mos6502.ac as u16 > 0xFF);
    mos6502.set_flag(Flags::Z, (res & 0xff) == 0);
    mos6502.set_flag(Flags::N, res & 0x80 != 0);

    mos6502.write(bus, (res & 0xFF) as u8)
}

pub fn and(mos6502: &mut MOS6502, bus: &mut Bus) {
    let fetched = mos6502.fetch(bus);
    let res = mos6502.ac & fetched;

    mos6502.set_flag(Flags::Z, res == 0x0);
    mos6502.set_flag(Flags::N, (res & 0x80) != 0);

    mos6502.write(bus, res)
}

pub fn asl(mos6502: &mut MOS6502, bus: &mut Bus) {
    let fetched = mos6502.fetch(bus);
    let (res, overflow) = fetched.overflowing_shl(1);
    mos6502.set_flag(Flags::C, overflow);
    mos6502.set_flag(Flags::N, (res & 0x80) != 0);
    mos6502.set_flag(Flags::Z, res == 0x0);
    mos6502.write(bus, res)
}

pub fn bcc(mos6502: &mut MOS6502, _: &mut Bus) {
    if mos6502.get_flag(Flags::C) == 0 {
        mos6502.pc = (mos6502.pc as u8).wrapping_add(mos6502.relative as u8) as u16;
    }
}

pub fn bcs(mos6502: &mut MOS6502, _: &mut Bus) {
    if mos6502.get_flag(Flags::C) == 1 {
        mos6502.pc = (mos6502.pc as u8).wrapping_add(mos6502.relative as u8) as u16;
    }
}

pub fn beq(mos6502: &mut MOS6502, _: &mut Bus) {
    if mos6502.get_flag(Flags::Z) == 1 {
        mos6502.pc = (mos6502.pc as u8).wrapping_add(mos6502.relative as u8) as u16;
    }
}

pub fn bit(mos6502: &mut MOS6502, bus: &mut Bus) {
    let fetched = mos6502.fetch(bus);
    let res = mos6502.ac & fetched;

    mos6502.set_flag(Flags::Z, res == 0x0);
    mos6502.set_flag(Flags::N, (res & 0x80) != 0);
    mos6502.set_flag(Flags::V, (res & 0x40) != 0);
}

pub fn bmi(mos6502: &mut MOS6502, _: &mut Bus) {
    if mos6502.get_flag(Flags::N) == 1 {
        mos6502.pc = (mos6502.pc as u8).wrapping_add(mos6502.relative as u8) as u16;
    }
}

pub fn bne(mos6502: &mut MOS6502, _: &mut Bus) {
    if mos6502.get_flag(Flags::Z) == 0 {
        mos6502.pc = (mos6502.pc as u8).wrapping_add(mos6502.relative as u8) as u16;
    }
}

pub fn bpl(mos6502: &mut MOS6502, _: &mut Bus) {
    if mos6502.get_flag(Flags::N) == 0 {
        mos6502.pc = (mos6502.pc as u8).wrapping_add(mos6502.relative as u8) as u16;
    }
}

pub fn brk(_: &mut MOS6502, _: &mut Bus) {}

pub fn bvc(mos6502: &mut MOS6502, _: &mut Bus) {
    if mos6502.get_flag(Flags::V) == 0 {
        mos6502.pc = (mos6502.pc as u8).wrapping_add(mos6502.relative as u8) as u16;
    }
}

pub fn bvs(mos6502: &mut MOS6502, _: &mut Bus) {
    if mos6502.get_flag(Flags::V) == 1 {
        mos6502.pc = (mos6502.pc as u8).wrapping_add(mos6502.relative as u8) as u16;
    }
}

pub fn clc(mos6502: &mut MOS6502, _: &mut Bus) {
    mos6502.set_flag(Flags::C, false)
}

pub fn cld(mos6502: &mut MOS6502, _: &mut Bus) {
    mos6502.set_flag(Flags::D, false)
}

pub fn cli(mos6502: &mut MOS6502, _: &mut Bus) {
    mos6502.set_flag(Flags::I, false)
}

pub fn clv(mos6502: &mut MOS6502, _: &mut Bus) {
    mos6502.set_flag(Flags::V, false)
}

pub fn cmp(mos6502: &mut MOS6502, bus: &mut Bus) {
    let fetched = mos6502.fetch(bus);

    mos6502.set_flag(Flags::Z, mos6502.ac == fetched);
    mos6502.set_flag(Flags::C, fetched <= mos6502.ac);

    let res = mos6502.ac.wrapping_sub(fetched);
    mos6502.set_flag(Flags::N, (res & 0x80) != 0);
}

pub fn cpx(mos6502: &mut MOS6502, bus: &mut Bus) {
    let fetched = mos6502.fetch(bus);

    mos6502.set_flag(Flags::Z, mos6502.x == fetched);
    mos6502.set_flag(Flags::C, fetched <= mos6502.x);

    let res = mos6502.x.wrapping_sub(fetched);
    mos6502.set_flag(Flags::N, (res & 0x80) != 0);
}

pub fn cpy(mos6502: &mut MOS6502, bus: &mut Bus) {
    let fetched = mos6502.fetch(bus);

    mos6502.set_flag(Flags::Z, mos6502.y == fetched);
    mos6502.set_flag(Flags::C, fetched <= mos6502.y);

    let res = mos6502.y.wrapping_sub(fetched);
    mos6502.set_flag(Flags::N, (res & 0x80) != 0);
}

pub fn dec(mos6502: &mut MOS6502, bus: &mut Bus) {
    let fetched = mos6502.fetch(bus).wrapping_sub(1);
    mos6502.set_flag(Flags::N, (fetched & 0x80) != 0);
    mos6502.set_flag(Flags::Z, fetched == 0x0);
    mos6502.write(bus, fetched & 0xff)
}

pub fn dex(mos6502: &mut MOS6502, _: &mut Bus) {
    mos6502.x = mos6502.x.wrapping_sub(1);
    mos6502.set_flag(Flags::N, (mos6502.x & 0x80) != 0);
    mos6502.set_flag(Flags::Z, mos6502.x == 0x0);
}

pub fn dey(mos6502: &mut MOS6502, _: &mut Bus) {
    mos6502.y = mos6502.y.wrapping_sub(1);
    mos6502.set_flag(Flags::N, (mos6502.y & 0x80) != 0);
    mos6502.set_flag(Flags::Z, mos6502.y == 0x0);
}

pub fn eor(mos6502: &mut MOS6502, bus: &mut Bus) {
    let fetched = mos6502.fetch(bus);
    let res = mos6502.ac ^ fetched;

    mos6502.set_flag(Flags::Z, res == 0x0);
    mos6502.set_flag(Flags::N, (res & 0x80) != 0);

    mos6502.write(bus, res)
}

pub fn inc(mos6502: &mut MOS6502, bus: &mut Bus) {
    let res = mos6502.fetch(bus).wrapping_add(1);
    mos6502.set_flag(Flags::Z, res == 0x0);
    mos6502.set_flag(Flags::N, (mos6502.y & 0x80) != 0);
    mos6502.write(bus, res)
}

pub fn inx(mos6502: &mut MOS6502, _: &mut Bus) {
    mos6502.x = mos6502.x.wrapping_add(1);
    mos6502.set_flag(Flags::Z, mos6502.x == 0x0);
    mos6502.set_flag(Flags::N, (mos6502.x & 0x80) != 0);
}

pub fn iny(mos6502: &mut MOS6502, _: &mut Bus) {
    mos6502.y = mos6502.y.wrapping_add(1);
    mos6502.set_flag(Flags::Z, mos6502.y == 0x0);
    mos6502.set_flag(Flags::N, (mos6502.y & 0x80) != 0);
}

pub fn jmp(mos6502: &mut MOS6502, _: &mut Bus) {
    mos6502.pc = mos6502.fetching_addr;
}

pub fn jsr(mos6502: &mut MOS6502, bus: &mut Bus) {
    let subrutine_addr = mos6502.fetching_addr;
    mos6502.pc = subrutine_addr;

    mos6502.fetching_addr = STACK_TOP + mos6502.stack_index as u16;
    mos6502.stack_index = mos6502.stack_index.wrapping_sub(1);
    mos6502.write(bus, ((mos6502.pc >> 8) & 0x00FF) as u8);

    mos6502.fetching_addr = STACK_TOP + mos6502.stack_index as u16;
    mos6502.stack_index = mos6502.stack_index.wrapping_sub(1);
    mos6502.write(bus, (mos6502.pc & 0x00FF) as u8);
}

pub fn lda(mos6502: &mut MOS6502, bus: &mut Bus) {
    mos6502.ac = mos6502.fetch(bus);

    mos6502.set_flag(Flags::Z, mos6502.ac == 0x0);
    mos6502.set_flag(Flags::N, (mos6502.ac & 0x80) != 0);
}

pub fn ldx(mos6502: &mut MOS6502, bus: &mut Bus) {
    mos6502.x = mos6502.fetch(bus);

    mos6502.set_flag(Flags::Z, mos6502.x == 0x0);
    mos6502.set_flag(Flags::N, (mos6502.x & 0x80) != 0);
}

pub fn ldy(mos6502: &mut MOS6502, bus: &mut Bus) {
    mos6502.y = mos6502.fetch(bus);

    mos6502.set_flag(Flags::Z, mos6502.y == 0x0);
    mos6502.set_flag(Flags::N, (mos6502.y & 0x80) != 0);
}

pub fn lsr(mos6502: &mut MOS6502, bus: &mut Bus) {
    let fetched = mos6502.fetch(bus);
    let (res, overflow) = fetched.overflowing_shr(1);
    mos6502.set_flag(Flags::C, overflow);
    mos6502.set_flag(Flags::N, (res & 0x80) != 0);
    mos6502.set_flag(Flags::Z, res == 0);
    mos6502.write(bus, res)
}

pub fn nop(_: &mut MOS6502, _: &mut Bus) {}

pub fn ora(mos6502: &mut MOS6502, bus: &mut Bus) {
    let fetched = mos6502.fetch(bus);
    let res = mos6502.ac | fetched;

    mos6502.set_flag(Flags::Z, res == 0x0);
    mos6502.set_flag(Flags::N, (res & 0x80) != 0);

    mos6502.write(bus, res)
}

pub fn pha(mos6502: &mut MOS6502, bus: &mut Bus) {
    mos6502.fetching_addr = STACK_TOP + mos6502.stack_index as u16;
    mos6502.stack_index = mos6502.stack_index.wrapping_sub(1);
    mos6502.write(bus, mos6502.ac);
}

pub fn php(mos6502: &mut MOS6502, bus: &mut Bus) {
    mos6502.fetching_addr = STACK_TOP + mos6502.stack_index as u16;
    mos6502.stack_index = mos6502.stack_index.wrapping_sub(1);
    mos6502.write(bus, mos6502.sr);
}

pub fn pla(mos6502: &mut MOS6502, bus: &mut Bus) {
    mos6502.stack_index = mos6502.stack_index.wrapping_add(1);
    mos6502.fetching_addr = STACK_TOP + mos6502.stack_index as u16;
    let fetched = mos6502.fetch(bus);
    mos6502.ac = fetched;
    mos6502.set_flag(Flags::Z, mos6502.ac == 0x0);
    mos6502.set_flag(Flags::N, (mos6502.ac & 0x80) != 0);
}

pub fn plp(mos6502: &mut MOS6502, bus: &mut Bus) {
    mos6502.stack_index = mos6502.stack_index.wrapping_add(1);
    mos6502.fetching_addr = STACK_TOP + mos6502.stack_index as u16;
    let fetched = mos6502.fetch(bus);
    mos6502.sr = fetched;
    mos6502.set_flag(Flags::Z, mos6502.sr == 0x0);
    mos6502.set_flag(Flags::N, (mos6502.sr & 0x80) != 0);
}

pub fn rol(mos6502: &mut MOS6502, bus: &mut Bus) {
    let (mut res, overflow) = mos6502.fetch(bus).overflowing_shl(1);
    res |= mos6502.get_flag(Flags::C);

    mos6502.set_flag(Flags::C, overflow);
    mos6502.set_flag(Flags::Z, res == 0);
    mos6502.set_flag(Flags::N, (res & 0x80) != 0);

    mos6502.write(bus, res);
}

pub fn ror(mos6502: &mut MOS6502, bus: &mut Bus) {
    let (mut res, overflow) = mos6502.fetch(bus).overflowing_shr(1);
    res |= mos6502.get_flag(Flags::C);

    mos6502.set_flag(Flags::C, overflow);
    mos6502.set_flag(Flags::Z, res == 0);
    mos6502.set_flag(Flags::N, (res & 0x80) != 0);

    mos6502.write(bus, res);
}

pub fn rti(_: &mut MOS6502, _: &mut Bus) {}

pub fn rts(mos6502: &mut MOS6502, _: &mut Bus) {
    let lo = STACK_TOP + mos6502.stack_index as u16;
    mos6502.stack_index = mos6502.stack_index.wrapping_add(1);
    let hi = STACK_TOP + mos6502.stack_index as u16;
    mos6502.stack_index = mos6502.stack_index.wrapping_add(1);

    mos6502.pc = (hi << 8) | (lo & 0x00FF);
}

pub fn sbc(mos6502: &mut MOS6502, bus: &mut Bus) {
    let (res, o) = mos6502.ac.overflowing_sub(mos6502.fetch(bus));
    let (res, o2) = res.overflowing_sub(mos6502.get_flag(Flags::C));

    mos6502.set_flag(Flags::C, res > 127);
    mos6502.set_flag(Flags::Z, res == 0);
    mos6502.set_flag(Flags::V, o || o2);
    mos6502.set_flag(Flags::N, (res & 0x80) != 0);

    mos6502.write(bus, res);
}

pub fn sec(mos6502: &mut MOS6502, _: &mut Bus) {
    mos6502.set_flag(Flags::C, true);
}

pub fn sed(mos6502: &mut MOS6502, _: &mut Bus) {
    mos6502.set_flag(Flags::D, true);
}

pub fn sei(mos6502: &mut MOS6502, _: &mut Bus) {
    mos6502.set_flag(Flags::I, true);
}

pub fn sta(mos6502: &mut MOS6502, bus: &mut Bus) {
    mos6502.write(bus, mos6502.ac);
}

pub fn stx(mos6502: &mut MOS6502, bus: &mut Bus) {
    mos6502.write(bus, mos6502.x);
}

pub fn sty(mos6502: &mut MOS6502, bus: &mut Bus) {
    mos6502.write(bus, mos6502.y);
}

pub fn tax(mos6502: &mut MOS6502, _: &mut Bus) {
    mos6502.x = mos6502.ac;
    mos6502.set_flag(Flags::Z, mos6502.x == 0);
    mos6502.set_flag(Flags::N, (mos6502.x & 0x80) != 0);
}

pub fn tay(mos6502: &mut MOS6502, _: &mut Bus) {
    mos6502.y = mos6502.ac;
    mos6502.set_flag(Flags::Z, mos6502.y == 0);
    mos6502.set_flag(Flags::N, (mos6502.y & 0x80) != 0);
}

pub fn tsx(mos6502: &mut MOS6502, bus: &mut Bus) {
    mos6502.fetching_addr = STACK_TOP + mos6502.stack_index as u16;
    mos6502.x = mos6502.fetch(bus);
    mos6502.set_flag(Flags::Z, mos6502.x == 0);
    mos6502.set_flag(Flags::N, (mos6502.x & 0x80) != 0);
}

pub fn txa(mos6502: &mut MOS6502, _: &mut Bus) {
    mos6502.ac = mos6502.x;
    mos6502.set_flag(Flags::Z, mos6502.y == 0);
    mos6502.set_flag(Flags::N, (mos6502.y & 0x80) != 0);
}

pub fn txs(mos6502: &mut MOS6502, bus: &mut Bus) {
    mos6502.fetching_addr = STACK_TOP + mos6502.stack_index as u16;
    mos6502.write(bus, mos6502.x);
}

pub fn tya(mos6502: &mut MOS6502, _: &mut Bus) {
    mos6502.ac = mos6502.y;
}
