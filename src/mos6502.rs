use crate::{
    bus::{Bus, BusDevice},
    opcodes::{self, Opcode},
};

pub const STACK_TOP: u16 = 0x100;

#[derive(PartialEq, Clone)]
pub enum AddrModes {
    IMP,
    ACC,
    IMM,
    ZP,
    ZPX,
    ZPY,
    RLT,
    ABS,
    ABX,
    ABY,
    IND,
    INX,
    INY,
}

#[repr(u8)]
#[derive(Clone)]
pub enum Flags {
    C = 1 << 0, // Carry Bit
    Z = 1 << 1, // Zero
    I = 1 << 2, // Disable Interrupts
    D = 1 << 3, // Decimal Mode
    B = 1 << 4, // Break
    U = 1 << 5, // Unused
    V = 1 << 6, // Overflow
    N = 1 << 7, // Negative
}

pub struct MOS6502 {
    pub pc: u16,
    pub ac: u8, // Accumulator
    pub x: u8,  // index X
    pub y: u8,  // index Y
    pub sr: u8, // status register

    pub stack_index: u8,
    pub addressing_modes: AddrModes,
    pub fetching_addr: u16, // la direccion de memoria de un valor resultante del modo de direccion
    pub relative: u16,      // la direccion de memoria de la rutine a ramificar
    pub cycles: u8,         // ciclos de la instruccion
}

impl Default for MOS6502 {
    fn default() -> Self {
        Self::new()
    }
}

impl MOS6502 {
    pub fn new() -> Self {
        Self {
            pc: 0,
            ac: 0,
            x: 0,
            y: 0,
            sr: 0,
            cycles: 0,
            stack_index: 0xFF,
            fetching_addr: 0,
            relative: 0,
            addressing_modes: AddrModes::IMP,
        }
    }
}

impl MOS6502 {
    pub fn clock(&mut self, bus: &mut Bus) -> Option<Opcode> {
        // siguiente instruccion
        if self.cycles == 0 {
            let opcode = bus.read(self.pc, BusDevice::Rom);
            let opcode = match opcodes::index_opcode(opcode) {
                Some(op) => op,
                _ => {
                    self.pc += 1;
                    return None;
                }
            };

            self.pc += 1;
            self.cycles = opcode.cycles;

            self.addressing_modes = opcode.addressing_mode.clone();
            match self.addressing_modes {
                AddrModes::IMP | AddrModes::ACC => {}
                AddrModes::IMM => self.imm(bus),
                AddrModes::ZP => self.zp(bus),
                AddrModes::ZPX => self.zpx(bus),
                AddrModes::ZPY => self.zpy(bus),
                AddrModes::RLT => self.rlt(bus),
                AddrModes::ABS => self.abs(bus),
                AddrModes::ABX => self.abx(bus),
                AddrModes::ABY => self.aby(bus),
                AddrModes::IND => self.ind(bus),
                AddrModes::INX => self.inx(bus),
                AddrModes::INY => self.iny(bus),
            };

            (opcode.action)(self, bus);
            return Some(opcode);
        }
        self.cycles -= 1;
        None
    }

    pub fn reset(&mut self, bus: &mut Bus) {
        let lo = bus.read(0xFFFC, BusDevice::Rom) as u16;
        let hi = bus.read(0xFFFC + 1, BusDevice::Rom) as u16;
        self.pc = (hi << 8) | (lo & 0x00FF);

        self.ac = 0;
        self.x = 0;
        self.y = 0;
        self.sr = 0;
        self.stack_index = 0xFF;
        self.fetching_addr = 0;
        self.relative = 0;
        self.cycles = 8;

        let opcode = bus.read(self.pc, BusDevice::Rom);
        if let Some(op) = opcodes::index_opcode(opcode) {
            self.cycles += op.cycles;
        };
    }

    pub fn fetch(&mut self, bus: &mut Bus) -> u8 {
        match self.addressing_modes {
            AddrModes::ACC => self.ac,
            AddrModes::IMM => bus.read(self.fetching_addr, BusDevice::Rom),
            _ => bus.read(self.fetching_addr, BusDevice::Ram),
        }
    }

    pub fn write(&mut self, bus: &mut Bus, v: u8) {
        if self.addressing_modes == AddrModes::ACC {
            self.ac = v;
        }
        bus.write(v, self.fetching_addr);
    }

    pub fn set_flag(&mut self, flag: Flags, value: bool) {
        if value {
            self.sr |= flag as u8;
        } else {
            self.sr &= !(flag as u8);
        }
    }

    pub fn get_flag(&self, flag: Flags) -> u8 {
        if (self.sr & flag as u8) != 0 {
            return 1;
        }
        0
    }

    pub fn imm(&mut self, _: &mut Bus) {
        self.fetching_addr = self.pc;
        self.pc += 1;
    }

    pub fn zp(&mut self, bus: &mut Bus) {
        self.fetching_addr = bus.read(self.pc, BusDevice::Ram) as u16 & 0x00FF;
        self.pc += 1;
    }

    pub fn zpx(&mut self, bus: &mut Bus) {
        self.fetching_addr = bus
            .read(self.pc & 0x00FF, BusDevice::Ram)
            .wrapping_add(self.x) as u16
            & 0x00FF;
        self.pc += 1;
    }

    pub fn zpy(&mut self, bus: &mut Bus) {
        self.fetching_addr = bus
            .read(self.pc & 0x00FF, BusDevice::Ram)
            .wrapping_add(self.y) as u16
            & 0x00FF;
        self.pc += 1;
    }

    pub fn rlt(&mut self, bus: &mut Bus) {
        self.relative = bus.read(self.pc, BusDevice::Rom) as u16;
        if self.relative & 0x80 != 0 {
            self.relative |= self.relative & 0xFF00;
        }
        self.pc += 1;
    }

    pub fn abs(&mut self, bus: &mut Bus) {
        let lo = bus.read(self.pc, BusDevice::Rom) as u16;
        self.pc += 1;
        let hi = bus.read(self.pc, BusDevice::Rom) as u16;
        self.pc += 1;

        self.fetching_addr = (hi << 8) | (lo & 0x00FF);
    }

    pub fn abx(&mut self, bus: &mut Bus) {
        let lo = bus.read(self.pc, BusDevice::Rom) as u16;
        self.pc += 1;
        let hi = bus.read(self.pc, BusDevice::Rom) as u16;
        self.pc += 1;

        self.fetching_addr = (hi << 8) | ((lo & 0x00FF) + self.x as u16);
    }

    pub fn aby(&mut self, bus: &mut Bus) {
        let lo = bus.read(self.pc, BusDevice::Rom) as u16;
        self.pc += 1;
        let hi = bus.read(self.pc, BusDevice::Rom) as u16;
        self.pc += 1;

        self.fetching_addr = (hi << 8) | ((lo & 0x00FF) + self.y as u16);
    }

    pub fn ind(&mut self, bus: &mut Bus) {
        let lo = bus.read(self.pc, BusDevice::Rom) as u16;
        self.pc += 1;
        let hi = bus.read(self.pc, BusDevice::Rom) as u16;
        self.pc += 1;

        let addrs = (hi << 8) | (lo & 0x00FF);
        let lo = bus.read(addrs, BusDevice::Ram) as u16;
        let hi = bus.read(addrs + 1, BusDevice::Ram) as u16;

        self.fetching_addr = (hi << 8) | (lo & 0x00FF);
    }

    pub fn inx(&mut self, bus: &mut Bus) {
        let zp_addrs = bus
            .read(self.pc & 0x00FF, BusDevice::Rom)
            .wrapping_add(self.x) as u16
            & 0x00FF;

        let lo = bus.read(zp_addrs, BusDevice::Ram) as u16;
        let hi = bus.read(zp_addrs + 1, BusDevice::Ram) as u16;

        self.fetching_addr = (hi << 8) | (lo & 0x00FF);

        self.pc += 1;
    }

    pub fn iny(&mut self, bus: &mut Bus) {
        let zp_addrs = bus.read(self.pc & 0x00FF, BusDevice::Rom) as u16 & 0x00FF;

        let lo = bus.read(zp_addrs, BusDevice::Ram) as u16;
        let hi = bus.read(zp_addrs + 1, BusDevice::Ram) as u16;

        self.fetching_addr = (hi << 8) | ((lo & 0x00FF) + self.y as u16);

        self.pc += 1;
    }
}
