pub const MAX_ROM_SIZE: usize = 0xFFFF;

pub enum BusDevice {
    Ram,
    Rom,
}

pub struct Bus {
    pub ram: Vec<u8>,
    pub rom: Vec<u8>,
}

impl Bus {
    pub fn new(rom: Vec<u8>) -> Self {
        Self {
            ram: vec![0x00; u16::MAX as usize],
            rom,
        }
    }

    pub fn write(&mut self, v: u8, a: u16) -> u8 {
        let tmp = self.ram[a as usize];
        self.ram[a as usize] = v;
        tmp
    }

    pub fn read(&self, a: u16, device: BusDevice) -> u8 {
        match device {
            BusDevice::Ram => {
                if (a as usize) < self.ram.len() {
                    self.ram[a as usize]
                } else {
                    0x00
                }
            }
            BusDevice::Rom => {
                if (a as usize) < self.rom.len() {
                    self.rom[a as usize]
                } else {
                    0x00
                }
            }
        }
    }
}
