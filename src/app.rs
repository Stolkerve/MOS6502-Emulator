use egui::{
    ComboBox, FontFamily, FontId, Frame, Layout, Margin, RichText, SelectableLabel, Slider,
    TextStyle,
};
use egui_extras::{Column, TableBuilder};
use number_prefix::NumberPrefix;

#[cfg(target_arch = "wasm32")]
use rfd::AsyncFileDialog;

#[cfg(not(target_arch = "wasm32"))]
use rfd::FileDialog;
#[cfg(not(target_arch = "wasm32"))]
use std::io::Read;

use std::{collections::HashMap, ops::RangeInclusive};

use chrono::prelude::*;

use crate::{
    bus::Bus,
    mos6502::{Flags, MOS6502, STACK_TOP},
    opcodes,
};

#[derive(PartialEq, Eq)]
pub enum RamRegion {
    ZeroPage,
    Stack,
    All,
}

impl core::fmt::Display for RamRegion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RamRegion::ZeroPage => write!(f, "Zero page"),
            RamRegion::Stack => write!(f, "Stack"),
            RamRegion::All => write!(f, "All"),
        }
    }
}

pub struct Mos6502Emulator {
    bus: Bus,
    deassembly: Vec<(usize, String)>,
    frecuency: f64,
    last_time: DateTime<Utc>,
    is_running: bool,
    mos6502: MOS6502,
    pause: bool,
    instruction_index: usize,
    breakpoint: HashMap<usize, bool>,
    ram_region: RamRegion,
    op_cycles: u8,
}

impl Default for Mos6502Emulator {
    fn default() -> Self {
        let mut memory_editor = egui_memory_editor::MemoryEditor::new();
        memory_editor.set_address_range("All", 0..0xFFFF);
        Self {
            bus: Bus::new(Vec::new()),
            deassembly: Vec::new(),
            last_time: Utc::now(),
            is_running: false,
            mos6502: MOS6502::new(),
            pause: false,
            instruction_index: 0,
            breakpoint: HashMap::new(),
            ram_region: RamRegion::All,
            frecuency: 1.0,
            op_cycles: 0,
        }
    }
}

impl Mos6502Emulator {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        cc.egui_ctx.style_mut(|style| {
            style.override_font_id = Some(FontId::new(18.0, FontFamily::Proportional));
        });

        // if let Some(storage) = cc.storage {
        //     return eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default();
        // }

        Self::default()
    }
}

#[cfg(target_arch = "wasm32")]
async fn read_file() -> Option<Vec<u8>> {
    let files = AsyncFileDialog::new()
        .add_filter("prg", &["PRG", "prg"])
        .pick_file();
    if let Some(file_handle) = files.await {
        return Some(file_handle.read().await);
    }
    None
}

impl eframe::App for Mos6502Emulator {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if self.is_running {
            ctx.request_repaint();
            let now = Utc::now();
            if now.signed_duration_since(self.last_time).num_milliseconds() as f64 / 1000.0
                >= 1.0 / self.frecuency
                && !self.pause
            {
                self.last_time = now;
                if let Some(op) = self.mos6502.clock(&mut self.bus) {
                    self.instruction_index =
                        usize::clamp(self.instruction_index + 1, 0, self.deassembly.len() - 1);
                    self.op_cycles = op.cycles;
                }
            }
        }

        egui::TopBottomPanel::top("top_panel")
            .frame(Frame::default().inner_margin(Margin::symmetric(14.0, 8.0)))
            .show(ctx, |ui| {
                ui.with_layout(Layout::left_to_right(egui::Align::TOP), |ui| {
                    if ui.button(RichText::new("Load ROM").size(16.0)).clicked() {
                        #[cfg(target_arch = "wasm32")]
                        {
                            use pollster::FutureExt as _;
                            read_file().block_on();
                        }

                        #[cfg(not(target_arch = "wasm32"))]
                        {
                            let files = FileDialog::new()
                                .add_filter("prg", &["PRG", "prg"])
                                .pick_file();
                            if let Some(file_path) = files {
                                if file_path.is_file() {
                                    if let Ok(mut file) = std::fs::File::open(file_path) {
                                        let mut data = Vec::<u8>::new();
                                        if file.read_to_end(&mut data).is_ok() {
                                            self.bus.rom = data;
                                        }
                                    }
                                }
                            }
                        }

                        self.bus.ram.fill(0);

                        self.deassembly = opcodes::deassembly(&mut self.bus);

                        self.last_time = Utc::now();
                        self.is_running = true;

                        self.mos6502.reset(&mut self.bus);
                        self.breakpoint.clear();
                        self.instruction_index = 0;
                        self.op_cycles = self.mos6502.cycles;
                    }

                    if ui.button(RichText::new("Load sample").size(16.0)).clicked() {
                        let src = [
                            0xa2, 0x00, 0xa0, 0x00, 0x8a, 0x99, 0x00, 0x02, 0x48, 0xe8, 0xc8, 0xc0,
                            0x10, 0xd0, 0xf5, 0x68, 0x99, 0x00, 0x02, 0xc8, 0xc0, 0x20, 0xd0, 0xf7,
                        ];

                        self.bus.rom = src.to_vec();
                        self.bus.ram.fill(0);

                        self.deassembly = opcodes::deassembly(&mut self.bus);

                        self.last_time = Utc::now();
                        self.is_running = true;

                        self.mos6502.reset(&mut self.bus);
                        self.breakpoint.clear();
                        self.instruction_index = 0;
                        self.op_cycles = self.mos6502.cycles;
                    }
                    ui.add_space(16.0);

                    if ui.button("▶").clicked() {
                        self.pause = false;
                    }

                    let mut pause_btn = ui.button("⏸");
                    if self.pause {
                        pause_btn = pause_btn.highlight();
                    }
                    if pause_btn.clicked() {
                        self.pause = !self.pause;
                    }

                    if ui.button("↺").clicked() {
                        self.mos6502.reset(&mut self.bus);
                    }

                    ui.add_space(16.0);

                    // stack back
                    if ui.button("⏶").clicked() {
                        self.instruction_index = self.instruction_index.saturating_sub(1);

                        self.mos6502.pc =
                            self.deassembly.get(self.instruction_index).unwrap().0 as u16;
                    }

                    // step up
                    if ui.button("⏷").clicked()
                        && self.instruction_index < self.deassembly.len() - 1
                    {
                        self.instruction_index =
                            usize::min(self.instruction_index + 1, self.deassembly.len() - 1);

                        self.mos6502.pc =
                            self.deassembly.get(self.instruction_index).unwrap().0 as u16;
                    }

                    ui.add_space(16.0);

                    if self.is_running {
                        let result = match NumberPrefix::decimal(self.frecuency) {
                            NumberPrefix::Standalone(v) => {
                                format!("Clock: {:.1} Hz", v)
                            }
                            NumberPrefix::Prefixed(prefix, v) => {
                                format!("Clock: {:.1} {}HZ", v, prefix)
                            }
                        };
                        ui.add(Slider::new(&mut self.frecuency, 1.0..=100.0).step_by(1.0));
                        ui.label(result);

                        ui.add_space(16.0);
                        ui.label(format!(
                            "Cycles {} / {}",
                            self.mos6502.cycles, self.op_cycles
                        ));
                    }
                })
            });

        egui::SidePanel::left("left_panel")
            .resizable(false)
            .show_animated(ctx, true, |ui| {
                egui::TopBottomPanel::top("top_left_panel")
                    .frame(Frame::default().inner_margin(Margin::symmetric(4.0, 8.0)))
                    .resizable(false)
                    .show_inside(ui, |ui| {
                        ui.vertical_centered(|ui| {
                            ui.label(RichText::new("Registers").heading().strong().size(26.0));
                        });
                        ui.add_space(18.0);

                        ui.add(
                            Slider::new(&mut self.mos6502.pc, 0x0..=0xFFFF)
                                .custom_formatter(format_slider_hex(6))
                                .prefix("PC: "),
                        );
                        ui.add(
                            Slider::new(&mut self.mos6502.ac, 0x0..=0xFF)
                                .custom_formatter(format_slider_hex(4))
                                .prefix("A: "),
                        );
                        ui.add(
                            Slider::new(&mut self.mos6502.x, 0x0..=0xFF)
                                .custom_formatter(format_slider_hex(4))
                                .prefix("X: "),
                        );
                        ui.add(
                            Slider::new(&mut self.mos6502.y, 0x0..=0xFF)
                                .custom_formatter(format_slider_hex(4))
                                .prefix("Y: "),
                        );
                        ui.add(
                            Slider::new(&mut self.mos6502.stack_index, 0x0..=0xFF)
                                .custom_formatter(format_slider_hex(4))
                                .prefix("SP: "),
                        );

                        ui.add_space(10.0);

                        ui.horizontal(|ui| {
                            if ui
                                .add(SelectableLabel::new(
                                    self.mos6502.get_flag(Flags::N) != 0,
                                    "N",
                                ))
                                .clicked()
                            {
                                self.mos6502
                                    .set_flag(Flags::N, self.mos6502.get_flag(Flags::N) == 0);
                            }
                            ui.add(SelectableLabel::new(
                                self.mos6502.get_flag(Flags::V) != 0,
                                "V",
                            ));
                            ui.add(SelectableLabel::new(
                                self.mos6502.get_flag(Flags::B) != 0,
                                "B",
                            ));
                            ui.add(SelectableLabel::new(
                                self.mos6502.get_flag(Flags::D) != 0,
                                "D",
                            ));
                            ui.add(SelectableLabel::new(
                                self.mos6502.get_flag(Flags::I) != 0,
                                "I",
                            ));
                            ui.add(SelectableLabel::new(
                                self.mos6502.get_flag(Flags::Z) != 0,
                                "Z",
                            ));
                            ui.add(SelectableLabel::new(
                                self.mos6502.get_flag(Flags::C) != 0,
                                "C",
                            ));
                        });
                    });
                egui::CentralPanel::default().show_inside(ui, |ui| {
                    ui.vertical_centered(|ui| {
                        ui.label(RichText::new("Stack").heading().strong().size(26.0));
                        ui.add_space(18.0);
                        TableBuilder::new(ui)
                            .striped(true)
                            .cell_layout(egui::Layout::left_to_right(egui::Align::Center))
                            .column(Column::auto())
                            .column(Column::remainder())
                            .header(20.0, |mut header| {
                                header.col(|ui| {
                                    ui.strong("Address");
                                });
                                header.col(|ui| {
                                    ui.strong("Value");
                                });
                            })
                            .body(|mut body| {
                                for i in 0..=0xFF {
                                    body.row(20.0, |mut row| {
                                        if self.mos6502.stack_index == 0xFF - i as u8 {
                                            row.set_selected(true);
                                        }
                                        row.col(|ui| {
                                            ui.label(format!("{:03X}", STACK_TOP + 0xFF - i));
                                        });
                                        row.col(|ui| {
                                            ui.label(format!(
                                                "{:02X}",
                                                self.bus.ram[(STACK_TOP + 0xFF - i) as usize]
                                            ));
                                        });
                                    });
                                }
                            });
                    });
                });
            });

        egui::SidePanel::right("right_panel")
            .frame(Frame::central_panel(&ctx.style()).inner_margin(Margin::symmetric(4.0, 8.0)))
            .show(ctx, |ui| {
                ui.vertical_centered(|ui| {
                    ui.label(RichText::new("RAM").heading().strong().size(26.0));
                    ComboBox::from_label("Select Region")
                        .selected_text(self.ram_region.to_string())
                        .show_ui(ui, |ui| {
                            ui.selectable_value(
                                &mut self.ram_region,
                                RamRegion::ZeroPage,
                                "Zero page",
                            );
                            ui.selectable_value(&mut self.ram_region, RamRegion::Stack, "Stack");
                            ui.selectable_value(&mut self.ram_region, RamRegion::All, "All");
                        });
                    ui.add_space(10.0);
                });
                let text_style = TextStyle::Body;
                let height = ui.text_style_height(&text_style) + 5.0;
                let (ram_slice, offset): (&[u8], usize) = match self.ram_region {
                    RamRegion::ZeroPage => (&self.bus.ram[0..=0xFF], 0),
                    RamRegion::Stack => (
                        &self.bus.ram[STACK_TOP as usize..=STACK_TOP as usize + 0xFF],
                        STACK_TOP as usize,
                    ),
                    RamRegion::All => (self.bus.ram.as_slice(), 0),
                };

                TableBuilder::new(ui)
                    .striped(true)
                    .resizable(true)
                    .cell_layout(egui::Layout::left_to_right(egui::Align::Center))
                    .column(Column::auto())
                    .column(Column::remainder().resizable(false))
                    .header(20.0, |mut header| {
                        header.col(|ui| {
                            ui.strong("Address");
                        });

                        header.col(|ui| {
                            ui.strong("Values");
                        });
                    })
                    .body(|body| {
                        body.rows(height, ram_slice.len() / 16, |mut row| {
                            let row_index = row.index();
                            row.col(|ui| {
                                ui.label(format!("{:04X}", (row_index * 16) + offset));
                            });
                            row.col(|ui| {
                                let mut asd: [u8; 48] = [0u8; 48];
                                let mut offset = 0;
                                for i in 0..16 {
                                    let val_str =
                                        format!("{:02X}", ram_slice[(row_index * 16) + i]);
                                    asd[offset] = val_str.chars().nth(0).unwrap() as u8;
                                    asd[offset + 1] = val_str.chars().nth(1).unwrap() as u8;
                                    asd[offset + 2] = b' ';
                                    offset += 3;
                                }
                                ui.label(std::str::from_utf8(&asd).unwrap());
                            });
                        })
                    })
            });

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.label(RichText::new("Disassembly").heading().strong().size(26.0));
            ui.add_space(18.0);
            let text_style = TextStyle::Body;
            let height = ui.text_style_height(&text_style) + 10.0;

            TableBuilder::new(ui)
                .striped(true)
                .resizable(true)
                .cell_layout(egui::Layout::left_to_right(egui::Align::Center))
                .column(Column::auto().resizable(false))
                .column(Column::auto())
                .column(Column::remainder())
                .header(20.0, |mut header| {
                    header.col(|__| {});
                    header.col(|ui| {
                        ui.strong("Address");
                    });

                    header.col(|ui| {
                        ui.strong("Instruction");
                    });
                })
                .body(|body| {
                    body.rows(height, self.deassembly.len(), |mut row| {
                        let row_index = row.index();
                        let row_data = self.deassembly.get(row_index).unwrap();
                        let breakpoint_value = self.breakpoint.get(&row_index).cloned();
                        if self.mos6502.pc as usize == row_data.0 {
                            if breakpoint_value.unwrap_or(false) {
                                self.pause = true
                            }
                            row.set_selected(true);
                        }
                        row.col(|ui| {
                            if ui.radio(breakpoint_value.unwrap_or(false), "").clicked() {
                                match breakpoint_value {
                                    Some(v) => self.breakpoint.insert(row_index, !(v)),
                                    None => self.breakpoint.insert(row_index, true),
                                };
                            }
                        });
                        row.col(|ui| {
                            ui.label(format!("{:04X}", row_data.0));
                        });
                        row.col(|ui| {
                            ui.label(&row_data.1);
                        });
                    });
                });
        });
    }
}

pub fn format_slider_hex<'a>(zeros: u8) -> impl 'a + Fn(f64, RangeInclusive<usize>) -> String {
    move |n: f64, _: RangeInclusive<usize>| {
        let n = n as u16;
        if zeros == 4 {
            return format!("{:#04X}", n);
        }
        format!("{:#06X}", n)
    }
}
