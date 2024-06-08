use egui::{FontFamily, FontId, Frame, Layout, Margin, RichText, TextStyle};
use egui_extras::{Column, TableBuilder};
use std::ops::RangeInclusive;

use crate::{
    bus::Bus,
    mos6502::{Flags, MOS6502, STACK_TOP},
    opcodes,
};

#[derive(serde::Deserialize, serde::Serialize)]
#[serde(default)]
pub struct Mos6502Emulator {
    #[serde(skip)]
    bus: Bus,

    #[serde(skip)]
    deassembly: Vec<(usize, String)>,

    #[serde(skip)]
    clock: std::time::Duration,

    #[serde(skip)]
    last_time: std::time::Instant,

    #[serde(skip)]
    is_running: bool,

    #[serde(skip)]
    mos6502: MOS6502,

    #[serde(skip)]
    ram_row: [u8; 54],

    #[serde(skip)]
    memory_editor: egui_memory_editor::MemoryEditor,
}

impl Default for Mos6502Emulator {
    fn default() -> Self {
        let mut memory_editor = egui_memory_editor::MemoryEditor::new();
        memory_editor.set_address_range("All", 0..0xFFFF);
        Self {
            bus: Bus::new(Vec::new()),
            deassembly: Vec::new(),
            clock: std::time::Duration::from_millis(200),
            last_time: std::time::Instant::now(),
            is_running: false,
            mos6502: MOS6502::new(),
            ram_row: [' ' as u8; 54],
            memory_editor,
        }
    }
}

impl Mos6502Emulator {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        cc.egui_ctx.style_mut(|style| {
            style.override_font_id = Some(FontId::new(18.0, FontFamily::Proportional));
        });

        if let Some(storage) = cc.storage {
            return eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default();
        }

        let a = Self::default();
        a
    }
}

impl eframe::App for Mos6502Emulator {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if self.is_running {
            ctx.request_repaint();
            let now = std::time::Instant::now();
            if now.duration_since(self.last_time) >= self.clock {
                self.last_time = now;
                self.mos6502.clock(&mut self.bus);
            }
        }

        egui::TopBottomPanel::top("top_panel")
            .frame(Frame::default().inner_margin(Margin::symmetric(14.0, 8.0)))
            .show(ctx, |ui| {
                ui.with_layout(Layout::left_to_right(egui::Align::TOP), |ui| {
                    if ui.button(RichText::new("Load ROM").size(16.0)).clicked() {
                        let src = [
                            0xa2, 0x00, 0xa0, 0x00, 0x8a, 0x99, 0x00, 0x02, 0x48, 0xe8, 0xc8, 0xc0,
                            0x10, 0xd0, 0xf5, 0x68, 0x99, 0x00, 0x02, 0xc8, 0xc0, 0x20, 0xd0, 0xf7,
                        ];

                        self.bus.rom = src.to_vec();

                        self.deassembly = opcodes::deassembly(&mut self.bus);

                        self.last_time = std::time::Instant::now();
                        self.is_running = true;

                        self.mos6502.reset(&mut self.bus);
                    }
                    ui.add_space(16.0);

                    if ui.button("▶").clicked() {}

                    if ui.button("⏸").clicked() {}

                    if ui.button("↺").clicked() {}

                    ui.add_space(16.0);

                    // Up
                    if ui.button("⏶").clicked() {}

                    // Down
                    if ui.button("⏷").clicked() {}

                    if self.is_running {
                        ui.label(format!(
                            "Clock: {} Hz",
                            1f64 / self.clock.as_millis() as f64
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
                            egui::Slider::new(&mut self.mos6502.pc, 0x0..=0xFFFF)
                                .custom_formatter(format_slider_hex(6))
                                .prefix("PC: "),
                        );
                        ui.add(
                            egui::Slider::new(&mut self.mos6502.ac, 0x0..=0xFF)
                                .custom_formatter(format_slider_hex(4))
                                .prefix("A: "),
                        );
                        ui.add(
                            egui::Slider::new(&mut self.mos6502.x, 0x0..=0xFF)
                                .custom_formatter(format_slider_hex(4))
                                .prefix("X: "),
                        );
                        ui.add(
                            egui::Slider::new(&mut self.mos6502.y, 0x0..=0xFF)
                                .custom_formatter(format_slider_hex(4))
                                .prefix("Y: "),
                        );
                        ui.add(
                            egui::Slider::new(&mut self.mos6502.stack_index, 0x0..=0xFF)
                                .custom_formatter(format_slider_hex(4))
                                .prefix("SP: "),
                        );

                        ui.add_space(10.0);

                        ui.horizontal(|ui| {
                            if ui
                                .add(egui::SelectableLabel::new(
                                    self.mos6502.get_flag(Flags::N) != 0,
                                    "N",
                                ))
                                .clicked()
                            {
                                self.mos6502
                                    .set_flag(Flags::N, !(self.mos6502.get_flag(Flags::N) != 0));
                            }
                            ui.add(egui::SelectableLabel::new(
                                self.mos6502.get_flag(Flags::V) != 0,
                                "V",
                            ));
                            ui.add(egui::SelectableLabel::new(
                                self.mos6502.get_flag(Flags::B) != 0,
                                "B",
                            ));
                            ui.add(egui::SelectableLabel::new(
                                self.mos6502.get_flag(Flags::D) != 0,
                                "D",
                            ));
                            ui.add(egui::SelectableLabel::new(
                                self.mos6502.get_flag(Flags::I) != 0,
                                "I",
                            ));
                            ui.add(egui::SelectableLabel::new(
                                self.mos6502.get_flag(Flags::Z) != 0,
                                "Z",
                            ));
                            ui.add(egui::SelectableLabel::new(
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
                                            ui.label(format!("{:04X}", STACK_TOP + 0xFF - i));
                                        });
                                        row.col(|ui| {
                                            ui.label(format!(
                                                "{:#04X}",
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
                    ui.add_space(18.0);

                    if self.is_running {
                        self.memory_editor.draw_editor_contents_read_only(
                            ui,
                            &mut self.bus,
                            |bus, address| Some(bus.ram[address]),
                        );
                    }
                });
            });

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.label(RichText::new("Disassembly").heading().strong().size(26.0));
            ui.add_space(18.0);

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
                .body(|mut body| {
                    for (address, instruction) in self.deassembly.iter() {
                        body.row(20.0, |mut row| {
                            if self.mos6502.pc as usize == *address {
                                row.set_selected(true);
                            }
                            row.col(|ui| if ui.radio(false, "").clicked() {});
                            row.col(|ui| {
                                ui.label(format!("{:06X}", address));
                            });
                            row.col(|ui| {
                                ui.label(instruction);
                            });
                        });
                    }
                });
        });
    }
}

pub fn format_slider_hex<'a>(zeros: u8) -> impl 'a + Fn(f64, RangeInclusive<usize>) -> String {
    return move |n: f64, _: RangeInclusive<usize>| {
        let n = n as u16;
        if zeros == 4 {
            return format!("{:#04X}", n);
        }
        format!("{:#06X}", n)
    };
}
