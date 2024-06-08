#![warn(clippy::all, rust_2018_idioms)]

mod app;
pub mod bus;
pub mod mos6502;
pub mod opcodes;
pub use app::Mos6502Emulator;
