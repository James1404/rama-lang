use itertools::Itertools;
use typed_index_collections::{TiVec, ti_vec};

use crate::{
    metadata::Metadata,
    tir::{CFG, CmpKind, Instruction, Loc, Ref, TIR},
    types::{FloatKind, IntSize, Type, TypeID},
};
use std::{fmt::Write, io, path::Path};

struct CodeBuilder<'a> {
    tir: TIR<'a>,
    code: String,
}

impl<'a> CodeBuilder<'a> {
    fn new(tir: TIR<'a>) -> Self {
        Self {
            tir,
            code: String::new(),
        }
    }

    fn loc(loc: Loc) -> String {
        format!("bb{}", loc.0)
    }

    fn reg(reg: Ref) -> String {
        format!("_{}", reg.0)
    }

    fn arg(idx: usize) -> String {
        format!("arg{}", idx)
    }

    fn field(idx: usize) -> String {
        format!("field{}", idx)
    }

    fn convert_type(&self, ty: TypeID) -> String {
        match self.tir.ctx.get(ty) {
            Type::Void => "void".to_owned(),
            Type::Unit => "int8_t".to_owned(),
            Type::Bool => "bool".to_owned(),
            Type::Int { size, signed } => match (signed, size) {
                (true, IntSize::Bits8) => "int8_t".to_owned(),
                (true, IntSize::Bits16) => "int16_t".to_owned(),
                (true, IntSize::Bits32) => "int32_t".to_owned(),
                (true, IntSize::Bits64) => "int64_t".to_owned(),
                (true, IntSize::BitsPtr) => "intptr_t".to_owned(),
                (false, IntSize::Bits8) => "uint8_t".to_owned(),
                (false, IntSize::Bits16) => "uint16_t".to_owned(),
                (false, IntSize::Bits32) => "uint32_t".to_owned(),
                (false, IntSize::Bits64) => "uint64_t".to_owned(),
                (false, IntSize::BitsPtr) => "uintptr_t".to_owned(),
            },
            Type::Float(FloatKind::F32) => "float".to_owned(),
            Type::Float(FloatKind::F64) => "long double".to_owned(),
            Type::Ptr(inner) => format!("{}*", self.convert_type(inner)),
            _ => panic!(),
        }
    }

    fn import_std(&mut self, file: &str) {
        writeln!(self.code, "#import <{}>", file).unwrap()
    }

    fn eval_instruction(&mut self, inst: &Instruction<'a>) -> std::fmt::Result {
        match inst {
            Instruction::Nop => panic!(),

            Instruction::Add { dest, lhs, rhs } => {
                let lhs = Self::reg(*lhs);
                let rhs = Self::reg(*rhs);
                let ty = "TODO";

                writeln!(self.code, "{ty} {dest} = {lhs} + {rhs}")
            }
            Instruction::Sub { dest, lhs, rhs } => {
                let lhs = Self::reg(*lhs);
                let rhs = Self::reg(*rhs);
                let ty = "TODO";

                writeln!(self.code, "{ty} {dest} = {lhs} - {rhs}")
            }
            Instruction::Mul { dest, lhs, rhs } => {
                let lhs = Self::reg(*lhs);
                let rhs = Self::reg(*rhs);
                let ty = "TODO";

                writeln!(self.code, "{ty} {dest} = {lhs} * {rhs}")
            }
            Instruction::Div { dest, lhs, rhs } => {
                let lhs = Self::reg(*lhs);
                let rhs = Self::reg(*rhs);
                let ty = "TODO";

                writeln!(self.code, "{ty} {dest} = {lhs} / {rhs}")
            }

            Instruction::Cmp {
                dest,
                lhs,
                rhs,
                ty: _,
                kind,
            } => {
                let l = Self::reg(*lhs);
                let r = Self::reg(*rhs);

                let cmp = match kind {
                    CmpKind::Equal => "==",
                    CmpKind::NotEqual => "!=",
                    CmpKind::LessThan => "<",
                    CmpKind::LessEqual => "<=",
                    CmpKind::GreaterThan => ">",
                    CmpKind::GreaterEqual => ">=",
                };

                writeln!(self.code, "bool {} = {l} {cmp} {r};", Self::reg(*dest),)
            }
            Instruction::Negate { dest, value } => todo!(),
            Instruction::Not { .. } => todo!(),
            Instruction::MakeVar { dest, value, ty } => {
                let ty = self.convert_type(*ty);
                writeln!(
                    self.code,
                    "{ty} {} = {};",
                    Self::reg(*dest),
                    Self::reg(*value)
                )
            }
            Instruction::ReadVar { dest, var } => {
                writeln!(self.code, "{} = {};", Self::reg(*dest), Self::reg(*var))
            }
            Instruction::WriteVar { var, value } => {
                writeln!(self.code, "{} = {};", Self::reg(*var), Self::reg(*value))
            }
            Instruction::Ref { .. } => todo!(),
            Instruction::Deref { .. } => todo!(),
            Instruction::Cast {
                dest,
                value,
                from,
                to,
            } => {
                let from = self.convert_type(*to);
                let to = self.convert_type(*to);
                writeln!(
                    self.code,
                    "{to} {} = ({from}){};",
                    Self::reg(*dest),
                    Self::reg(*value)
                )
            }
            Instruction::MakeStruct { dest, ty } => todo!(),
            Instruction::WriteField {
                r#struct,
                field,
                value,
                ty,
            } => todo!(),
            Instruction::ReadField {
                dest,
                r#struct,
                field,
                ty,
            } => {
                let ty = ""; // TODO
                writeln!(
                    self.code,
                    "{} {} = &{}.{};",
                    ty,
                    Self::reg(*dest),
                    Self::reg(*r#struct),
                    Self::field(*field)
                )
            }

            Instruction::ReadArg { dest, index } => {
                let ty = ""; // TODO
                writeln!(
                    self.code,
                    "{} {} = {}",
                    ty,
                    Self::reg(*dest),
                    Self::arg(*index)
                )
            }

            Instruction::Call { dest, func, args } => {
                let ty = self.convert_type(self.tir.get_func(*func).ty);
                let name = self.tir.funcs[*func].name;

                write!(self.code, "{} {} = {}(", ty, Self::reg(*dest), name)?;

                let mut iter = args.iter().peekable();
                while let Some(arg) = iter.next() {
                    write!(self.code, "{}", Self::reg(*arg))?;

                    if iter.peek().is_some() {
                        write!(self.code, ", ")?;
                    }
                }

                writeln!(self.code, ");")
            }
            Instruction::Integer { dest, value, ty } => writeln!(
                self.code,
                "{} {} = {value};",
                self.convert_type(*ty),
                Self::reg(*dest)
            ),
            Instruction::Float { dest, value, ty } => writeln!(
                self.code,
                "{} {} = {value};",
                self.convert_type(*ty),
                Self::reg(*dest)
            ),
            Instruction::Bool { dest, value } => {
                writeln!(self.code, "bool {} = {value};", Self::reg(*dest))
            }

            Instruction::String { dest, value } => {
                writeln!(self.code, "const char* {} = \"{value}\";", Self::reg(*dest))
            }

            Instruction::Unit { dest } => {
                writeln!(self.code, "int8_t {} = 0", Self::reg(*dest))
            }

            Instruction::Goto(loc) => writeln!(self.code, "goto {};", Self::loc(*loc)),
            Instruction::If { cond, t, f } => writeln!(
                self.code,
                "if ({}) goto {} else goto {}",
                Self::reg(*cond),
                Self::loc(*t),
                Self::loc(*f)
            ),

            Instruction::ReturnNone => writeln!(self.code, "return;"),
            Instruction::Return(value) => writeln!(self.code, "return {};", Self::reg(*value)),
        }
    }

    fn eval_cfg(&mut self, cfg: CFG<'a>) -> std::fmt::Result {
        for (loc, bb) in cfg.blocks.iter_enumerated() {
            writeln!(self.code, "{}:", Self::loc(loc))?;

            for inst in &bb.instructions {
                write!(self.code, "\t")?;
                self.eval_instruction(inst)?;
            }
        }

        Ok(())
    }

    fn build(mut self) -> String {
        self.import_std("assert.h");
        self.import_std("stdio.h");
        self.import_std("stdlib.h");
        self.import_std("stdbool.h");
        self.import_std("string.h");

        for func in self.tir.func_iter() {
            let ty = match self.tir.ctx.get(func.ty) {
                Type::Fn(ty) => ty,
                _ => panic!(),
            };
            let return_ty = self.convert_type(ty.return_ty);
            write!(self.code, "{} {}(", return_ty, func.name).unwrap();

            let mut iter = ty.parameters.iter().enumerate().peekable();
            while let Some((idx, param)) = iter.next() {
                write!(
                    self.code,
                    "{} {}",
                    Self::arg(idx),
                    self.convert_type(param.1)
                )
                .unwrap();

                if iter.peek().is_some() {
                    write!(self.code, ", ").unwrap();
                }
            }

            writeln!(self.code, ");").unwrap();
        }

        for func in self.tir.clone().func_iter() {
            let ty = match self.tir.ctx.get(func.ty) {
                Type::Fn(ty) => ty,
                _ => panic!(),
            };
            let return_ty = self.convert_type(ty.return_ty);
            write!(self.code, "{} {}(", return_ty, func.name).unwrap();

            let mut iter = ty.parameters.iter().enumerate().peekable();
            while let Some((idx, param)) = iter.next() {
                write!(
                    self.code,
                    "{} {}",
                    Self::arg(idx),
                    self.convert_type(param.1)
                )
                .unwrap();

                if iter.peek().is_some() {
                    write!(self.code, ", ").unwrap();
                }
            }

            writeln!(self.code, ") {{").unwrap();
            self.eval_cfg(func.cfg).expect("Error");
            writeln!(self.code, "}}").unwrap();
        }

        self.code
    }
}

pub fn compile<'a>(tir: TIR<'a>, metadata: Metadata<'a>) -> io::Result<()> {
    let code = CodeBuilder::new(tir);
    let code = code.build();

    println!("{}", code);

    let path = format!("build/{}", metadata.filename);
    let path = Path::new(&path);

    let dir = path.parent().unwrap();
    std::fs::create_dir_all(dir)?;

    let mut file = std::fs::File::create(path)?;
    io::Write::write_all(&mut file, code.as_bytes())?;

    Ok(())
}
