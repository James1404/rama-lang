use crate::{
    metadata::Metadata,
    tir::{CFG, CmpKind, Instruction, Loc, Ref, TIR},
    types::{FloatKind, IntSize, Type, TypeID},
};
use core::fmt;
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
            Type::Unit => "i32".to_owned(),
            Type::Bool => "bool".to_owned(),
            Type::Int { size, signed } => match (signed, size) {
                (true, IntSize::Bits8) => "i8".to_owned(),
                (true, IntSize::Bits16) => "i16".to_owned(),
                (true, IntSize::Bits32) => "i32".to_owned(),
                (true, IntSize::Bits64) => "i64".to_owned(),
                (true, IntSize::BitsPtr) => "isize".to_owned(),
                (false, IntSize::Bits8) => "u8".to_owned(),
                (false, IntSize::Bits16) => "u16".to_owned(),
                (false, IntSize::Bits32) => "u32".to_owned(),
                (false, IntSize::Bits64) => "u64".to_owned(),
                (false, IntSize::BitsPtr) => "usize".to_owned(),
            },
            Type::Float(FloatKind::F32) => "f32".to_owned(),
            Type::Float(FloatKind::F64) => "f64".to_owned(),
            Type::Ptr(inner) => format!("{}*", self.convert_type(inner)),
            _ => panic!("{}", self.tir.ctx.display(ty)),
        }
    }

    fn include_file(&mut self, file: &str) -> fmt::Result {
        writeln!(self.code, "#include \"{}\"", file)
    }

    fn include_std(&mut self, file: &str) -> fmt::Result {
        writeln!(self.code, "#include <{}>", file)
    }

    fn define(&mut self, name: &str, value: &str) -> fmt::Result {
        writeln!(self.code, "#define {} {}", name, value)
    }

    fn typedef(&mut self, name: &str, ty: &str) -> fmt::Result {
        writeln!(self.code, "typedef {} {};", ty, name)
    }

    fn eval_instruction(&mut self, inst: &Instruction<'a>) -> fmt::Result {
        match inst {
            Instruction::Nop => panic!(),

            Instruction::Add { dest, lhs, rhs, ty } => {
                let dest = Self::reg(*dest);
                let lhs = Self::reg(*lhs);
                let rhs = Self::reg(*rhs);
                let ty = self.convert_type(*ty);

                writeln!(self.code, "{ty} {dest} = {lhs} + {rhs};")
            }
            Instruction::Sub { dest, lhs, rhs, ty } => {
                let dest = Self::reg(*dest);
                let lhs = Self::reg(*lhs);
                let rhs = Self::reg(*rhs);
                let ty = self.convert_type(*ty);

                writeln!(self.code, "{ty} {dest} = {lhs} - {rhs};")
            }
            Instruction::Mul { dest, lhs, rhs,ty } => {
                let dest = Self::reg(*dest);
                let lhs = Self::reg(*lhs);
                let rhs = Self::reg(*rhs);
                let ty = self.convert_type(*ty);

                writeln!(self.code, "{ty} {dest} = {lhs} * {rhs};")
            }
            Instruction::Div { dest, lhs, rhs,ty } => {
                let dest = Self::reg(*dest);
                let lhs = Self::reg(*lhs);
                let rhs = Self::reg(*rhs);
                let ty = self.convert_type(*ty);

                writeln!(self.code, "{ty} {dest} = {lhs} / {rhs};")
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

                writeln!(self.code, "bool {} = {l} {cmp} {r};", Self::reg(*dest))
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
            Instruction::MakeStruct { dest, ty } => {
                let ty = self.convert_type(*ty);
                writeln!(self.code, "{ty} {};", Self::reg(*dest))
            }
            Instruction::WriteField {
                r#struct,
                field,
                value,
                ty: _,
            } => writeln!(
                self.code,
                "{}.{} = {};",
                Self::reg(*r#struct),
                Self::field(*field),
                Self::reg(*value)
            ),

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
                writeln!(self.code, "i32 {} = 0;", Self::reg(*dest))
            }

            Instruction::Goto(loc) => writeln!(self.code, "goto {};", Self::loc(*loc)),
            Instruction::Goto_if { cond, loc } => writeln!(self.code, "if ({}) goto {};", Self::reg(*cond), Self::loc(*loc)),
            Instruction::Goto_if_not { cond, loc } => writeln!(self.code, "if (!{}) goto {};", Self::reg(*cond), Self::loc(*loc)),

            Instruction::ReturnNone => writeln!(self.code, "return;"),
            Instruction::Return(value) => writeln!(self.code, "return {};", Self::reg(*value)),
        }
    }

    fn newline(&mut self) -> fmt::Result {
        writeln!(self.code, "")
    }

    fn eval_cfg(&mut self, cfg: CFG<'a>) -> fmt::Result {
        for (loc, bb) in cfg.blocks.iter_enumerated() {
            writeln!(self.code, "{}:", Self::loc(loc))?;
            writeln!(self.code, "\t{{}}")?;

            for inst in &bb.instructions {
                write!(self.code, "\t")?;
                self.eval_instruction(inst)?;
            }
        }

        Ok(())
    }

    fn build(mut self) -> Result<String, fmt::Error> {
        self.include_std("assert.h")?;
        self.include_std("stdio.h")?;
        self.include_std("stdlib.h")?;
        self.include_std("stdbool.h")?;
        self.include_std("stdint.h")?;
        self.include_std("string.h")?;

        self.newline()?;

        self.typedef("f32", "float")?;
        self.typedef("f64", "long double")?;

        self.newline()?;

        self.typedef("u8", "unsigned char")?;
        self.typedef("u16", "unsigned short int")?;
        self.typedef("u32", "unsigned long int")?;
        self.typedef("u64", "unsigned long long int")?;
        self.typedef("usize", "unsigned long long int")?;

        self.newline()?;

        self.typedef("i8", "char")?;
        self.typedef("i16", "short int")?;
        self.typedef("i32", "long int")?;
        self.typedef("i64", "long long int")?;
        self.typedef("isize", "long long int")?;

        self.newline()?;

        self.define("TODO", "i32")?;

        self.newline()?;

        //
        // forward declerations
        //

        for func in self.tir.func_iter() {
            let ty = match self.tir.ctx.get(func.ty) {
                Type::Fn(ty) => ty,
                _ => panic!(),
            };
            let return_ty = self.convert_type(ty.return_ty);
            write!(self.code, "{} {}(", return_ty, func.name)?;

            let mut iter = ty.parameters.iter().enumerate().peekable();
            while let Some((idx, param)) = iter.next() {
                write!(
                    self.code,
                    "{} {}",
                    Self::arg(idx),
                    self.convert_type(param.1)
                )?;

                if iter.peek().is_some() {
                    write!(self.code, ", ")?;
                }
            }

            writeln!(self.code, ");")?;
        }

        self.newline()?;

        for func in self.tir.clone().func_iter() {
            let ty = match self.tir.ctx.get(func.ty) {
                Type::Fn(ty) => ty,
                _ => panic!(),
            };
            let return_ty = self.convert_type(ty.return_ty);
            write!(self.code, "{} {}(", return_ty, func.name)?;

            let mut iter = ty.parameters.iter().enumerate().peekable();
            while let Some((idx, param)) = iter.next() {
                write!(
                    self.code,
                    "{} {}",
                    Self::arg(idx),
                    self.convert_type(param.1)
                )?;

                if iter.peek().is_some() {
                    write!(self.code, ", ")?;
                }
            }

            writeln!(self.code, ") {{")?;
            self.eval_cfg(func.cfg)?;
            writeln!(self.code, "}}")?;
        }

        Ok(self.code)
    }
}

pub fn compile<'a>(tir: TIR<'a>, metadata: Metadata<'a>) -> io::Result<()> {
    let code = CodeBuilder::new(tir);
    let code = code.build().unwrap();

    println!("{}", code);

    let path = format!("build/{}.c", metadata.name);
    let path = Path::new(&path);

    let dir = path.parent().unwrap();
    std::fs::create_dir_all(dir)?;

    let mut file = std::fs::File::create(path)?;
    io::Write::write_all(&mut file, code.as_bytes())?;

    Ok(())
}
