use typed_index_collections::{TiVec, ti_vec};

use crate::{
    metadata::Metadata,
    tir::{CFG, Instruction, Loc, Ref, TIR},
    types::{FloatKind, IntSize, Type, TypeID},
};
use std::{fmt::Write, io, path::Path};

struct CFGMapping {
    data: TiVec<Ref, String>,
    args: Vec<String>,
}

impl CFGMapping {
    fn new() -> Self {
        Self {
            data: ti_vec![],
            args: vec![],
        }
    }

    fn push(&mut self, value: String) {
        self.data.push(value);
    }

    fn get(&mut self, value: Ref) -> String {
        self.data[value].clone()
    }
}

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

    fn convert_type(&self, ty: TypeID) -> String {
        match self.tir.ctx.get(ty) {
            Type::Void => "void".to_owned(),
            Type::Unit => "unit".to_owned(),
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

    fn eval_cfg(&mut self, mapping: &mut CFGMapping, cfg: CFG<'a>) {
        for bb in cfg.blocks.iter() {}
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

            let mut iter = ty.parameters.iter().peekable();
            while let Some(param) = iter.next() {
                write!(self.code, "{} {}", param.0, self.convert_type(param.1)).unwrap();

                if iter.peek().is_some() {
                    write!(self.code, ", ").unwrap();
                }
            }

            writeln!(self.code, ");").unwrap();
        }

        for func in self.tir.clone().func_iter() {
            let mut mapping = CFGMapping::new();
            self.eval_cfg(&mut mapping, func.cfg);
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
