use crate::typed_ast::TypedAST;

extern crate llvm_sys as llvm;

pub struct Codegen<'a> {
    ast: TypedAST<'a>,

    context: *mut llvm::LLVMContext,
    module: *mut llvm::LLVMModule,
    builder: *mut llvm::LLVMBuilder,
}

impl<'a> Drop for Codegen<'a> {
    fn drop(&mut self) {
        unsafe {
            llvm::core::LLVMDisposeBuilder(self.builder);
            llvm::core::LLVMDisposeModule(self.module);
            llvm::core::LLVMContextDispose(self.context);
        }
    }
}

impl<'a> Codegen<'a> {
    pub fn new(ast: TypedAST<'a>) -> Self {
        let context = unsafe { llvm::core::LLVMContextCreate() };
        let module =
            unsafe { llvm::core::LLVMModuleCreateWithName(b"test\0".as_ptr() as *const _) };
        let builder = unsafe { llvm::core::LLVMCreateBuilderInContext(context) };

        Self {
            ast,
            context,
            module,
            builder,
        }
    }

    pub fn run(mut self) {
        unsafe { llvm::core::LLVMDumpModule(self.module) };
    }
}
