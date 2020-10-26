use sljit_rs::{definition::*, *};
fn main() {
    let comp = SlJITCompiler::new();

    comp.set_context(
        0,
        sljit_arg1(SLJIT_ARG_TYPE_SW) | sljit_arg2(SLJIT_ARG_TYPE_SW),
        2,
        2,
        0,
        0,
        0,
    );
    comp.emit_op0(SLJIT_ENDBR);
    comp.emit_fast_enter(SLJIT_R0, 0);
    comp.emit_op2(SLJIT_ADD, SLJIT_R0, 0, SLJIT_S0, 0, SLJIT_S1, 0);
    comp.emit_op_src(SLJIT_FAST_RETURN, SLJIT_R0, 0);
    unsafe {
        let code = comp.generate_code();

        let func: extern "C" fn(i64, i64) -> i64;
        func = std::mem::transmute(code);
        println!("{}", func(2, 3));
    }
}
