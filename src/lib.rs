#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_parens)]
#![allow(non_snake_case)]
pub use raw_bindings::{
    sljit_s16, sljit_s32, sljit_s8, sljit_sw, sljit_u16, sljit_u32, sljit_u8, sljit_uw,
    SLJIT_NUMBER_OF_FLOAT_REGISTERS, SLJIT_NUMBER_OF_REGISTERS,
    SLJIT_NUMBER_OF_SAVED_FLOAT_REGISTERS, SLJIT_NUMBER_OF_SAVED_REGISTERS,
};
pub mod raw_bindings;
pub mod definition {
    use super::raw_bindings::*;

    pub struct SlJITCompiler {
        raw: *mut sljit_compiler,
    }

    impl SlJITCompiler {
        pub fn new() -> Self {
            Self {
                raw: unsafe { sljit_create_compiler(core::ptr::null_mut(), core::ptr::null_mut()) },
            }
        }
        pub fn emit_op0(&self, op: sljit_s32) -> sljit_s32 {
            unsafe { sljit_emit_op0(self.raw, op) }
        }
        #[inline]
        pub fn get_compiler_error(&self) -> sljit_s32 {
            unsafe {
                extern "C" {
                    fn sljit_get_compiler_error(raw: *mut sljit_compiler) -> sljit_s32;
                }

                sljit_get_compiler_error(self.raw)
            }
        }
        /// Create executable code from the sljit instruction stream. This is the final step
        ///of the code generation so no more instructions can be added after this call.
        #[inline]
        pub unsafe fn generate_code(&self) -> *mut u8 {
            {
                sljit_generate_code(self.raw).cast()
            }
        }
        /// Free executable code
        #[inline]
        pub unsafe fn free_code(code: *mut u8) {
            {
                sljit_free_code(code.cast(), core::ptr::null_mut())
            }
        }
        ///When the protected executable allocator is used the JIT code is mapped
        ///twice. The first mapping has read/write and the second mapping has read/exec
        ///permissions. This function returns with the relative offset of the executable
        ///mapping using the writable mapping as the base after the machine code is
        ///successfully generated. The returned value is always 0 for the normal executable
        ///allocator, since it uses only one mapping with read/write/exec permissions.
        ///Dynamic code modifications requires this value.
        ///Before a successful code generation, this function returns with 0.
        pub fn get_executable_offset(&self) -> sljit_sw {
            unsafe {
                extern "C" {
                    fn sljit_get_executable_offset(_: *mut sljit_compiler) -> sljit_sw;
                };
                sljit_get_executable_offset(self.raw)
            }
        }
        /// The executable memory consumption of the generated code can be retrieved by
        ///this function. The returned value can be used for statistical purposes.
        ///Before a successful code generation, this function returns with 0.
        pub fn get_generated_code_size(&self) -> usize {
            unsafe {
                extern "C" {
                    fn sljit_get_generated_code_size(_: *mut sljit_compiler) -> sljit_uw;
                };
                sljit_get_generated_code_size(self.raw) as _
            }
        }

        pub fn emit_enter(
            &self,
            options: sljit_s32,
            arg_types: sljit_s32,
            scratches: sljit_s32,
            saveds: sljit_s32,
            fscratches: sljit_s32,
            fsaveds: sljit_s32,
            local_size: sljit_s32,
        ) -> sljit_s32 {
            unsafe {
                sljit_emit_enter(
                    self.raw, options, arg_types, scratches, saveds, fscratches, fsaveds,
                    local_size,
                )
            }
        }
        /// The machine code has a context (which contains the local stack space size,
        /// number of used registers, etc.) which initialized by sljit_emit_enter. Several
        /// functions (like sljit_emit_return) requres this context to be able to generate
        /// the appropriate code. However, some code fragments (like inline cache) may have
        /// no normal entry point so their context is unknown for the compiler. Their context
        /// can be provided to the compiler by the sljit_set_context function.
        ///
        /// Note: every call of sljit_emit_enter and sljit_set_context overwrites
        ///     the previous context.
        pub fn set_context(
            &self,
            options: sljit_s32,
            arg_types: sljit_s32,
            scratches: sljit_s32,
            saveds: sljit_s32,
            fscratches: sljit_s32,
            fsaveds: sljit_s32,
            local_size: sljit_s32,
        ) -> sljit_s32 {
            unsafe {
                sljit_set_context(
                    self.raw, options, arg_types, scratches, saveds, fscratches, fsaveds,
                    local_size,
                )
            }
        }

        pub fn emit_return(&self, op: sljit_s32, src: sljit_s32, srcw: sljit_sw) -> sljit_s32 {
            unsafe { sljit_emit_return(self.raw, op, src, srcw) }
        }

        pub fn emit_fast_enter(&self, dst: sljit_s32, dstw: sljit_sw) -> sljit_s32 {
            unsafe { sljit_emit_fast_enter(self.raw, dst, dstw) }
        }

        pub fn emit_op1(
            &self,
            op: sljit_s32,
            dst: sljit_s32,
            dstw: sljit_sw,
            src: sljit_s32,
            srcw: sljit_sw,
        ) -> sljit_s32 {
            unsafe { sljit_emit_op1(self.raw, op, dst, dstw, src, srcw) }
        }

        pub fn emit_op2(
            &self,
            op: sljit_s32,
            dst: sljit_s32,
            dstw: sljit_sw,
            src1: sljit_s32,
            src1w: sljit_sw,
            src2: sljit_s32,
            src2w: sljit_sw,
        ) -> sljit_s32 {
            unsafe { sljit_emit_op2(self.raw, op, dst, dstw, src1, src1w, src2, src2w) }
        }

        pub fn emit_op_src(&self, op: sljit_s32, src: sljit_s32, srcw: sljit_sw) -> sljit_s32 {
            unsafe { sljit_emit_op_src(self.raw, op, src, srcw) }
        }

        pub fn emit_fop1(
            &self,
            op: sljit_s32,
            dst: sljit_s32,
            dstw: sljit_sw,
            src: sljit_s32,
            srcw: sljit_sw,
        ) -> sljit_s32 {
            unsafe { sljit_emit_fop1(self.raw, op, dst, dstw, src, srcw) }
        }
        pub fn emit_fop2(
            &self,
            op: sljit_s32,
            dst: sljit_s32,
            dstw: sljit_sw,
            src1: sljit_s32,
            src1w: sljit_sw,
            src2: sljit_s32,
            src2w: sljit_sw,
        ) -> sljit_s32 {
            unsafe { sljit_emit_fop2(self.raw, op, dst, dstw, src1, src1w, src2, src2w) }
        }

        pub fn emit_label(&self) -> SlJITLabel {
            SlJITLabel {
                label: unsafe { sljit_emit_label(self.raw) },
            }
        }

        pub fn emit_jump(&self, ty: sljit_s32) -> SlJITJump {
            unsafe {
                SlJITJump {
                    jump: sljit_emit_jump(self.raw, ty),
                }
            }
        }

        pub fn emit_call(&self, type_: sljit_s32, arg_types: sljit_s32) -> SlJITJump {
            unsafe {
                SlJITJump {
                    jump: sljit_emit_call(self.raw, type_, arg_types),
                }
            }
        }

        pub fn emit_cmp(
            &self,
            type_: sljit_s32,
            src1: sljit_s32,
            src1w: sljit_sw,
            src2: sljit_s32,
            src2w: sljit_sw,
        ) -> SlJITJump {
            unsafe {
                SlJITJump {
                    jump: sljit_emit_cmp(self.raw, type_, src1, src1w, src2, src2w),
                }
            }
        }

        pub fn emit_fcmp(
            &self,
            type_: sljit_s32,
            src1: sljit_s32,
            src1w: sljit_sw,
            src2: sljit_s32,
            src2w: sljit_sw,
        ) -> SlJITJump {
            unsafe {
                SlJITJump {
                    jump: sljit_emit_fcmp(self.raw, type_, src1, src1w, src2, src2w),
                }
            }
        }

        pub fn emit_ijump(&self, type_: sljit_s32, src: sljit_s32, srcw: sljit_sw) -> sljit_s32 {
            unsafe { sljit_emit_ijump(self.raw, type_, src, srcw) }
        }
        pub fn emit_icall(
            &self,
            type_: sljit_s32,
            arg_types: sljit_s32,
            src: sljit_s32,
            srcw: sljit_sw,
        ) -> sljit_s32 {
            unsafe { sljit_emit_icall(self.raw, type_, arg_types, src, srcw) }
        }

        pub fn emit_op_flags(
            &self,
            op: sljit_s32,
            dst: sljit_s32,
            dstw: sljit_sw,
            type_: sljit_s32,
        ) -> sljit_s32 {
            unsafe { sljit_emit_op_flags(self.raw, op, dst, dstw, type_) }
        }

        pub fn emit_cmov(
            &self,
            type_: sljit_s32,
            dst_reg: sljit_s32,
            src: sljit_s32,
            srcw: sljit_sw,
        ) -> sljit_s32 {
            unsafe { sljit_emit_cmov(self.raw, type_, dst_reg, src, srcw) }
        }

        pub fn emit_mem(
            &self,
            type_: sljit_s32,
            reg: sljit_s32,
            mem: sljit_s32,
            memw: sljit_sw,
        ) -> sljit_s32 {
            unsafe { sljit_emit_mem(self.raw, type_, reg, mem, memw) }
        }
        pub fn emit_fmem(
            &self,
            type_: sljit_s32,
            reg: sljit_s32,
            mem: sljit_s32,
            memw: sljit_sw,
        ) -> sljit_s32 {
            unsafe { sljit_emit_fmem(self.raw, type_, reg, mem, memw) }
        }

        pub fn get_local_base(
            &self,
            dst: sljit_s32,
            dstw: sljit_sw,
            offset: sljit_sw,
        ) -> sljit_s32 {
            unsafe { sljit_get_local_base(self.raw, dst, dstw, offset) }
        }

        pub fn emit_const(
            &self,
            dst: sljit_s32,
            dstw: sljit_sw,
            init_value: sljit_sw,
        ) -> SlJITConst {
            unsafe {
                SlJITConst {
                    c: sljit_emit_const(self.raw, dst, dstw, init_value),
                }
            }
        }

        pub fn emit_put_label(&self, dst: sljit_s32, dstw: sljit_sw) -> SlJITPutLabel {
            unsafe {
                SlJITPutLabel {
                    label: sljit_emit_put_label(self.raw, dst, dstw),
                }
            }
        }

        pub fn emit_op_custom(&self, ins: &[u8]) -> sljit_s32 {
            unsafe { sljit_emit_op_custom(self.raw, ins.as_ptr() as *mut _, ins.len() as _) }
        }

        pub fn set_current_flags(&self, current_flags: sljit_s32) {
            unsafe { sljit_set_current_flags(self.raw, current_flags) }
        }
    }

    pub fn has_cpu_features(features: sljit_s32) -> bool {
        unsafe { sljit_has_cpu_feature(features) != 0 }
    }

    impl Drop for SlJITCompiler {
        fn drop(&mut self) {
            unsafe { sljit_free_compiler(self.raw) }
        }
    }
    #[derive(Copy, Clone, PartialOrd, PartialEq, Ord, Eq)]
    pub struct SlJITLabel {
        label: *mut sljit_label,
    }

    impl SlJITLabel {
        pub fn get_addr(&self) -> sljit_uw {
            unsafe { sljit_get_label_addr(self.label) }
        }
    }
    #[derive(Copy, Clone, PartialOrd, PartialEq, Ord, Eq)]
    pub struct SlJITJump {
        jump: *mut sljit_jump,
    }

    impl SlJITJump {
        pub fn set_label(&self, label: &SlJITLabel) {
            unsafe { sljit_set_label(self.jump, label.label) }
        }

        pub fn set_target(&self, target: sljit_uw) {
            unsafe { sljit_set_target(self.jump, target) }
        }

        pub fn get_addr(&self) -> sljit_uw {
            unsafe { sljit_get_jump_addr(self.jump) }
        }
    }

    #[derive(Copy, Clone, PartialOrd, PartialEq, Ord, Eq)]
    pub struct SlJITPutLabel {
        pub label: *mut sljit_put_label,
    }

    impl SlJITPutLabel {
        pub fn set_put_label(&self, label: SlJITLabel) {
            unsafe { sljit_set_put_label(self.label, label.label) }
        }
    }

    pub struct SlJITConst {
        pub c: *mut sljit_const,
    }

    impl SlJITConst {
        pub fn get_addr(&self) -> sljit_uw {
            unsafe { sljit_get_const_addr(self.c) }
        }
    }

    pub unsafe fn set_jump_addr(addr: sljit_uw, new_target: sljit_uw, executable_offset: sljit_sw) {
        {
            sljit_set_jump_addr(addr, new_target, executable_offset)
        }
    }

    pub unsafe fn set_const(addr: sljit_uw, new_constant: sljit_sw, executable_offset: sljit_sw) {
        {
            sljit_set_const(addr, new_constant, executable_offset)
        }
    }
    pub fn get_platform_name() -> &'static str {
        unsafe {
            std::ffi::CStr::from_ptr(sljit_get_platform_name())
                .to_str()
                .unwrap()
        }
    }

    pub struct SlJITStack {
        stack: *mut sljit_stack,
    }

    impl SlJITStack {
        pub fn new(start_size: sljit_uw, max_size: sljit_uw) -> Option<Self> {
            unsafe {
                let p = sljit_allocate_stack(start_size, max_size, core::ptr::null_mut());
                if p.is_null() {
                    None
                } else {
                    Some(Self { stack: p })
                }
            }
        }

        pub fn resize(&self, new_start: *mut u8) -> *mut u8 {
            unsafe { sljit_stack_resize(self.stack, new_start) }
        }
    }

    impl Drop for SlJITStack {
        fn drop(&mut self) {
            unsafe { sljit_free_stack(self.stack, core::ptr::null_mut()) }
        }
    }

    pub struct SlJIT;

    impl SlJIT {
        pub fn func_offset(func_name: *const i8) -> sljit_sw {
            func_name as sljit_sw
        }

        pub fn free_unused_memory_exec() {
            unsafe {
                sljit_free_unused_memory_exec();
            }
        }

        pub fn get_register_index(reg: sljit_s32) -> sljit_s32 {
            unsafe { sljit_get_register_index(reg) }
        }

        pub fn get_float_register_index(reg: sljit_s32) -> sljit_s32 {
            unsafe { sljit_get_float_register_index(reg) }
        }
    }
}
pub const SLJIT_R0: sljit_s32 = 1;
pub const SLJIT_R1: sljit_s32 = 2;
pub const SLJIT_R2: sljit_s32 = 3;
pub const SLJIT_R3: sljit_s32 = 4;
pub const SLJIT_R4: sljit_s32 = 5;
pub const SLJIT_R5: sljit_s32 = 6;
pub const SLJIT_R6: sljit_s32 = 7;
pub const SLJIT_R7: sljit_s32 = 8;
pub const SLJIT_R8: sljit_s32 = 9;
pub const SLJIT_R9: sljit_s32 = 10;
pub const SLJIT_UNUSED: sljit_s32 = 0;
pub const fn sljit_r(i: sljit_s32) -> sljit_s32 {
    1 + i
}

pub const SLJIT_S0: sljit_s32 = SLJIT_NUMBER_OF_REGISTERS as _;
pub const SLJIT_S1: sljit_s32 = SLJIT_S0 - 1;
pub const SLJIT_S2: sljit_s32 = SLJIT_S0 - 2;
pub const SLJIT_S3: sljit_s32 = SLJIT_S0 - 3;
pub const SLJIT_S4: sljit_s32 = SLJIT_S0 - 4;
pub const SLJIT_S5: sljit_s32 = SLJIT_S0 - 5;
pub const SLJIT_S6: sljit_s32 = SLJIT_S0 - 6;
pub const SLJIT_S7: sljit_s32 = SLJIT_S0 - 7;
pub const SLJIT_S8: sljit_s32 = SLJIT_S0 - 8;
pub const SLJIT_S9: sljit_s32 = SLJIT_S0 - 9;
pub const fn sljit_s(i: i32) -> i32 {
    SLJIT_NUMBER_OF_REGISTERS as i32 - i
}

pub fn sljit_first_saved_reg() -> sljit_s32 {
    SLJIT_S0 - SLJIT_NUMBER_OF_SAVED_REGISTERS as sljit_s32 + 1
}

pub const SLJIT_SP: sljit_s32 = SLJIT_NUMBER_OF_REGISTERS as i32 + 1;

macro_rules! c {
    ($t: ty; $($name : ident = $v: expr),*) => {
        $(
            pub const $name : $t = $v;
        )*
    };
}

c!(
    sljit_s32;
    SLJIT_RETURN_REG = SLJIT_R0,
    SLJIT_FR0 = 1,
    SLJIT_FR1 = 2,
    SLJIT_FR2 = 3,
    SLJIT_FR3 = 4,
    SLJIT_FR4 = 5,
    SLJIT_FR5 = 6
);

pub const fn sljit_fr(i: sljit_s32) -> sljit_s32 {
    1 + i
}

c!(
    sljit_s32;
    SLJIT_FS0 = SLJIT_NUMBER_OF_FLOAT_REGISTERS as sljit_s32,
    SLJIT_FS1 = SLJIT_FS0 - 1,
    SLJIT_FS2 = SLJIT_FS0 -2,
    SLJIT_FS3 = SLJIT_FS0  -3,
    SLJIT_FS4 = SLJIT_FS0 - 4,
    SLJIT_FS5 = SLJIT_FS0 - 5
);
pub const fn sljit_fs(i: sljit_s32) -> sljit_s32 {
    SLJIT_NUMBER_OF_FLOAT_REGISTERS as i32 - i
}

c!(sljit_s32;
    SLJIT_ARG_TYPE_VOID = 0,
    SLJIT_ARG_TYPE_SW = 1,
    SLJIT_ARG_TYPE_UW = 2,
    SLJIT_ARG_TYPE_S32 = 3,
    SLJIT_ARG_TYPE_U32 = 4,
    SLJIT_ARG_TYPE_F32 = 5,
    SLJIT_ARG_TYPE_F64 = 6,

    SLJIT_DEF_SHIFT = 4
);

pub const fn sljit_def_ret(t: sljit_s32) -> sljit_s32 {
    t
}

pub const fn sljit_def_arg1(t: sljit_s32) -> sljit_s32 {
    t << SLJIT_DEF_SHIFT
}

pub const fn sljit_def_arg2(t: sljit_s32) -> sljit_s32 {
    t << (2 * SLJIT_DEF_SHIFT)
}

pub const fn sljit_def_arg3(t: sljit_s32) -> sljit_s32 {
    t << (3 * SLJIT_DEF_SHIFT)
}

pub const fn sljit_def_arg4(t: sljit_s32) -> sljit_s32 {
    t << (4 * SLJIT_DEF_SHIFT)
}

pub const fn sljit_ret(t: sljit_s32) -> sljit_s32 {
    sljit_def_ret(t)
}

pub const fn sljit_arg1(t: sljit_s32) -> sljit_s32 {
    sljit_def_arg1(t)
}

pub const fn sljit_arg2(t: sljit_s32) -> sljit_s32 {
    sljit_def_arg2(t)
}

pub const fn sljit_arg3(t: sljit_s32) -> sljit_s32 {
    sljit_def_arg3(t)
}

pub const fn sljit_arg4(t: sljit_s32) -> sljit_s32 {
    sljit_def_arg4(t)
}
macro_rules! def {
    (#define $name : ident $val: expr;$($rest:tt)*) => {

        pub const $name: sljit_s32 = $val;
        def!(@parse $($rest)*);

    };

    (@parse #define $name: ident ($($a:ident),*) $val: expr; $($rest:tt)*) => {
        pub const fn $name($($a: sljit_s32),*) -> sljit_s32 {
            $val
        }

        def!(@parse $($rest)*);
    };

    (@parse #define $name : ident $val: expr; $($rest:tt)*) => {
        pub const $name : sljit_s32 = $val;
        def!(@parse $($rest)*);
    };
    (@parse) => {};
}

def!(

   /*
      Source and destination operands for arithmetical instructions
       imm              - a simple immediate value (cannot be used as a destination)
       reg              - any of the registers (immediate argument must be 0)
       [imm]            - absolute immediate memory address
       [reg+imm]        - indirect memory address
       [reg+(reg<<imm)] - indirect indexed memory address (shift must be between 0 and 3)
                          useful for (byte, half, int, sljit_sw) array access
                          (fully supported by both x86 and ARM architectures, and cheap operation on others)
   */

   /*
      IMPORTANT NOTE: memory access MUST be naturally aligned unless
                      SLJIT_UNALIGNED macro is defined and its value is 1.
        length | alignment
      ---------+-----------
        byte   | 1 byte (any physical_address is accepted)
        half   | 2 byte (physical_address & 0x1 == 0)
        int    | 4 byte (physical_address & 0x3 == 0)
        word   | 4 byte if SLJIT_32BIT_ARCHITECTURE is defined and its value is 1
               | 8 byte if SLJIT_64BIT_ARCHITECTURE is defined and its value is 1
       pointer | size of sljit_p type (4 byte on 32 bit machines, 4 or 8 byte
               | on 64 bit machines)
      Note:   Different architectures have different addressing limitations.
              A single instruction is enough for the following addressing
              modes. Other adrressing modes are emulated by instruction
              sequences. This information could help to improve those code
              generators which focuses only a few architectures.
      x86:    [reg+imm], -2^32+1 <= imm <= 2^32-1 (full address space on x86-32)
              [reg+(reg<<imm)] is supported
              [imm], -2^32+1 <= imm <= 2^32-1 is supported
              Write-back is not supported
      arm:    [reg+imm], -4095 <= imm <= 4095 or -255 <= imm <= 255 for signed
                   bytes, any halfs or floating point values)
              [reg+(reg<<imm)] is supported
              Write-back is supported
      arm-t2: [reg+imm], -255 <= imm <= 4095
              [reg+(reg<<imm)] is supported
              Write back is supported only for [reg+imm], where -255 <= imm <= 255
      arm64:  [reg+imm], -256 <= imm <= 255, 0 <= aligned imm <= 4095 * alignment
              [reg+(reg<<imm)] is supported
              Write back is supported only for [reg+imm], where -256 <= imm <= 255
      ppc:    [reg+imm], -65536 <= imm <= 65535. 64 bit loads/stores and 32 bit
                   signed load on 64 bit requires immediates divisible by 4.
                   [reg+imm] is not supported for signed 8 bit values.
              [reg+reg] is supported
              Write-back is supported except for one instruction: 32 bit signed
                   load with [reg+imm] addressing mode on 64 bit.
      mips:   [reg+imm], -65536 <= imm <= 65535
      sparc:  [reg+imm], -4096 <= imm <= 4095
              [reg+reg] is supported
      s390x:  [reg+imm], -2^19 <= imm < 2^19
              [reg+reg] is supported
              Write-back is not supported
   */

   /* Macros for specifying operand types. */
   #define SLJIT_MEM		0x80;
   #define SLJIT_MEM0()		SLJIT_MEM;
   #define SLJIT_MEM1(r1)		SLJIT_MEM | (r1);
   #define SLJIT_MEM2(r1, r2)	SLJIT_MEM | (r1) | ((r2) << 8);
   #define SLJIT_IMM		0x40;
   #define SLJIT_I32_OP		0x100;

/* Set F32 (single) precision mode for floating-point computation. This
   option is similar to SLJIT_I32_OP, it just applies to floating point
   registers. When this option is passed, the CPU performs 32 bit floating
   point operations, rather than 64 bit one. Similar to SLJIT_I32_OP, all
   register arguments must be the result of those operations where this
   option was also set.
   This option is part of the instruction name, so there is no need to
   manually set it. E.g:
     SLJIT_MOV_F32 = (SLJIT_MOV_F64 | SLJIT_F32_OP)
 */
#define SLJIT_F32_OP		SLJIT_I32_OP;

/* Many CPUs (x86, ARM, PPC) have status flags which can be set according
   to the result of an operation. Other CPUs (MIPS) do not have status
   flags, and results must be stored in registers. To cover both architecture
   types efficiently only two flags are defined by SLJIT:
    * Zero (equal) flag: it is set if the result is zero
    * Variable flag: its value is defined by the last arithmetic operation
   SLJIT instructions can set any or both of these flags. The value of
   these flags is undefined if the instruction does not specify their value.
   The description of each instruction contains the list of allowed flag
   types.
   Example: SLJIT_ADD can set the Z, OVERFLOW, CARRY flags hence
     sljit_op2(..., SLJIT_ADD, ...)
       Both the zero and variable flags are undefined so they can
       have any value after the operation is completed.
     sljit_op2(..., SLJIT_ADD | SLJIT_SET_Z, ...)
       Sets the zero flag if the result is zero, clears it otherwise.
       The variable flag is undefined.
     sljit_op2(..., SLJIT_ADD | SLJIT_SET_OVERFLOW, ...)
       Sets the variable flag if an integer overflow occurs, clears
       it otherwise. The zero flag is undefined.
     sljit_op2(..., SLJIT_ADD | SLJIT_SET_Z | SLJIT_SET_CARRY, ...)
       Sets the zero flag if the result is zero, clears it otherwise.
       Sets the variable flag if unsigned overflow (carry) occurs,
       clears it otherwise.
   If an instruction (e.g. SLJIT_MOV) does not modify flags the flags are
   unchanged.
   Using these flags can reduce the number of emitted instructions. E.g. a
   fast loop can be implemented by decreasing a counter register and set the
   zero flag to jump back if the counter register has not reached zero.
   Motivation: although CPUs can set a large number of flags, usually their
   values are ignored or only one of them is used. Emulating a large number
   of flags on systems without flag register is complicated so SLJIT
   instructions must specify the flag they want to use and only that flag
   will be emulated. The last arithmetic instruction can be repeated if
   multiple flags need to be checked.
*/

/* Set Zero status flag. */
#define SLJIT_SET_Z			0x0200;
/* Set the variable status flag if condition is true.
   See comparison types. */
#define SLJIT_SET(condition)			((condition) << 10);

/* Notes:
     - you cannot postpone conditional jump instructions except if noted that
       the instruction does not set flags (See: SLJIT_KEEP_FLAGS).
     - flag combinations: '|' means 'logical or'. */

/* Starting index of opcodes for sljit_emit_op0. */
#define SLJIT_OP0_BASE			0;

/* Flags: - (does not modify flags)
   Note: breakpoint instruction is not supported by all architectures (e.g. ppc)
         It falls back to SLJIT_NOP in those cases. */
#define SLJIT_BREAKPOINT		(SLJIT_OP0_BASE + 0);
/* Flags: - (does not modify flags)
   Note: may or may not cause an extra cycle wait
         it can even decrease the runtime in a few cases. */
#define SLJIT_NOP			(SLJIT_OP0_BASE + 1);
/* Flags: - (may destroy flags)
   Unsigned multiplication of SLJIT_R0 and SLJIT_R1.
   Result is placed into SLJIT_R1:SLJIT_R0 (high:low) word */
#define SLJIT_LMUL_UW			(SLJIT_OP0_BASE + 2);
/* Flags: - (may destroy flags)
   Signed multiplication of SLJIT_R0 and SLJIT_R1.
   Result is placed into SLJIT_R1:SLJIT_R0 (high:low) word */
#define SLJIT_LMUL_SW			(SLJIT_OP0_BASE + 3);
/* Flags: - (may destroy flags)
   Unsigned divide of the value in SLJIT_R0 by the value in SLJIT_R1.
   The result is placed into SLJIT_R0 and the remainder into SLJIT_R1.
   Note: if SLJIT_R1 is 0, the behaviour is undefined. */
#define SLJIT_DIVMOD_UW			(SLJIT_OP0_BASE + 4);
#define SLJIT_DIVMOD_U32		(SLJIT_DIVMOD_UW | SLJIT_I32_OP);
/* Flags: - (may destroy flags)
   Signed divide of the value in SLJIT_R0 by the value in SLJIT_R1.
   The result is placed into SLJIT_R0 and the remainder into SLJIT_R1.
   Note: if SLJIT_R1 is 0, the behaviour is undefined.
   Note: if SLJIT_R1 is -1 and SLJIT_R0 is integer min (0x800..00),
         the behaviour is undefined. */
#define SLJIT_DIVMOD_SW			(SLJIT_OP0_BASE + 5);
#define SLJIT_DIVMOD_S32		(SLJIT_DIVMOD_SW | SLJIT_I32_OP);
/* Flags: - (may destroy flags)
   Unsigned divide of the value in SLJIT_R0 by the value in SLJIT_R1.
   The result is placed into SLJIT_R0. SLJIT_R1 preserves its value.
   Note: if SLJIT_R1 is 0, the behaviour is undefined. */
#define SLJIT_DIV_UW			(SLJIT_OP0_BASE + 6);
#define SLJIT_DIV_U32			(SLJIT_DIV_UW | SLJIT_I32_OP);
/* Flags: - (may destroy flags)
   Signed divide of the value in SLJIT_R0 by the value in SLJIT_R1.
   The result is placed into SLJIT_R0. SLJIT_R1 preserves its value.
   Note: if SLJIT_R1 is 0, the behaviour is undefined.
   Note: if SLJIT_R1 is -1 and SLJIT_R0 is integer min (0x800..00),
         the behaviour is undefined. */
#define SLJIT_DIV_SW			(SLJIT_OP0_BASE + 7);
#define SLJIT_DIV_S32			(SLJIT_DIV_SW | SLJIT_I32_OP);
/* Flags: - (does not modify flags)
   ENDBR32 instruction for x86-32 and ENDBR64 instruction for x86-64
   when Intel Control-flow Enforcement Technology (CET) is enabled.
   No instruction for other architectures.  */
#define SLJIT_ENDBR			(SLJIT_OP0_BASE + 8);
/* Flags: - (may destroy flags)
   Skip stack frames before return.  */
#define SLJIT_SKIP_FRAMES_BEFORE_RETURN	(SLJIT_OP0_BASE + 9);


/* Starting index of opcodes for sljit_emit_op1. */
#define SLJIT_OP1_BASE			32;

/* The MOV instruction transfers data from source to destination.
   MOV instruction suffixes:
   U8  - unsigned 8 bit data transfer
   S8  - signed 8 bit data transfer
   U16 - unsigned 16 bit data transfer
   S16 - signed 16 bit data transfer
   U32 - unsigned int (32 bit) data transfer
   S32 - signed int (32 bit) data transfer
   P   - pointer (sljit_p) data transfer
*/

/* Flags: - (does not modify flags) */
#define SLJIT_MOV			(SLJIT_OP1_BASE + 0);
/* Flags: - (does not modify flags) */
#define SLJIT_MOV_U8			(SLJIT_OP1_BASE + 1);
#define SLJIT_MOV32_U8			(SLJIT_MOV_U8 | SLJIT_I32_OP);
/* Flags: - (does not modify flags) */
#define SLJIT_MOV_S8			(SLJIT_OP1_BASE + 2);
#define SLJIT_MOV32_S8			(SLJIT_MOV_S8 | SLJIT_I32_OP);
/* Flags: - (does not modify flags) */
#define SLJIT_MOV_U16			(SLJIT_OP1_BASE + 3);
#define SLJIT_MOV32_U16			(SLJIT_MOV_U16 | SLJIT_I32_OP);
/* Flags: - (does not modify flags) */
#define SLJIT_MOV_S16			(SLJIT_OP1_BASE + 4);
#define SLJIT_MOV32_S16			(SLJIT_MOV_S16 | SLJIT_I32_OP);
/* Flags: - (does not modify flags)
   Note: no SLJIT_MOV32_U32 form, since it is the same as SLJIT_MOV32 */
#define SLJIT_MOV_U32			(SLJIT_OP1_BASE + 5);
/* Flags: - (does not modify flags)
   Note: no SLJIT_MOV32_S32 form, since it is the same as SLJIT_MOV32 */
#define SLJIT_MOV_S32			(SLJIT_OP1_BASE + 6);
/* Flags: - (does not modify flags) */
#define SLJIT_MOV32			(SLJIT_MOV_S32 | SLJIT_I32_OP);
/* Flags: - (does not modify flags)
   Note: load a pointer sized data, useful on x32 (a 32 bit mode on x86-64
         where all x64 features are available, e.g. 16 register) or similar
         compiling modes */
#define SLJIT_MOV_P			(SLJIT_OP1_BASE + 7);
/* Flags: Z
   Note: immediate source argument is not supported */
#define SLJIT_NOT			(SLJIT_OP1_BASE + 8);
#define SLJIT_NOT32			(SLJIT_NOT | SLJIT_I32_OP);
/* Flags: Z | OVERFLOW
   Note: immediate source argument is not supported */
#define SLJIT_NEG			(SLJIT_OP1_BASE + 9);
#define SLJIT_NEG32			(SLJIT_NEG | SLJIT_I32_OP);
/* Count leading zeroes
   Flags: - (may destroy flags)
   Note: immediate source argument is not supported */
#define SLJIT_CLZ			(SLJIT_OP1_BASE + 10);
#define SLJIT_CLZ32			(SLJIT_CLZ | SLJIT_I32_OP);


/* Starting index of opcodes for sljit_emit_op2. */
#define SLJIT_OP2_BASE			96;

/* Flags: Z | OVERFLOW | CARRY */
#define SLJIT_ADD			(SLJIT_OP2_BASE + 0);
#define SLJIT_ADD32			(SLJIT_ADD | SLJIT_I32_OP);
/* Flags: CARRY */
#define SLJIT_ADDC			(SLJIT_OP2_BASE + 1);
#define SLJIT_ADDC32			(SLJIT_ADDC | SLJIT_I32_OP);
/* Flags: Z | LESS | GREATER_EQUAL | GREATER | LESS_EQUAL
          SIG_LESS | SIG_GREATER_EQUAL | SIG_GREATER
          SIG_LESS_EQUAL | CARRY */
#define SLJIT_SUB			(SLJIT_OP2_BASE + 2);
#define SLJIT_SUB32			(SLJIT_SUB | SLJIT_I32_OP);
/* Flags: CARRY */
#define SLJIT_SUBC			(SLJIT_OP2_BASE + 3);
#define SLJIT_SUBC32			(SLJIT_SUBC | SLJIT_I32_OP);
/* Note: integer mul
   Flags: MUL_OVERFLOW */
#define SLJIT_MUL			(SLJIT_OP2_BASE + 4);
#define SLJIT_MUL32			(SLJIT_MUL | SLJIT_I32_OP);
/* Flags: Z */
#define SLJIT_AND			(SLJIT_OP2_BASE + 5);
#define SLJIT_AND32			(SLJIT_AND | SLJIT_I32_OP);
/* Flags: Z */
#define SLJIT_OR			(SLJIT_OP2_BASE + 6);
#define SLJIT_OR32			(SLJIT_OR | SLJIT_I32_OP);
/* Flags: Z */
#define SLJIT_XOR			(SLJIT_OP2_BASE + 7);
#define SLJIT_XOR32			(SLJIT_XOR | SLJIT_I32_OP);
/* Flags: Z
   Let bit_length be the length of the shift operation: 32 or 64.
   If src2 is immediate, src2w is masked by (bit_length - 1).
   Otherwise, if the content of src2 is outside the range from 0
   to bit_length - 1, the result is undefined. */
#define SLJIT_SHL			(SLJIT_OP2_BASE + 8);
#define SLJIT_SHL32			(SLJIT_SHL | SLJIT_I32_OP);
/* Flags: Z
   Let bit_length be the length of the shift operation: 32 or 64.
   If src2 is immediate, src2w is masked by (bit_length - 1).
   Otherwise, if the content of src2 is outside the range from 0
   to bit_length - 1, the result is undefined. */
#define SLJIT_LSHR			(SLJIT_OP2_BASE + 9);
#define SLJIT_LSHR32			(SLJIT_LSHR | SLJIT_I32_OP);
/* Flags: Z
   Let bit_length be the length of the shift operation: 32 or 64.
   If src2 is immediate, src2w is masked by (bit_length - 1).
   Otherwise, if the content of src2 is outside the range from 0
   to bit_length - 1, the result is undefined. */
#define SLJIT_ASHR			(SLJIT_OP2_BASE + 10);
#define SLJIT_ASHR32			(SLJIT_ASHR | SLJIT_I32_OP);

/* Starting index of opcodes for sljit_emit_op2. */
#define SLJIT_OP_SRC_BASE		128;

/* Note: src cannot be an immedate value
   Flags: - (does not modify flags) */
#define SLJIT_FAST_RETURN		(SLJIT_OP_SRC_BASE + 0);
/* Skip stack frames before fast return.
   Note: src cannot be an immedate value
   Flags: may destroy flags. */
#define SLJIT_SKIP_FRAMES_BEFORE_FAST_RETURN	(SLJIT_OP_SRC_BASE + 1);
/* Prefetch value into the level 1 data cache
   Note: if the target CPU does not support data prefetch,
         no instructions are emitted.
   Note: this instruction never fails, even if the memory address is invalid.
   Flags: - (does not modify flags) */
#define SLJIT_PREFETCH_L1		(SLJIT_OP_SRC_BASE + 2);
/* Prefetch value into the level 2 data cache
   Note: same as SLJIT_PREFETCH_L1 if the target CPU
         does not support this instruction form.
   Note: this instruction never fails, even if the memory address is invalid.
   Flags: - (does not modify flags) */
#define SLJIT_PREFETCH_L2		(SLJIT_OP_SRC_BASE + 3);
/* Prefetch value into the level 3 data cache
   Note: same as SLJIT_PREFETCH_L2 if the target CPU
         does not support this instruction form.
   Note: this instruction never fails, even if the memory address is invalid.
   Flags: - (does not modify flags) */
#define SLJIT_PREFETCH_L3		(SLJIT_OP_SRC_BASE + 4);
/* Prefetch a value which is only used once (and can be discarded afterwards)
   Note: same as SLJIT_PREFETCH_L1 if the target CPU
         does not support this instruction form.
   Note: this instruction never fails, even if the memory address is invalid.
   Flags: - (does not modify flags) */
#define SLJIT_PREFETCH_ONCE		(SLJIT_OP_SRC_BASE + 5);

/* Starting index of opcodes for sljit_emit_fop1. */
#define SLJIT_FOP1_BASE			160;

/* Flags: - (does not modify flags) */
#define SLJIT_MOV_F64			(SLJIT_FOP1_BASE + 0);
#define SLJIT_MOV_F32			(SLJIT_MOV_F64 | SLJIT_F32_OP);
/* Convert opcodes: CONV[DST_TYPE].FROM[SRC_TYPE]
   SRC/DST TYPE can be: D - double, S - single, W - signed word, I - signed int
   Rounding mode when the destination is W or I: round towards zero. */
/* Flags: - (does not modify flags) */
#define SLJIT_CONV_F64_FROM_F32		(SLJIT_FOP1_BASE + 1);
#define SLJIT_CONV_F32_FROM_F64		(SLJIT_CONV_F64_FROM_F32 | SLJIT_F32_OP);
/* Flags: - (does not modify flags) */
#define SLJIT_CONV_SW_FROM_F64		(SLJIT_FOP1_BASE + 2);
#define SLJIT_CONV_SW_FROM_F32		(SLJIT_CONV_SW_FROM_F64 | SLJIT_F32_OP);
/* Flags: - (does not modify flags) */
#define SLJIT_CONV_S32_FROM_F64		(SLJIT_FOP1_BASE + 3);
#define SLJIT_CONV_S32_FROM_F32		(SLJIT_CONV_S32_FROM_F64 | SLJIT_F32_OP);
/* Flags: - (does not modify flags) */
#define SLJIT_CONV_F64_FROM_SW		(SLJIT_FOP1_BASE + 4);
#define SLJIT_CONV_F32_FROM_SW		(SLJIT_CONV_F64_FROM_SW | SLJIT_F32_OP);
/* Flags: - (does not modify flags) */
#define SLJIT_CONV_F64_FROM_S32		(SLJIT_FOP1_BASE + 5);
#define SLJIT_CONV_F32_FROM_S32		(SLJIT_CONV_F64_FROM_S32 | SLJIT_F32_OP);
/* Note: dst is the left and src is the right operand for SLJIT_CMPD.
   Flags: EQUAL_F | LESS_F | GREATER_EQUAL_F | GREATER_F | LESS_EQUAL_F */
#define SLJIT_CMP_F64			(SLJIT_FOP1_BASE + 6);
#define SLJIT_CMP_F32			(SLJIT_CMP_F64 | SLJIT_F32_OP);
/* Flags: - (does not modify flags) */
#define SLJIT_NEG_F64			(SLJIT_FOP1_BASE + 7);
#define SLJIT_NEG_F32			(SLJIT_NEG_F64 | SLJIT_F32_OP);
/* Flags: - (does not modify flags) */
#define SLJIT_ABS_F64			(SLJIT_FOP1_BASE + 8);
#define SLJIT_ABS_F32			(SLJIT_ABS_F64 | SLJIT_F32_OP);


/* Starting index of opcodes for sljit_emit_fop2. */
#define SLJIT_FOP2_BASE			192;

/* Flags: - (does not modify flags) */
#define SLJIT_ADD_F64			(SLJIT_FOP2_BASE + 0);
#define SLJIT_ADD_F32			(SLJIT_ADD_F64 | SLJIT_F32_OP);
/* Flags: - (does not modify flags) */
#define SLJIT_SUB_F64			(SLJIT_FOP2_BASE + 1);
#define SLJIT_SUB_F32			(SLJIT_SUB_F64 | SLJIT_F32_OP);
/* Flags: - (does not modify flags) */
#define SLJIT_MUL_F64			(SLJIT_FOP2_BASE + 2);
#define SLJIT_MUL_F32			(SLJIT_MUL_F64 | SLJIT_F32_OP);
/* Flags: - (does not modify flags) */
#define SLJIT_DIV_F64			(SLJIT_FOP2_BASE + 3);
#define SLJIT_DIV_F32			(SLJIT_DIV_F64 | SLJIT_F32_OP);


);
def!(
/* Invert (negate) conditional type: xor (^) with 0x1 */

/* Integer comparison types. */
#define SLJIT_EQUAL			0;
#define SLJIT_EQUAL32			(SLJIT_EQUAL | SLJIT_I32_OP);
#define SLJIT_ZERO			0;
#define SLJIT_ZERO32			(SLJIT_ZERO | SLJIT_I32_OP);
#define SLJIT_NOT_EQUAL			1;
#define SLJIT_NOT_EQUAL32		(SLJIT_NOT_EQUAL | SLJIT_I32_OP);
#define SLJIT_NOT_ZERO			1;
#define SLJIT_NOT_ZERO32		(SLJIT_NOT_ZERO | SLJIT_I32_OP);

#define SLJIT_LESS			2;
#define SLJIT_LESS32			(SLJIT_LESS | SLJIT_I32_OP);
#define SLJIT_SET_LESS			SLJIT_SET(SLJIT_LESS);
#define SLJIT_GREATER_EQUAL		3;
#define SLJIT_GREATER_EQUAL32		(SLJIT_GREATER_EQUAL | SLJIT_I32_OP);
#define SLJIT_SET_GREATER_EQUAL		SLJIT_SET(SLJIT_GREATER_EQUAL);
#define SLJIT_GREATER			4;
#define SLJIT_GREATER32			(SLJIT_GREATER | SLJIT_I32_OP);
#define SLJIT_SET_GREATER		SLJIT_SET(SLJIT_GREATER);
#define SLJIT_LESS_EQUAL		5;
#define SLJIT_LESS_EQUAL32		(SLJIT_LESS_EQUAL | SLJIT_I32_OP);
#define SLJIT_SET_LESS_EQUAL		SLJIT_SET(SLJIT_LESS_EQUAL);
#define SLJIT_SIG_LESS			6;
#define SLJIT_SIG_LESS32		(SLJIT_SIG_LESS | SLJIT_I32_OP);
#define SLJIT_SET_SIG_LESS		SLJIT_SET(SLJIT_SIG_LESS);
#define SLJIT_SIG_GREATER_EQUAL		7;
#define SLJIT_SIG_GREATER_EQUAL32	(SLJIT_SIG_GREATER_EQUAL | SLJIT_I32_OP);
#define SLJIT_SET_SIG_GREATER_EQUAL	SLJIT_SET(SLJIT_SIG_GREATER_EQUAL);
#define SLJIT_SIG_GREATER		8;
#define SLJIT_SIG_GREATER32		(SLJIT_SIG_GREATER | SLJIT_I32_OP);
#define SLJIT_SET_SIG_GREATER		SLJIT_SET(SLJIT_SIG_GREATER);
#define SLJIT_SIG_LESS_EQUAL		9;
#define SLJIT_SIG_LESS_EQUAL32		(SLJIT_SIG_LESS_EQUAL | SLJIT_I32_OP);
#define SLJIT_SET_SIG_LESS_EQUAL	SLJIT_SET(SLJIT_SIG_LESS_EQUAL);

#define SLJIT_OVERFLOW			10;
#define SLJIT_OVERFLOW32		(SLJIT_OVERFLOW | SLJIT_I32_OP);
#define SLJIT_SET_OVERFLOW		SLJIT_SET(SLJIT_OVERFLOW);
#define SLJIT_NOT_OVERFLOW		11;
#define SLJIT_NOT_OVERFLOW32		(SLJIT_NOT_OVERFLOW | SLJIT_I32_OP);

#define SLJIT_MUL_OVERFLOW		12;
#define SLJIT_MUL_OVERFLOW32		(SLJIT_MUL_OVERFLOW | SLJIT_I32_OP);
#define SLJIT_SET_MUL_OVERFLOW		SLJIT_SET(SLJIT_MUL_OVERFLOW);
#define SLJIT_MUL_NOT_OVERFLOW		13;
#define SLJIT_MUL_NOT_OVERFLOW32	(SLJIT_MUL_NOT_OVERFLOW | SLJIT_I32_OP);

/* There is no SLJIT_CARRY or SLJIT_NOT_CARRY. */
#define SLJIT_SET_CARRY			SLJIT_SET(14);

/* Floating point comparison types. */
#define SLJIT_EQUAL_F64			16;
#define SLJIT_EQUAL_F32			(SLJIT_EQUAL_F64 | SLJIT_F32_OP);
#define SLJIT_SET_EQUAL_F		SLJIT_SET(SLJIT_EQUAL_F64);
#define SLJIT_NOT_EQUAL_F64		17;
#define SLJIT_NOT_EQUAL_F32		(SLJIT_NOT_EQUAL_F64 | SLJIT_F32_OP);
#define SLJIT_SET_NOT_EQUAL_F		SLJIT_SET(SLJIT_NOT_EQUAL_F64);
#define SLJIT_LESS_F64			18;
#define SLJIT_LESS_F32			(SLJIT_LESS_F64 | SLJIT_F32_OP);
#define SLJIT_SET_LESS_F		SLJIT_SET(SLJIT_LESS_F64);
#define SLJIT_GREATER_EQUAL_F64		19;
#define SLJIT_GREATER_EQUAL_F32		(SLJIT_GREATER_EQUAL_F64 | SLJIT_F32_OP);
#define SLJIT_SET_GREATER_EQUAL_F	SLJIT_SET(SLJIT_GREATER_EQUAL_F64);
#define SLJIT_GREATER_F64		20;
#define SLJIT_GREATER_F32		(SLJIT_GREATER_F64 | SLJIT_F32_OP);
#define SLJIT_SET_GREATER_F		SLJIT_SET(SLJIT_GREATER_F64);
#define SLJIT_LESS_EQUAL_F64		21;
#define SLJIT_LESS_EQUAL_F32		(SLJIT_LESS_EQUAL_F64 | SLJIT_F32_OP);
#define SLJIT_SET_LESS_EQUAL_F		SLJIT_SET(SLJIT_LESS_EQUAL_F64);
#define SLJIT_UNORDERED_F64		22;
#define SLJIT_UNORDERED_F32		(SLJIT_UNORDERED_F64 | SLJIT_F32_OP);
#define SLJIT_SET_UNORDERED_F		SLJIT_SET(SLJIT_UNORDERED_F64);
#define SLJIT_ORDERED_F64		23;
#define SLJIT_ORDERED_F32		(SLJIT_ORDERED_F64 | SLJIT_F32_OP);
#define SLJIT_SET_ORDERED_F		SLJIT_SET(SLJIT_ORDERED_F64);

/* Unconditional jump types. */
#define SLJIT_JUMP			24;
    /* Fast calling method. See sljit_emit_fast_enter / SLJIT_FAST_RETURN. */
#define SLJIT_FAST_CALL			25;
    /* Called function must be declared with the SLJIT_FUNC attribute. */
#define SLJIT_CALL			26;
    /* Called function must be declared with cdecl attribute.
       This is the default attribute for C functions. */
#define SLJIT_CALL_CDECL		27;

/* The target can be changed during runtime (see: sljit_set_jump_addr). */
#define SLJIT_REWRITABLE_JUMP		0x1000;

/* When SLJIT_MEM_SUPP is passed, no instructions are emitted.
   Instead the function returns with SLJIT_SUCCESS if the instruction
   form is supported and SLJIT_ERR_UNSUPPORTED otherwise. This flag
   allows runtime checking of available instruction forms. */
   #define SLJIT_MEM_SUPP		0x0200;
   /* Memory load operation. This is the default. */
   #define SLJIT_MEM_LOAD		0x0000;
   /* Memory store operation. */
   #define SLJIT_MEM_STORE		0x0400;
   /* Base register is updated before the memory access. */
   #define SLJIT_MEM_PRE		0x0800;
   /* Base register is updated after the memory access. */
   #define SLJIT_MEM_POST		0x1000;
);

def!(
    #define SLJIT_MAJOR_VERSION	0;
    #define SLJIT_MINOR_VERSION	94;
    /* Indicates no error. */
#define SLJIT_SUCCESS			0;
/* After the call of sljit_generate_code(), the error code of the compiler
   is set to this value to avoid future sljit calls (in debug mode at least).
   The complier should be freed after sljit_generate_code(). */
#define SLJIT_ERR_COMPILED		1;
/* Cannot allocate non executable memory. */
#define SLJIT_ERR_ALLOC_FAILED		2;
/* Cannot allocate executable memory.
   Only for sljit_generate_code() */
#define SLJIT_ERR_EX_ALLOC_FAILED	3;
/* Return value for SLJIT_CONFIG_UNSUPPORTED placeholder architecture. */
#define SLJIT_ERR_UNSUPPORTED		4;
/* An ivalid argument is passed to any SLJIT function. */
#define SLJIT_ERR_BAD_ARGUMENT		5;
/* Dynamic code modification is not enabled. */
#define SLJIT_ERR_DYN_CODE_MOD		6;

);

#[cfg(test)]
mod tests {
    use super::*;
    use definition::*;
    union Code {
        nil: (),
        f0: extern "C" fn() -> sljit_sw,
        f1: extern "C" fn(a: sljit_sw) -> sljit_sw,
        f2: extern "C" fn(a: sljit_sw, b: sljit_sw) -> sljit_sw,
        f3: extern "C" fn(a: sljit_sw, b: sljit_sw, c: sljit_sw) -> sljit_sw,
        code: *mut u8,
    }

    #[test]
    pub fn enter_and_return() {
        unsafe {
            let mut code = Code { nil: () };

            let compiler = SlJITCompiler::new();
            compiler.emit_enter(
                0,
                sljit_arg1(SLJIT_ARG_TYPE_SW as _)
                    | sljit_arg2(SLJIT_ARG_TYPE_SW as _)
                    | sljit_arg3(SLJIT_ARG_TYPE_SW as _),
                3,
                3,
                0,
                0,
                0,
            );
            compiler.emit_return(SLJIT_MOV as _, SLJIT_S1, 0);
            assert_eq!(compiler.get_generated_code_size(), 0);
            code.code = compiler.generate_code();
            drop(compiler);
            assert_eq!((code.f3)(3, -21, 4), -21);
            assert_eq!((code.f3)(-21, 4, 3), 4);
        }
    }
}
