use std::env;
use std::path::PathBuf;

use cc;

fn main() {
    let src = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap()).join("src");

    let mut cc = cc::Build::new();
    cc.warnings(false);

    cc.define("SLJIT_CONFIG_AUTO", "1");
    if env::var("TARGET").unwrap().contains("apple-darwin") {
        cc.define("SLJIT_PROT_EXECUTABLE_ALLOCATOR", "0");
    } else {
        cc.define("SLJIT_PROT_EXECUTABLE_ALLOCATOR", "1");
    }
    cc.define("SLJIT_ARGUMENT_CHECKS", "1");
    cc.define("SLJIT_DEBUG", "0");
    cc.define("SLJIT_VERBOSE", "0");

    cc.define("SLJIT_INLINE", "");
    cc.include(&src.join("sljit"));
    cc.flag("-fno-inline-functions");
    cc.file(src.join("sljit").join("sljit_src").join("sljitLir.c"));
    cc.compile("sljit");

    let bindings = bindgen::Builder::default()
        // The input header we would like to generate
        // bindings for.
        .header("wrapper.h")
        .generate_inline_functions(true)
        // Tell cargo to invalidate the built crate whenever any of the
        // included header files changed.
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");
    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
