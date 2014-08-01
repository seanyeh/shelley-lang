let compile_js_string source =
    Main.compile_string (Js.to_string source)
in
    Js.Unsafe.global##shlycompile <- Js.wrap_callback compile_js_string

let get_stdlib () = Main.stdlib in
    Js.Unsafe.global##shlystdlib <- Js.wrap_callback get_stdlib;

