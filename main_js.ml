let compile_js_string source =
    Main.compile_string (Js.to_string source)
in
    Js.Unsafe.global##shlycompile <- Js.wrap_callback compile_js_string

