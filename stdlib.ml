let source_file = "stdlib.sh"

let source =
    let chan = open_in source_file in
    let len = in_channel_length chan in
    let s = String.create len in
        really_input chan s 0 len;
        close_in chan;
        s
