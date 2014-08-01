let sh_files =
    (* ["stdlib.sh";"stdarr.sh"] *)
    ["stdlib.sh"]

let stdlib_files =
    ["stdlib/stdlib.shly"]
    (* "stdlib/array.shly"] *)

let get_contents filename =
    let chan = open_in filename in
    let len = in_channel_length chan in
    let s = String.create len in
        really_input chan s 0 len;
        close_in chan;
        s

let add_sources acc x = acc ^ "\n" ^ (get_contents x)

let sh_source =
    List.fold_left add_sources "" sh_files

let stdlib_source =
    List.fold_left add_sources "" stdlib_files
