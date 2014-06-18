let source_files =
    ["stdlib.sh";"stdarr.sh"]

let get_contents filename =
    let chan = open_in filename in
    let len = in_channel_length chan in
    let s = String.create len in
        really_input chan s 0 len;
        close_in chan;
        s

let source =
    List.fold_left (fun acc x -> acc ^ (get_contents x)) "" source_files
