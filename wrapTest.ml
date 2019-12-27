open Wrap

module TW = TextWrap(struct
  (* Get the width of a character *)
  let width_of_char = function
    | '1' | 'i' | 'l' | 'I' -> 0.5
    | 'w' | 'W' | 'm' | 'M' -> 1.2
    | '\t' -> 4.0
    | '\n' -> 0.0
    | _ -> 1.0
  
  let debug = false
end)

let () =
  let wrapInput = "Here's another example of text where wrapping might be more difficult. " ^
                  "This string is very, very long and consists of words of varying lengths. " ^
                  "By utilizing some extremely long words, we can hopefully trigger some of " ^
                  "the more obscure edge-cases that word-wrapping can result in, such as " ^
                  "placing a hyphen in the middle of a word on top of another hyphen." in
  print_endline "==================";
  TW.wrap ~max_width:5.0 ~hyphenate:true wrapInput |> AppendList.iter print_endline;
  print_endline "==================";
  TW.wrap ~max_width:10.0 ~hyphenate:true wrapInput |> AppendList.iter print_endline;
  print_endline "==================";
  TW.wrap ~max_width:15.0 wrapInput |> AppendList.iter print_endline;
  print_endline "==================";
  TW.wrap ~max_width:40.0 wrapInput |> AppendList.iter print_endline
