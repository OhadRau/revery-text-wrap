open Batteries

(* Split a string into an AppendList of strings, chopping at every newline and
   removing the newline characters. *)
let split_on_newline string =
  let lines = AppendList.empty () in
  let first = ref 0 in
  for i = 0 to String.length string - 1 do
    if string.[i] = '\n' then begin
      AppendList.append lines (String.slice ~first:!first ~last:i string);
      first := i + 1
    end
  done;
  (* Append the remaining characters after the last split *)
  if !first < String.length string - 1 then begin
    AppendList.append lines (String.slice ~first:!first ~last:(String.length string) string)
  end;
  lines

(* Tokenize a string into an AppendList of strings, chopping on every instance of whitespace
   and every hyphen. Isolate whitespace into its own token and preserve it in the final list. *)
let split_tokens string =
  let tokens = AppendList.empty () in
  let first = ref 0 in
  for i = 0 to String.length string - 1 do
    match string.[i] with
    | '\n' | ' ' | '\t' ->
      (* Push everything before this character (if there is anything) *)
      if !first <> i then begin
        AppendList.append tokens (String.slice ~first:!first ~last:i string)
      end;
      (* Then push this character *)
      AppendList.append tokens (String.slice ~first:i ~last:(i+1) string);
      (* And skip it *)
      first := i + 1
    | '-' ->
      (* When a word is already hyphenated, split it into tokens so we don't needlessly
         rehyphenate it and end up with a double hyphen *)
      AppendList.append tokens (String.slice ~first:!first ~last:(i+1) string);
      first := i + 1
    | _ -> ()
  done;
  (* Append the remaining characters after the last split *)
  if !first < String.length string - 1 then begin
    AppendList.append tokens (String.slice ~first:!first ~last:(String.length string) string)
  end;
  tokens

(* Check if a token is blank/whitespace *)
let is_whitespace str =
  (* We only have to check the start b/c the tokenizer won't return anything after the whitespace *)
  String.length str = 0 || str.[0] == ' ' || str.[0] == '\t'