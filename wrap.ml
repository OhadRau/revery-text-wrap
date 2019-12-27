open Batteries

module AppendList = AppendList

module type TEXT_WRAP_CONFIG = sig
  val width_of_char : char -> float
  val debug : bool
end

module TextWrap (C: TEXT_WRAP_CONFIG) = struct
  open Tokenize

  let width_of_char = C.width_of_char

  (* Get the width of a token (i.e. the sum of each character's width) *)
  let width_of_token str =
    let sum = ref 0.0 in
    str |> String.iter begin fun char ->
      sum := !sum +. width_of_char char
    end;
    !sum

  let wrap ~max_width ?(hyphenate=false) ?(ignore_preceding_whitespace=true) text =
    (* Create a buffer for the outputted lines *)
    let output_lines = AppendList.empty () in
    (* Split the input text into lines and for each line: *)
    split_on_newline text |> AppendList.iter begin fun line ->
      (* Create a buffer for the wrapped portion of the line *)
      let buffer = Buffer.create (String.length line) in
      (* Store the width of this portion *)
      let width = ref 0.0 in
      (* Tokenize the line by whitespace and for each token: *)
      split_tokens line |> AppendList.iter begin fun token ->
        (* Calculate the width of the token *)
        let token_width = width_of_token token in

        if C.debug then begin
          Printf.printf "Token: %s, Width: %f, Token_width: %f, Max_width: %f\n"
            token !width token_width max_width
        end;

        (* If the buffer is already too long, push the buffer and reset it *)
        if !width >= max_width then begin
          if C.debug then begin
            print_endline "Clear"
          end;
          AppendList.append output_lines (Buffer.contents buffer);
          Buffer.reset buffer;
          width := 0.0
        end;

        (* Check to see if the line starts w/ whitespace and if we should ignore it *)
        if ignore_preceding_whitespace && is_whitespace token && !width = 0.0 then begin
          if C.debug then begin
            print_endline "Decision: ignore"
          end;
          ()
        (* If it's gonna be too long with the whitespace, don't add it & just prepare to wrap,
          so that we don't get the whitespace at the start of the next line. *)
        end else if ignore_preceding_whitespace && is_whitespace token
              && !width +. token_width > max_width then begin
          if C.debug then begin
            print_endline "Decision: append-whitespace"
          end;
          width := !width +. token_width
        (* If we can add the token without exceeding the limit, add it to the current line *)
        end else if !width +. token_width <= max_width then begin
          if C.debug then begin
            print_endline "Decision: append"
          end;
          width := !width +. token_width;
          Buffer.add_string buffer token
        (* If it would exceed the limit and the user wants hyphenation: *)
        end else if hyphenate && !width +. token_width > max_width then begin
          if C.debug then begin
            print_endline "Decision: hyphenate"
          end;
          for i = 0 to String.length token - 1 do
            let char_width = width_of_char token.[i] in
            if C.debug then begin
              Printf.printf "--Index: %d, Char: %c, Char_width: %f\n" i token.[i] char_width
            end;
            (* FIXME: When do we actually insert the hyphen? *)
            if (!width +. char_width <= max_width) || (!width = 0.0 && char_width >= max_width) then begin
              if C.debug then begin
                print_endline "--Decision: append"
              end;
              Buffer.add_char buffer token.[i];
              width := !width +. char_width
            end else begin
              if C.debug then begin
                print_endline "--Decision: break"
              end;
              (* Finalize the current line and reset the buffer (if we need to) *)
              if !width > 0.0 then begin
                if i > 0 then begin
                  if C.debug then begin
                    print_endline "--Clear+hyphenate"
                  end;
                  let buffer_length = Buffer.length buffer in
                  let last_char = Buffer.nth buffer (buffer_length - 1) in
                  let hyphen = if i = 1 then " " else "-" in
                  AppendList.append output_lines (Buffer.sub buffer 0 (buffer_length - 1) ^ hyphen);
                  Buffer.reset buffer;
                  Buffer.add_char buffer last_char;
                  width := width_of_char last_char
                end else begin
                  if C.debug then begin
                    print_endline "--Clear"
                  end;
                  AppendList.append output_lines (Buffer.contents buffer);
                  Buffer.reset buffer;
                  width := 0.0
                end
              end;
              (* Then push the new token onto the buffer *)
              width := !width +. char_width;
              Buffer.add_char buffer token.[i]
            end
          done
        (* If it would exceed the limit and the user doesn't want hyphenation: *)
        end else begin
          if C.debug then begin
            print_endline "Decision: wrap"
          end;
          (* Finalize the current line and reset the buffer (if we need to) *)
          if !width > 0.0 then begin
            AppendList.append output_lines (Buffer.contents buffer);
            Buffer.reset buffer
          end;
          (* Then push the new token onto the buffer *)
          width := token_width;
          Buffer.add_string buffer token
        end
      end;

      (* Finalize any remaining text in the buffer *)
      if !width > 0.0 then begin
        AppendList.append output_lines (Buffer.contents buffer)
      end
    end;
    output_lines
end