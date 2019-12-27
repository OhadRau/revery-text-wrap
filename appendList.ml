(* List designed for O(1) appending/prepending:

   The structure of this list is similar to Batteries'
   RefList module, where each node holds a *mutable*
   reference to the next node. This allows us to update
   a node's reference in constant time rather than having
   to copy the entire list. We store a tail pointer to
   avoid walking to the back of the list when we append,
   which makes this faster for our use-case than RefList.
 *)
type 'a t = {
  mutable head: 'a node;
  mutable tail: 'a node
}

(* Each node is either None (= []) or Some (x, xs)
   (= x::xs). However, having this as a field in a
   record rather than a constructor gives us the
   ability to make the node mutable. *)
and 'a node = {
  mutable node: ('a * 'a node) option
}

(* Create an empty AppendList *)
let empty () = { head = { node = None }; tail = { node = None } }

(* Convert 'a list -> 'a AppendList.t *)
let of_list list =
  let alist = empty () in
  let rec aux = function
    | [] ->
      alist.tail <- { node = None };
      alist.tail
    | car::cdr -> { node = Some (car, aux cdr) } in
  alist.head <- aux list;
  alist

(* Convert 'a AppendList.t -> 'a list *)
let to_list alist =
  let rec aux = function
    | { node = None } -> []
    | { node = Some (car, cdr) } -> car::(aux cdr) in
  aux alist.head

(* Prepend x in front of xs. Similar to x::xs, but
   in-place/mutates the array. Runs in O(1) and returns (). *)
let prepend x xs =
  let root = xs.head.node in
  let was_empty = xs.head.node = None in
  xs.head <- { node = Some (x, { node = root }) };
  if was_empty then xs.tail <- xs.head

(* Append x to the end of xs. Similar to xs@[x], but
   in-place/mutates the array. Runs in O(1) and returns (). *)
let append xs x =
  let new_end = { node = None } in
  xs.tail.node <- Some (x, new_end);
  if xs.head.node = None then xs.head <- xs.tail;
  xs.tail <- new_end

(* Iterate f over the list, passing the index and value
   at each cell in the list. *)
let iteri f list =
  let rec aux i = function
    | { node = None } -> ()
    | { node = Some (x, xs) } ->
      f i x;
      aux (i + 1) xs in
  aux 0 list.head

(* Iterate f over the list, passing the value at each cell
   in the list *)
let iter f list =
  iteri (fun _ x -> f x) list