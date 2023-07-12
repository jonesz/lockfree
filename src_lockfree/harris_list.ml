type 'a node = Nil | Next of 'a * ('a node * bool) Atomic.t
type 'a t = { mutable head : 'a node }

let atomic_helper a =
  let atomic_tuple = Atomic.get a in
  let succ, marked = atomic_tuple in
  (atomic_tuple, succ, marked)

let mk_node x succ =
  match succ with
  | None -> Next (x, (Nil, false) |> Atomic.make)
  | Some succ -> Next (x, (succ, false) |> Atomic.make)

let create () = { head = Nil }

let rec find pred curr x =
  match curr with
  | Nil -> pred
  | Next (curr_x, curr_atomic) ->
      let _, succ, curr_marked = atomic_helper curr_atomic in
      if curr_marked then
        match pred with
        (* TODO: The pred was Nil; curr is therefore the head of the list AND needs to be deleted. *)
        | Nil -> find curr succ x
        | Next (_, pred_atomic) ->
            let pred_tuple, _, pred_marked = atomic_helper pred_atomic in
            (* If the following fails, we call find on (pred, pred.next) (which ends up being (pred, curr)),
               so we get another shot. *)
            let _ =
              Atomic.compare_and_set pred_atomic pred_tuple (succ, pred_marked)
            in
            let _, pred_next, _ = atomic_helper pred_atomic in
            find pred pred_next x
      else if x <= curr_x then pred
      else find curr succ x

let contains lst x =
  let rec _contains n =
    match n with
    | Nil -> false (* Reached the end of the list. *)
    | Next (y, atomic) ->
        if x < y then false
        else
          let next, marked = Atomic.get atomic in
          if marked then _contains next
          else if x == y then true
          else _contains next
  in

  _contains lst.head

let add lst x =
  match lst.head with
  | Nil ->
      (* TODO: This needs to be atomic. *)
      lst.head <- mk_node x None;
      true
  | _ -> (
      let pred = find Nil lst.head x in
      match pred with
      | Nil ->
          (* TODO: This needs to be atomic. *)
          lst.head <- mk_node x (Some lst.head);
          true
      | Next (_, pred_next) ->
          let pred_next_tuple, succ, pred_marked = atomic_helper pred_next in
          let curr = mk_node x (Some succ) in
          Atomic.compare_and_set pred_next pred_next_tuple (curr, pred_marked))

let remove lst x =
  match lst.head with
  | Nil -> false
  | _ -> (
      let pred = find Nil lst.head x in
      match pred with
      | Nil -> false
      | Next (_, pred_next) -> (
          (* find will return the predecessor, we need to look at the next value. *)
          let _, curr, _ = atomic_helper pred_next in
          match curr with
          | Nil -> false
          | Next (curr_x, curr_next) ->
              if x == curr_x then
                let curr_next_tuple, succ, _ = atomic_helper curr_next in
                Atomic.compare_and_set curr_next curr_next_tuple (succ, true)
              else false))
