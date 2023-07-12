module HList = Saturn.Harris_list

(* TODO: This can be refactored and be made more programmatic. *)
let test_hlist () =
  let l = HList.create () in
  assert (HList.contains l 2 == false);

  assert (HList.add l 3 == true);
  assert (HList.add l 2 == true);
  assert (HList.add l 4 == true);
  assert (HList.add l 8 == true);
  assert (HList.add l 5 == true);
  assert (HList.add l 1 == true);

  assert (HList.contains l 2 == true);
  assert (HList.contains l 3 == true);
  assert (HList.contains l 4 == true);
  assert (HList.contains l 9 == false);

  assert (HList.remove l 3 == true);
  assert (HList.contains l 3 == false);

  assert (HList.remove l 3 == false);
  assert (HList.remove l 8 == true);
  assert (HList.add l 10 == true);

  print_string "test_harris_list_empty: ok\n"

let _ = test_hlist ()
