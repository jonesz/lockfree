open Saturn.Queue

let tests_sequential =
  QCheck.
    [
      (* TEST 1: push *)
      Test.make ~name:"push" (list int) (fun lpush ->
          assume (lpush <> []);
          (* Building a random queue *)
          let queue = create () in
          List.iter (push queue) lpush;

          (* Testing property *)
          not (is_empty queue));
      (* TEST 2 - push, pop until empty *)
      Test.make ~name:"push_pop_until_empty" (list int) (fun lpush ->
          (* Building a random queue *)
          let queue = create () in
          List.iter (push queue) lpush;

          (* Popping until [is_empty q] is true *)
          let count = ref 0 in
          while not (is_empty queue) do
            incr count;
            ignore (pop queue)
          done;

          (* Testing property *)
          pop queue = None && !count = List.length lpush);
      (* TEST 3 - push, pop, check FIFO  *)
      Test.make ~name:"fifo" (list int) (fun lpush ->
          (* Building a random queue *)
          let queue = create () in
          List.iter (push queue) lpush;

          let out = ref [] in
          let insert v = out := v :: !out in

          for _ = 1 to List.length lpush do
            match pop queue with None -> assert false | Some v -> insert v
          done;

          (* Testing property *)
          lpush = List.rev !out);
    ]

let tests_one_consumer_one_producer =
  QCheck.
    [
      (* TEST 1 - one consumer one producer:
         Parallel [push] and [pop]. *)
      Test.make ~count:10_000 ~name:"parallel_fifo" (list int) (fun lpush ->
          (* Initialization *)
          let queue = create () in

          (* Producer pushes. *)
          let producer =
            Domain.spawn (fun () -> List.iter (push queue) lpush)
          in

          let fifo =
            List.fold_left
              (fun acc item ->
                let popped = ref None in
                while Option.is_none !popped do
                  popped := pop queue
                done;
                acc && item = Option.get !popped)
              true lpush
          in
          let empty = is_empty queue in

          (* Ensure nothing is left behind. *)
          Domain.join producer;
          fifo && empty);
    ]

let tests_two_domains =
  QCheck.
    [
      (* TEST 1 - two domains doing multiple times one push then one pop.
         Parallel [push] and [pop].
      *)
      Test.make ~count:10_000 ~name:"parallel_pop_push"
        (pair small_nat small_nat) (fun (npush1, npush2) ->
          (* Initialization *)
          let queue = create () in
          let sema = Semaphore.Binary.make false in

          (* Using these lists instead of a random one enables to
             check for more properties. *)
          let lpush1 = List.init npush1 (fun i -> i) in
          let lpush2 = List.init npush2 (fun i -> i + npush1) in

          let work lpush =
            List.map
              (fun elt ->
                push queue elt;
                Domain.cpu_relax ();
                pop queue)
              lpush
          in

          let domain1 =
            Domain.spawn (fun () ->
                Semaphore.Binary.release sema;
                work lpush1)
          in
          let popped2 =
            while not (Semaphore.Binary.try_acquire sema) do
              Domain.cpu_relax ()
            done;
            work lpush2
          in

          (* As a domain always pushs before popping, all pops
             succeeds. *)
          let popped1 = Domain.join domain1 |> List.map Option.get in
          let popped2 = popped2 |> List.map Option.get in

          (* Check 1 : no elements are missing (everyting is popped). *)
          let all_elt_in =
            List.sort compare (popped1 @ popped2) = lpush1 @ lpush2
          in
          (* filter the elements pushed and popped by domain 1 *)
          let push1_pop1 = List.filter (fun elt -> elt < npush1) popped1 in
          (* filter the elements pushed by domain 2 and popped by domain 1 *)
          let push2_pop1 = List.filter (fun elt -> elt >= npush1) popped1 in
          (* filter the elements pushed by domain 1 and popped by domain 2 *)
          let push1_pop2 = List.filter (fun elt -> elt < npush1) popped2 in
          (* filter the elements pushed and popped by domain 2 *)
          let push2_pop2 = List.filter (fun elt -> elt >= npush1) popped2 in

          (* all these lists must be sorted *)
          let is_sorted list = List.sort compare list = list in
          all_elt_in && is_sorted push1_pop1 && is_sorted push1_pop2
          && is_sorted push2_pop1 && is_sorted push2_pop2);
      (* TEST 2 -
         Parallel [push] and [pop] with two domains

         Two domains randomly pushs and pops in parallel. They stop as
         soon as they have finished pushing a list of element to
         push. *)
      Test.make ~count:10_000 ~name:"parallel_pop_push_random"
        (pair small_nat small_nat) (fun (npush1, npush2) ->
          (* Initialization *)
          let queue = create () in
          let sema = Semaphore.Binary.make false in

          let lpush1 = List.init npush1 (fun i -> i) in
          let lpush2 = List.init npush2 (fun i -> i + npush1) in

          let work lpush =
            let consecutive_pop = ref 0 in
            let rec loop lpush popped =
              let what_to_do = Random.int 2 in
              if what_to_do = 0 || !consecutive_pop > 10 then (
                (* randomly choosing between pushing and popping except
                   if too many consecutive pops have already occurred *)
                consecutive_pop := 0;
                match lpush with
                | [] -> popped
                | elt :: xs ->
                    push queue elt;
                    loop xs popped)
              else (
                incr consecutive_pop;
                let p = pop queue in
                loop lpush (p :: popped))
            in
            loop lpush []
          in

          let domain1 =
            Domain.spawn (fun () ->
                Semaphore.Binary.release sema;
                work lpush1)
          in
          let popped2 =
            while not (Semaphore.Binary.try_acquire sema) do
              Domain.cpu_relax ()
            done;
            work lpush2
          in

          let popped1 =
            Domain.join domain1
            |> List.filter (function None -> false | _ -> true)
            |> List.map Option.get
          in
          let popped2 =
            popped2
            |> List.filter (function None -> false | _ -> true)
            |> List.map Option.get
          in

          (* Pop everything that is still on the queue *)
          let popped3 =
            let rec loop popped =
              match pop queue with
              | None -> popped
              | Some v -> loop (v :: popped)
            in
            loop []
          in
          (* Check that no element is missing. *)
          let all_n_elt_in =
            List.sort compare (popped1 @ popped2 @ popped3) = lpush1 @ lpush2
          in

          all_n_elt_in);
    ]

let main () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Michael_scott_queue"
    [
      ("test_sequential", to_alcotest tests_sequential);
      ("one_cons_one_prod", to_alcotest tests_one_consumer_one_producer);
      ("two_domains", to_alcotest tests_two_domains);
    ]
;;

main ()
