
(* sandbox to test performances *)

open Mset

let time f x =
  let open Unix in
  let u = (times()).tms_utime in
  let y = f x in
  let ut = (times()).tms_utime -. u in
  Format.printf "%2.2f@." ut;
  y

let test xl =
  let module M = (val chars xl) in
  let open M in
  Internals.dump ();
  List.iter (fun (x, cap) ->
      assert (cap > 0);
      let ms = remove1 x full in
      assert (not (inclusion full ms))
    ) xl;
  iter_sub (fun ms _ -> assert (inclusion ms full)) full;
  ()

let n = int_of_string Sys.argv.(1)
let () =
  let xl = ref [] in
  for i = 1 to n do
    xl := (Char.chr (96 + i), 1 + Random.int 3) :: !xl
  done;
  time test !xl

