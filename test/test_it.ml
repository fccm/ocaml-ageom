
let exit_code = ref 0

let cmp_int (a:int) b =
  (a = b)

let cmp_float
      ?(epsilon_float =
        Pervasives.epsilon_float)
      () =
  fun a b ->
    (abs_float (a -. b)) <= epsilon_float

let report_error_int ~name ~result ~should =
  Printf.eprintf "%s: ERROR\n" name;
  Printf.eprintf " result: %d\n" result;
  Printf.eprintf " should: %d\n" should;
;;

let report_error_float ~name ~result ~should =
  Printf.eprintf "%s: ERROR\n" name;
  Printf.eprintf " result: %g\n" result;
  Printf.eprintf " should: %g\n" should;
;;

let test_int ~cmp ~name ~result ~should =
  let result = result () in
  if cmp should result
  then Printf.printf "%s: OK\n%!" name
  else begin
    exit_code := 1;
    report_error_int ~name ~result ~should
  end

let test_float ~cmp ~name ~result ~should =
  let result = result () in
  if cmp should result
  then Printf.printf "%s: OK\n%!" name
  else begin
    exit_code := 1;
    report_error_float ~name ~result ~should
  end

let test_int ~cmp ~name ~result ~should =
  try test_int ~cmp ~name ~result ~should
  with e ->
    Printf.eprintf "%s: FAILED\n" name;
    Printf.eprintf " exception: %s\n"
      (Printexc.to_string e)

let test_float ~cmp ~name ~result ~should =
  try test_float ~cmp ~name ~result ~should
  with e ->
    Printf.eprintf "%s: FAILED\n" name;
    Printf.eprintf " exception: %s\n"
      (Printexc.to_string e)

let run_tests tests =
  List.iter (fun test -> test ()) tests;
  exit !exit_code;
;;

