module Agl = IntAGeom.Angle.Degree
module T = Test_it

let test = T.test_int ~cmp:T.cmp_int ;;

let test1 () =
  test
    ~name:"Test int rad angle add (simple)"
    ~result:(fun () -> Agl.add 30_0 20_0)
    ~should:50_0

let test2 () =
  test
    ~name:"Test int rad angle add (overflow)"
    ~result:(fun () -> Agl.add 340_0 80_0)
    ~should:60_0

let () =
  T.run_tests [
    test1;
    test2;
  ]

