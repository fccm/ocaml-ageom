module Agl = IntAGeom.Angle.Radian
module T = Test_it

let test = T.test_int ~cmp:T.cmp_int ;;

let test1 () =
  test
    ~name:"Test int rad angle add (simple)"
    ~result:(fun () -> Agl.add 10472 7854)
    ~should:18326

let test2 () =
  test
    ~name:"Test int rad angle add (overflow)"
    ~result:(fun () -> Agl.add 31416 41888)
    ~should:10472

let test3 () =
  test
    ~name:"Test int rad angle sub (simple)"
    ~result:(fun () -> Agl.sub 10472 7854)
    ~should:2618

let test4 () =
  test
    ~name:"Test int rad angle sub (underflow)"
    ~result:(fun () -> Agl.sub 7854 10472)
    ~should:60214

let () =
  T.run_tests [
    test1;
    test2;
    test3;
    test4;
  ]

