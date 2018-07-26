module Q = AGeom.Quaternion
module T = Test_it

let epsilon_float = 1.0e-8
let cmp_float = T.cmp_float ~epsilon_float ()

let test = T.test_float ~cmp:cmp_float ;;

let q1 = (1.0, 2.0, 3.0, 4.0)
let q2 = (2.0, 3.0, 4.0, 5.0)
let q3 = (3.0, 4.0, 5.0, 6.0)
let r = 7

let test1 () =
  test
    ~name:"test1 quaternion norm"
    ~result:(fun () -> Q.norm q1)
    ~should:5.47722557505166119

let test2 () =
  test
    ~name:"test2 quaternion norm"
    ~result:(fun () -> Q.norm q2)
    ~should:7.34846922834953453

let test3 () =
  test
    ~name:"test3 quaternion norm"
    ~result:(fun () -> Q.norm q3)
    ~should:9.27361849549570394

let () =
  T.run_tests [
    test1;
    test2;
    test3;
  ]

