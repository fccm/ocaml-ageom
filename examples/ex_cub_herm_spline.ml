open AGeom
module CubHermSpline = Curves2d.CubicHermiteSpline
module Vec = Vector2d.Infix

let plot (x, y) =
  let x = int_of_float x in
  let y = int_of_float y in
  Graphics.plot x y

let circ r (x, y) =
  let x = int_of_float x in
  let y = int_of_float y in
  Graphics.draw_circle x y r

let line (x1, y1) (x2, y2) =
  let x1 = int_of_float x1 in
  let y1 = int_of_float y1 in
  let y2 = int_of_float y2 in
  let x2 = int_of_float x2 in
  Graphics.moveto x1 y1;
  Graphics.lineto x2 y2

let rec drop_last = function
  | [_] | [] -> []
  | x::xs -> x :: drop_last xs

let () =
  Graphics.open_graph "";
  Graphics.resize_window 600 400;
  (*
  let p1 = (20., 40.) in
  let p2 = (80., 40.) in
  let p3 = (140., 280.) in
  let p4 = (240., 140.) in
  let p5 = (280., 240.) in
  let p6 = (380., 240.) in
  let ps = ref [p1; p2; p3; p4; p5; p6] in
  *)
  Random.self_init ();
  let p1 = Point2d.rand (20., 20.) (580., 380.) in
  let p2 = Point2d.rand (20., 20.) (580., 380.) in
  let p3 = Point2d.rand (20., 20.) (580., 380.) in
  let p4 = Point2d.rand (20., 20.) (580., 380.) in
  let p5 = Point2d.rand (20., 20.) (580., 380.) in
  let p6 = Point2d.rand (20., 20.) (580., 380.) in
  let p7 = Point2d.rand (20., 20.) (580., 380.) in
  let ps = ref [p1; p2; p3; p4; p5; p6; p7] in

  let rec aux f = function
    | p1::((p2::p3::p4::_) as tail) ->
        f p1 p2 p3 p4;
        aux f tail
    | _ -> ()
  in
  let n = 60 in
  while true do
    Graphics.clear_graph ();
    Graphics.set_color Graphics.blue;
    List.iter (circ 3) !ps;
    Graphics.set_color Graphics.yellow;
    aux (fun p1 p2 p3 p4 ->
      line p1 p2;
      line p3 p4;
    ) !ps;
    Graphics.set_color Graphics.black;
    for i = 0 to n do
      let t = float i /. float n in
      aux (fun p1 p2 p3 p4 ->
        let p =
          CubHermSpline.pnt 0.0
            ~p_pred:p1 ~p0:p2
            ~p_succ:p4 ~p1:p3
            t
        in
        plot p;
      ) !ps;
      Graphics.set_color Graphics.black;
    done;
    let p = Point2d.rand (20., 20.) (580., 380.) in
    ps := p :: (drop_last !ps);
    let k = Graphics.read_key () in
    if k = '\027' || k = 'q' then exit 0;
  done
