open AGeom
module QuadCubCurve = Curves2d.Bezier.Cubic
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

let () =
  Graphics.open_graph "";
  Graphics.resize_window 600 400;
  (*
  let p1 = (40., 40.) in
  let p2 = (100., 320.) in
  let p3 = (260.,  40.) in
  let p4 = (240., 240.) in
  *)
  Random.self_init ();
  let n = 50 in
  while true do
    let p1, p2, p3, p4 = QuadCubCurve.rand (20., 20.) (580., 380.) in
    Graphics.clear_graph ();
    Graphics.set_color Graphics.blue;
    List.iter (circ 3) [p1; p2; p3; p4];
    Graphics.set_color Graphics.green;
    line p1 p2;
    line p3 p4;
    Graphics.set_color Graphics.black;
    for i = 0 to n do
      let t = float i /. float n in
      let p = QuadCubCurve.pnt t (p1, p2, p3, p4) in
      let d = QuadCubCurve.drv t (p1, p2, p3, p4) in
      let dv = Vec.((~. d) * 20.0) in
      let p2 = Vec.(p + dv) in
      plot p;
      Graphics.set_color Graphics.red;
      if (i mod 10) = 0 then
        line p p2;
      Graphics.set_color Graphics.black;
    done;
    let k = Graphics.read_key () in
    if k = '\027' || k = 'q' then exit 0;
  done
