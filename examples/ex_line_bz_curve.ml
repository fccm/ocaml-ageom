open AGeom
module LineBezCurve = Curves2d.Bezier.Linear

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
  Random.self_init ();
  let n = 100 in
  while true do
    let p1, p2 = LineBezCurve.rand (20., 20.) (580., 380.) in
    Graphics.clear_graph ();
    Graphics.set_color Graphics.blue;
    List.iter (circ 3) [p1; p2];
    Graphics.set_color Graphics.green;
    line p1 p2;
    Graphics.set_color Graphics.black;
    for i = 0 to n do
      let t = float i /. float n in
      let p = LineBezCurve.pnt t (p1, p2) in
      plot p;
      Graphics.set_color Graphics.black;
    done;
    let k = Graphics.read_key () in
    if k = '\027' || k = 'q' then exit 0;
  done
