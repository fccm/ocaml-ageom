open IntAGeom
module QuadBezCurve = Curves2d.Bezier.Quadratic
module Vec = Vector2d.Infix

let plot (x, y) =
  Graphics.plot x y

let circ r (x, y) =
  Graphics.draw_circle x y r

let line (x1, y1) (x2, y2) =
  Graphics.moveto x1 y1;
  Graphics.lineto x2 y2

let () =
  Graphics.open_graph "";
  Graphics.resize_window 600 400;
  (*
  *)
  Random.self_init ();
  (*
  let p1 = ( 50,  50) in
  let p2 = (550, 100) in
  let p3 = (500, 350) in
  *)
  let n = 100 in
  while true do
    let p1, p2, p3 = QuadBezCurve.rand (20, 20) (580, 380) in
    let ps = (p1, p2, p3) in
    Graphics.clear_graph ();
    Graphics.set_color Graphics.blue;
    List.iter (circ 3) [p1; p2; p3];
    Graphics.set_color Graphics.green;
    line p1 p2;
    line p2 p3;
    Graphics.set_color Graphics.black;
    for i = 0 to n do
      let t = i * 10 in
      let p = QuadBezCurve.pnt ps t in
      plot p;
      (*
      *)
      let d = QuadBezCurve.drv (p1, p2, p3) t in
      let dv = Vec.((~. d) /. 40) in
      let dp = Vec.(p +. dv) in
      Graphics.set_color Graphics.red;
      if (i mod 10) = 0 then
        line p dp;
      Graphics.set_color Graphics.black;
    done;
    let k = Graphics.read_key () in
    if k = '\027' || k = 'q' then exit 0;
  done
