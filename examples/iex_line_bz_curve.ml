open IntAGeom
module LineBezCurve = Curves2d.Bezier.Linear
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
  Random.self_init ();
  let n = 100 in
  while true do
    let p1, p2 = LineBezCurve.rand (20, 20) (580, 380) in
    let ps = (p1, p2) in
    Graphics.clear_graph ();
    Graphics.set_color Graphics.blue;
    List.iter (circ 3) [p1; p2];
    Graphics.set_color Graphics.black;
    for i = 0 to n do
      let t = i * 10 in
      let p = LineBezCurve.pnt ps t in
      let d = LineBezCurve.drv (p1, p2) t in
      let dv = Vec.((~. d) /. 40) in
      let dp = Vec.(p +. dv) in
      Graphics.set_color Graphics.red;
      if (i mod 10) = 0 then (
        line p dp;
        Printf.printf " len: %d\n%!" (Vector2d.length (Vector2d.of_points p dp));
      );
      Graphics.set_color Graphics.black;
    done;
    Printf.printf "\n%!";
    let k = Graphics.read_key () in
    if k = '\027' || k = 'q' then exit 0;
  done
