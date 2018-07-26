open IntAGeom
module QuadCubCurve = Curves2d.Bezier.Cubic
module Vec = Vector2d.Infix

let plot (x, y) =
  Graphics.plot x y

let circ r (x, y) =
  Graphics.draw_circle x y r

let line (x1, y1) (x2, y2) =
  Graphics.moveto x1 y1;
  Graphics.lineto x2 y2

let pp s (x, y) =
  Printf.printf "%s => x:%d y:%d\n%!" s x y

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
  while true do
    let p1, p2, p3, p4 = QuadCubCurve.rand (20, 20) (580, 380) in
    let ps = (p1, p2, p3, p4) in
    pp "p1" p1;
    pp "p2" p2;
    pp "p3" p3;
    pp "p4" p4;
    print_endline "-----------------";
    Graphics.clear_graph ();
    Graphics.set_color Graphics.blue;
    List.iter (circ 3) [p1; p2; p3; p4];
    Graphics.set_color Graphics.green;
    line p1 p2;
    line p3 p4;
    Graphics.set_color Graphics.black;
    let n = 100 in
    for i = 0 to n do
      let t = i * 10 in
      let p = QuadCubCurve.pnt ps t in
      (*
      if (i mod 10) = 0
      then pp (Printf.sprintf "t:%d" t) p;
      *)
      plot p;
      (*
      *)
      let d = QuadCubCurve.drv ps t in
      let dv = Vec.((~. d) /. 40) in
      let p2 = Vec.(p +. dv) in
      Graphics.set_color Graphics.red;
      if (i mod 10) = 0 then
      begin
        line p p2;
        pp (Printf.sprintf "@ t:%d" t) dv;
      end;
      Graphics.set_color Graphics.black;
    done;
    let k = Graphics.read_key () in
    if k = '\027' || k = 'q' then exit 0;
  done
