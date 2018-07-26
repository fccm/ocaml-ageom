(* Copyright (C) 2013 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it freely.
*)
(* Analytic Geometry *)

(* https://en.wikipedia.org/wiki/Analytic_geometry *)

open IntMath

(* Types *)

type point2d = int * int  (* (x, y) *)
type point3d = int * int * int  (* (x, y, z) *)

type vector2d = int * int  (* (x, y) *)
type vector3d = int * int * int  (* (x, y, z) *)

type rad_angle = int  (* radian * 10_000 *)
type deg_angle = int  (* degree * 10_000 *)


module Angle = struct
module Degree = struct

(** Angles in
    {{:http://en.wikipedia.org/wiki/Degree_%28angle%29}degree} *)

type t = deg_angle  (* degree *)

let pi = 1800  (* 180 * 10 *)
let pi_half = 900  (*  90 * 10*)
let pi_twice = 3600  (* 360 * 10 *)


let of_degrees v = v
let to_degrees v = v

let of_radians v =
  (v * 1800 / 31416)

let to_radians v =
  (v * 31416 / 1800)

let rand ?(max = pi_twice) () =
  (Random.int max)

let srand s ?(max = pi_twice) () =
  (Random.State.int s max)

let bounded a =
  if a >= pi_twice
  then (a mod pi_twice) else
  if a < 0
  then pi_twice + (a mod pi_twice) else
  (a)

let add a b =
  bounded (a + b)

let sub a b =
  bounded (a - b)

let mul a b =
  bounded (a * b)

let div a b =
  bounded (a / b)

let mean angles =
  let sum_sin = List.fold_left (fun sum a -> sum + sin a) 0 angles
  and sum_cos = List.fold_left (fun sum a -> sum + cos a) 0 angles in
  atan2 sum_sin sum_cos

  module Infix = struct
    let ( +. ) = add ;;
    let ( -. ) = sub ;;
    let ( *. ) = mul ;;
    let ( /. ) = div ;;
    let ( ?. ) = rand ;;
  end
end
module Radian = struct

(** Angles in
    {{:http://en.wikipedia.org/wiki/Radian}radian} *)

type t = rad_angle  (* radian *)

let pi = 31416  (* (pi * 10_000) *)
let pi_half = 15708  (* (pi * 10_000) / 2 *)
let pi_twice = 62832  (* (pi * 10_000) * 2 *)


let of_degrees v =
  (v * pi / 180 / 10_000)

let to_degrees v =
  (v * 180 / pi / 10_000)

let of_radians v = v
let to_radians v = v

let rand ?(max = pi_twice) () =
  (Random.int max)

let srand s ?(max = pi_twice) () =
  (Random.State.int s max)

let bounded a =
  if a >= pi_twice
  then (a mod pi_twice) else
  if a < 0
  then pi_twice + (a mod pi_twice) else
  (a)

let add a b =
  bounded (a + b)

let sub a b =
  bounded (a - b)

let mul a b =
  bounded (a * b)

let div a b =
  bounded (a / b)

let mean angles =
  let sum_sin = List.fold_left (fun sum a -> sum + sin a) 0 angles
  and sum_cos = List.fold_left (fun sum a -> sum + cos a) 0 angles in
  atan2 sum_sin sum_cos

  module Infix = struct
    let ( +. ) = add ;;
    let ( -. ) = sub ;;
    let ( *. ) = mul ;;
    let ( /. ) = div ;;
    let ( ?. ) = rand ;;
  end
end
end


module Point2d = struct

type t = point2d

let of_point3d (x, y, _) =
  (x, y)

let sq_dist (x1, y1) (x2, y2) =
  let x_diff = x1 - x2
  and y_diff = y1 - y2 in
  ( (x_diff * x_diff) +
    (y_diff * y_diff) )

let distance p1 p2 =
  sqrt (sq_dist p1 p2)

(** https://en.wikipedia.org/wiki/Dot_product *)
let dot (x1, y1) (x2, y2) =
  (x1 * x2) +
  (y1 * y2)

let midpoint (ax, ay) (bx, by) =
  ((ax + bx) / 2,
   (ay + by) / 2)

let rand (x1, y1) (x2, y2) =
  let x = x1 + Random.int (x2 - x1)
  and y = y1 + Random.int (y2 - y1) in
  (x, y)

let srand s (x1, y1) (x2, y2) =
  let x = x1 + Random.State.int s (x2 - x1)
  and y = y1 + Random.State.int s (y2 - y1) in
  (x, y)

  module Infix = struct
    let ( -|- ) = midpoint ;;
    let ( <=> ) = distance ;;
    let ( ?. ) = rand ;;
  end
end


module Point3d = struct

type t = point3d

let of_point2d ?(z = 0) (x, y) =
  (x, y, z)

let sq_dist (x1, y1, z1) (x2, y2, z2) =
  let x_diff = x1 - x2
  and y_diff = y1 - y2
  and z_diff = z1 - z2 in
  ( (x_diff * x_diff) +
    (y_diff * y_diff) +
    (z_diff * z_diff) )

let distance p1 p2 =
  sqrt (sq_dist p1 p2)

let midpoint (ax, ay, az) (bx, by, bz) =
  ((ax + bx) / 2,
   (ay + by) / 2,
   (az + bz) / 2)

let rand (x1, y1, z1) (x2, y2, z2) =
  let x = x1 + Random.int (x2 - x1)
  and y = y1 + Random.int (y2 - y1)
  and z = z1 + Random.int (z2 - z1) in
  (x, y, z)

let srand s (x1, y1, z1) (x2, y2, z2) =
  let x = x1 + Random.State.int s (x2 - x1)
  and y = y1 + Random.State.int s (y2 - y1)
  and z = z1 + Random.State.int s (z2 - z1) in
  (x, y, z)

  module Infix = struct
    let ( -|- ) = midpoint ;;
    let ( <=> ) = distance ;;
    let ( ?. ) = rand ;;
  end
end


module Vector2d = struct

type t = vector2d

let of_points (ax, ay) (bx, by) =
  (bx - ax,
   by - ay)

let add (ax, ay) (bx, by) =
  (ax + bx,
   ay + by)

let sub (ax, ay) (bx, by) =
  (ax - bx,
   ay - by)

let mul (x, y) k =
  (x * k,
   y * k)

let div (x, y) k =
  (x / k,
   y / k)

let dot (x1, y1) (x2, y2) =
  (x1 * x2) +
  (y1 * y2)

let sq_length (x, y) =
  (x * x + y * y)

let length p =
  sqrt (sq_length p)

let normalise ((x, y) as v) =
  let f = length v in
  if f <> 0
  then ((x * 1000) / f, (y * 1000) / f)
  else (v)

let rand (x, y) =
  let x = Random.int x
  and y = Random.int y in
  (x, y)

let srand s (x, y) =
  let x = Random.State.int s x
  and y = Random.State.int s y in
  (x, y)

  module Infix = struct
    let ( +. ) = add ;;
    let ( -. ) = sub ;;
    let ( *. ) = mul ;;
    let ( /. ) = div ;;
    let ( !. ) = dot ;;
    let ( ~. ) = normalise ;;
    let ( ?. ) = rand ;;
  end
end


module Vector3d = struct

type t = vector3d

let add (ax, ay, az) (bx, by, bz) =
  (ax + bx,
   ay + by,
   az + bz)

let sub (ax, ay, az) (bx, by, bz) =
  (ax - bx,
   ay - by,
   az - bz)

let mul (x, y, z) k =
  (x * k,
   y * k,
   z * k)

let div (x, y, z) k =
  (x / k,
   y / k,
   z / k)

let of_points (ax, ay, az) (bx, by, bz) =
  (bx - ax,
   by - ay,
   bz - az)

let dot (x1, y1, z1) (x2, y2, z2) =
  (x1 * x2) +
  (y1 * y2) +
  (z1 * z2)

let sq_length (x, y, z) =
  (x * x +
   y * y +
   z * z)

let length v =
  sqrt (sq_length v)

let normalise ((x, y, z) as v) =
  let f = length v in
  if f <> 0 then
    ((x * 1000) / f,
     (y * 1000) / f,
     (z * 1000) / f)
  else (v)

let cross (x1, y1, z1) (x2, y2, z2) =
  (y1 * z2 - z1 * y2,
   z1 * x2 - x1 * z2,
   x1 * y2 - y1 * x2)

let rand (x, y, z) =
  let x = Random.int x
  and y = Random.int y
  and z = Random.int z in
  (x, y, z)

let srand s (x, y, z) =
  let x = Random.State.int s x
  and y = Random.State.int s y
  and z = Random.State.int s z in
  (x, y, z)

  module Infix = struct
    let ( +. ) = add ;;
    let ( -. ) = sub ;;
    let ( *. ) = mul ;;
    let ( /. ) = div ;;
    let ( !. ) = dot ;;
    let ( ~. ) = normalise ;;
    let ( ?. ) = rand ;;
  end
end


let interval = (0, 1000)

module Curves2d = struct
module Bezier = struct
module Linear = struct
  let interval = interval

  let pnt (p1, p2) t =
    let ti = 1000 - t in
    Vector2d.Infix.(
      ( (p1 *. ti) +.
        (p2 *. t)
      ) /. 1000
    )

  let drv (p1, p2) t =
    Vector2d.Infix.(
      (p2 -. p1)
    )

  let rand a b =
    let p1 = Point2d.rand a b in
    let p2 = Point2d.rand a b in
    (p1, p2)

end
module Quadratic = struct
  let interval = interval

  let pnt (p1, p2, p3) t =
    let ti = 1000 - t in
    Vector2d.Infix.(
      ( p1 *. ((ti * ti) / 1000) +.
        p2 *. ((2 * ti * t) / 1000) +.
        p3 *. ((t * t) / 1000)
      ) /. 1000
    )

  let drv (p1, p2, p3) t =
    Vector2d.Infix.(
      ( (p2 -. p1) *. (2 * (1000 - t)) +.
        (p3 -. p2) *. (2 * t)
      ) /. 1000
    )

  let rand a b =
    let p1 = Point2d.rand a b in
    let p2 = Point2d.rand a b in
    let p3 = Point2d.rand a b in
    (p1, p2, p3)

end
module Cubic = struct
  let interval = interval

  let pnt (p1, p2, p3, p4) t =
    let ti = 1000 - t in
    let ti2 = ti * ti in
    let ti3 = ti * ti2 in
    let t2 = t * t in
    let t3 = t * t2 in
    Vector2d.Infix.(
      ( p1 *. (ti3 / 1000_000) +.
        p2 *. ((3 * ti2 * t) / 1000_000) +.
        p3 *. ((3 * ti * t2) / 1000_000) +.
        p4 *. (t3 / 1000_000)
      ) /. 1000
    )

  let drv (p1, p2, p3, p4) t =
    let t2 = t * t in
    let ti = 1000 - t in
    let ti2 = ti * ti in
    let tit6 = 6 * ti * t in
    Vector2d.Infix.(
      ( p1 *. ((-3 * ti2) / 1000) +.
        p2 *. ((3 * ti2 - tit6) / 1000) +.
        p3 *. ((tit6 - 3 * t2) / 1000) +.
        p4 *. ((3 * t2) / 1000)
      ) /. 1000
    )

  let rand a b =
    let p1 = Point2d.rand a b in
    let p2 = Point2d.rand a b in
    let p3 = Point2d.rand a b in
    let p4 = Point2d.rand a b in
    (p1, p2, p3, p4)
end
end
end


module Curves3d = struct
module Bezier = struct
module Linear = struct
  let interval = interval

  let pnt (p1, p2) t =
    let ti = 1000 - t in
    Vector3d.Infix.(
      ( (p1 *. ti) +.
        (p2 *. t)
      ) /. 1000
    )

  let drv (p1, p2) t =
    Vector3d.Infix.(
      (p2 -. p1)
    )

  let rand a b =
    let p1 = Point3d.rand a b in
    let p2 = Point3d.rand a b in
    (p1, p2)

end
module Quadratic = struct
  let interval = interval

  let pnt (p1, p2, p3) t =
    let ti = 1000 - t in
    Vector3d.Infix.(
      ( p1 *. ((ti * ti) / 1000) +.
        p2 *. ((2 * ti * t) / 1000) +.
        p3 *. ((t * t) / 1000)
      ) /. 1000
    )

  let drv (p1, p2, p3) t =
    Vector3d.Infix.(
      ( (p2 -. p1) *. (2 * (1000 - t)) +.
        (p3 -. p2) *. (2 * t)
      ) /. 1000
    )

  let rand a b =
    let p1 = Point3d.rand a b in
    let p2 = Point3d.rand a b in
    let p3 = Point3d.rand a b in
    (p1, p2, p3)

end
module Cubic = struct
  let interval = interval

  let pnt (p1, p2, p3, p4) t =
    let ti = 1000 - t in
    let ti2 = ti * ti in
    let ti3 = ti * ti2 in
    let t2 = t * t in
    let t3 = t * t2 in
    Vector3d.Infix.(
      ( p1 *. (ti3 / 1000_000) +.
        p2 *. ((3 * ti2 * t) / 1000_000) +.
        p3 *. ((3 * ti * t2) / 1000_000) +.
        p4 *. (t3)
      ) /. 1000
    )

  let drv (p1, p2, p3, p4) t =
    let t2 = t * t in
    let ti = 1000 - t in
    let ti2 = ti * ti in
    let tit6 = 6 * ti * t in
    Vector3d.Infix.(
      ( p1 *. ((-3 * ti2) / 1000) +.
        p2 *. ((3 * ti2 - tit6) / 1000) +.
        p3 *. ((tit6 - 3 * t2) / 1000) +.
        p4 *. ((3 * t2) / 1000)
      ) /. 1000
    )

  let rand a b =
    let p1 = Point3d.rand a b in
    let p2 = Point3d.rand a b in
    let p3 = Point3d.rand a b in
    let p4 = Point3d.rand a b in
    (p1, p2, p3, p4)
end
end
end

