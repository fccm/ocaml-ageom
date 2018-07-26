(* Copyright (C) 2013 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it freely.
*)
(* Analytic Geometry *)

(* https://en.wikipedia.org/wiki/Analytic_geometry *)

(* Types *)

type point2d = float * float  (* (x, y) *)
type point3d = float * float * float  (* (x, y, z) *)

type vector2d = float * float  (* (x, y) *)
type vector3d = float * float * float  (* (x, y, z) *)

type angle = float  (* radian *)


let pi = 3.14159_26535_89793_23846_26433_83279_50288
let pi_half = pi *. 0.5
let pi_twice = pi *. 2.0


module Angle = struct

type t = angle  (* radian *)

let pi = pi
let pi_half = pi_half

let degrees_to_rad = pi /. 180.0
let rad_to_degrees = 180.0 /. pi

let of_degrees v =
  (v *. degrees_to_rad)

let to_degrees v =
  (v *. rad_to_degrees)

let rand ?(max = pi_twice) () =
  (Random.float max)

let srand s ?(max = pi_twice) () =
  (Random.State.float s max)

let overflow a =
  if a >= pi_twice
  then (mod_float a pi_twice) else
  if a < 0.0
  then pi_twice +. (mod_float a pi_twice) else
  (a)

let add a b =
  overflow (a +. b)

let sub a b =
  overflow (a -. b)

let mul a b =
  overflow (a *. b)

let div a b =
  overflow (a /. b)

let mean angles =
  let sum_sin = List.fold_left (fun sum a -> sum +. sin a) 0.0 angles
  and sum_cos = List.fold_left (fun sum a -> sum +. cos a) 0.0 angles in
  atan2 sum_sin sum_cos

module Infix = struct
  let ( + ) = add ;;
  let ( - ) = sub ;;
  let ( * ) = mul ;;
  let ( / ) = div ;;
  let ( ?. ) = rand ;;
end
end


module Point2d = struct

type t = float * float

let of_point3d (x, y, _) =
  (x, y)

let distance (x1, y1) (x2, y2) =
  let x_diff = x1 -. x2
  and y_diff = y1 -. y2 in
  sqrt (
    (x_diff *. x_diff) +.
    (y_diff *. y_diff))

(** https://en.wikipedia.org/wiki/Dot_product *)
let dot (x1, y1) (x2, y2) =
  (x1 *. x2) +.
  (y1 *. y2)

let midpoint (ax, ay) (bx, by) =
  ((ax +. bx) *. 0.5,
   (ay +. by) *. 0.5)

let rand (x1, y1) (x2, y2) =
  let x = x1 +. Random.float (x2 -. x1)
  and y = y1 +. Random.float (y2 -. y1) in
  (x, y)

let srand s (x1, y1) (x2, y2) =
  let x = x1 +. Random.State.float s (x2 -. x1)
  and y = y1 +. Random.State.float s (y2 -. y1) in
  (x, y)

let to_string (x, y) =
  Printf.sprintf "(%g, %g)" x y

let of_string s =
  Scanf.sscanf s "(%f, %f)" (fun x y -> x, y)

module Infix = struct
  let ( -|- ) = midpoint ;;
  let ( <=> ) = distance ;;
  let ( ?. ) = rand ;;
end
end


module Point3d = struct

type t = float * float * float

let of_point2d ?(z = 0.0) (x, y) =
  (x, y, z)

let distance (x1, y1, z1) (x2, y2, z2) =
  let x_diff = x1 -. x2
  and y_diff = y1 -. y2
  and z_diff = z1 -. z2 in
  sqrt (
    (x_diff *. x_diff) +.
    (y_diff *. y_diff) +.
    (z_diff *. z_diff))

let midpoint (ax, ay, az) (bx, by, bz) =
  ((ax +. bx) *. 0.5,
   (ay +. by) *. 0.5,
   (az +. bz) *. 0.5)

let rand (x1, y1, z1) (x2, y2, z2) =
  let x = x1 +. Random.float (x2 -. x1)
  and y = y1 +. Random.float (y2 -. y1)
  and z = z1 +. Random.float (z2 -. z1) in
  (x, y, z)

let srand s (x1, y1, z1) (x2, y2, z2) =
  let x = x1 +. Random.State.float s (x2 -. x1)
  and y = y1 +. Random.State.float s (y2 -. y1)
  and z = z1 +. Random.State.float s (z2 -. z1) in
  (x, y, z)

let to_string (x, y, z) =
  Printf.sprintf "(%g, %g, %g)" x y z

let of_string s =
  Scanf.sscanf s "(%f, %f, %f)" (fun x y z -> x, y, z)

module Infix = struct
  let ( -|- ) = midpoint ;;
  let ( <=> ) = distance ;;
  let ( ?. ) = rand ;;
end
end


module Vector2d = struct

type t = float * float

let of_points (ax, ay) (bx, by) =
  (bx -. ax,
   by -. ay)

let add (ax, ay) (bx, by) =
  (ax +. bx,
   ay +. by)

let sub (ax, ay) (bx, by) =
  (ax -. bx,
   ay -. by)

let mul (x, y) k =
  (x *. k,
   y *. k)

let div (x, y) k =
  (x /. k,
   y /. k)

let dot (x1, y1) (x2, y2) =
  (x1 *. x2) +.
  (y1 *. y2)

let length (x, y) =
  sqrt (x *. x +. y *. y)

let normalise ((x, y) as v) =
  let f = length v in
  if f <> 0.0
  then (x /. f, y /. f)
  else (v)

let angle (x, y) =
  (atan2 y x) -. pi_half

let rand (x, y) =
  let x = Random.float x
  and y = Random.float y in
  (x, y)

let srand s (x, y) =
  let x = Random.State.float s x
  and y = Random.State.float s y in
  (x, y)

let to_string (x, y) =
  Printf.sprintf "(%g, %g)" x y

let of_string s =
  Scanf.sscanf s "(%f, %f)" (fun x y -> x, y)

module Infix = struct
  let ( + ) = add ;;
  let ( - ) = sub ;;
  let ( * ) = mul ;;
  let ( / ) = div ;;
  let ( !. ) = dot ;;
  let ( ~. ) = normalise ;;
  let ( ?. ) = rand ;;
end


let cross (x1, y1) (x2, y2) =
  (x1 *. y2) -. (y1 *. x2)

let rot90 (x, y) =
  (y, -. x)

let rot90neg (x, y) =
  (-. y, x)


let get_line_intersection
      ~seg1:((x0, y0), (x1, y1))
      ~seg2:((x2, y2), (x3, y3)) =

  let sx1 = x1 -. x0 in
  let sy1 = y1 -. y0 in
  let sx2 = x3 -. x2 in
  let sy2 = y3 -. y2 in

  let sx3 = x0 -. x2 in
  let sy3 = y0 -. y2 in

  let d = (-. sx2 *. sy1 +. sx1 *. sy2) in

  let s = (-. sy1 *. sx3 +. sx1 *. sy3) /. d in
  let t = (   sx2 *. sy3 -. sy2 *. sx3) /. d in

  if s >= 0.0 && s <= 1.0 &&
     t >= 0.0 && t <= 1.0 then
  begin
    (* one intersection *)
    let x = x0 +. (t *. sx1)
    and y = y0 +. (t *. sy1) in
    Some(x, y)
  end
  else
    (None)  (* no intersection *)

end


module Vector3d = struct

type t = float * float * float

let add (ax, ay, az) (bx, by, bz) =
  (ax +. bx,
   ay +. by,
   az +. bz)

let sub (ax, ay, az) (bx, by, bz) =
  (ax -. bx,
   ay -. by,
   az -. bz)

let mul (x, y, z) k =
  (k *. x,
   k *. y,
   k *. z)

let div (x, y, z) k =
  (x /. k,
   y /. k,
   z /. k)

let of_points (ax, ay, az) (bx, by, bz) =
  (bx -. ax,
   by -. ay,
   bz -. az)

let dot (x1, y1, z1) (x2, y2, z2) =
  (x1 *. x2) +.
  (y1 *. y2) +.
  (z1 *. z2)

let length (x, y, z) =
  sqrt (
    x *. x +.
    y *. y +.
    z *. z)

let normalise ((x, y, z) as v) =
  let f = length v in
  if f <> 0.0 then
    (x /. f,
     y /. f,
     z /. f)
  else (v)

let cross (x1, y1, z1) (x2, y2, z2) =
  (y1 *. z2 -. z1 *. y2,
   z1 *. x2 -. x1 *. z2,
   x1 *. y2 -. y1 *. x2)

let rand (x, y, z) =
  let x = Random.float x
  and y = Random.float y
  and z = Random.float z in
  (x, y, z)

let srand s (x, y, z) =
  let x = Random.State.float s x
  and y = Random.State.float s y
  and z = Random.State.float s z in
  (x, y, z)

let to_string (x, y, z) =
  Printf.sprintf "(%g, %g, %g)" x y z

let of_string s =
  Scanf.sscanf s "(%f, %f, %f)" (fun x y z -> x, y, z)

module Infix = struct
  let ( + ) = add ;;
  let ( - ) = sub ;;
  let ( * ) = mul ;;
  let ( / ) = div ;;
  let ( !. ) = dot ;;
  let ( ~. ) = normalise ;;
  let ( ?. ) = rand ;;
end
end


module Quaternion = struct
type t = float * float * float * float

let q a b c d = (a, b, c, d)

let real (r, _, _, _) = r
let imag (_, i, j, k) = (i, j, k)

let of_scalar s = (s, 0.0, 0.0, 0.0)

let to_list (a, b, c, d) = [a; b; c; d]
let of_list = function [a; b; c; d] -> (a, b, c, d)
  | _ -> invalid_arg "Quaternion.of_list"

let ( + ) = ( +. )
let ( - ) = ( -. )
let ( * ) = ( *. )
let ( / ) = ( /. )

let addr (a, b, c, d) r = (a + r, b, c, d)
let mulr (a, b, c, d) r = (a*r, b*r, c*r, d*r)

let radd r (a, b, c, d) = (r + a, b, c, d)
let rmul r (a, b, c, d) = (r*a, r*b, r*c, r*d)

let rsub r (a, b, c, d) = (r - a, b, c, d)
let subr (a, b, c, d) r = (a - r, b, c, d)


let add (a, b, c, d) (p, q, r, s) = (a+p, b+q, c+r, d+s)

let sub (a, b, c, d) (p, q, r, s) = (a-p, b-q, c-r, d-s)

let mul (a, b, c, d) (p, q, r, s) =
  ( a*p - b*q - c*r - d*s,
    a*q + b*p + c*s - d*r,
    a*r - b*s + c*p + d*q,
    a*s + b*r - c*q + d*p )

let norm2 (a, b, c, d) =
  ( a * a +
    b * b +
    c * c +
    d * d )

let norm q =
  sqrt (norm2 q)

let conj (a, b, c, d) = (a, -. b, -. c, -. d)
let neg (a, b, c, d) = (-. a, -. b, -. c, -. d)

let unit ((a, b, c, d) as q) =
  let n = norm q in
  (a/n, b/n, c/n, d/n)

let reciprocal ((a, b, c, d) as q) =
  let n2 = norm2 q in
  (a/n2, b/n2, c/n2, d/n2)

let div q1 q2 =
  mul q1 (reciprocal q2)

let length = norm

let rand (a, b, c, d) =
  let a = Random.float a
  and b = Random.float b
  and c = Random.float c
  and d = Random.float d in
  (a, b, c, d)

let srand s (a, b, c, d) =
  let a = Random.State.float s a
  and b = Random.State.float s b
  and c = Random.State.float s c
  and d = Random.State.float s d in
  (a, b, c, d)

let to_string (a, b, c, d) =
  Printf.sprintf "(%g, %g, %g, %g)" a b c d

let of_string s =
  Scanf.sscanf s "(%f, %f, %f, %f)" (fun a b c d -> a, b, c, d)

module Infix = struct
  let ( + ) = add ;;
  let ( - ) = sub ;;
  let ( * ) = mul ;;
  let ( / ) = div ;;
  let ( ~. ) = conj ;;
  let ( |. ) = norm ;;
  let ( ?. ) = rand ;;
end
end


module Curves2d = struct
module Bezier = struct
module Linear = struct

  let pnt t (p1, p2) =
    let ti = 1.0 -. t in
    Vector2d.Infix.(
      (p1 * ti) +
      (p2 * t)
    )

  let drv t (p1, p2) =
    Vector2d.Infix.(
      (p2 - p1)
    )

  let rand a b =
    let p1 = Point2d.rand a b in
    let p2 = Point2d.rand a b in
    (p1, p2)

  let srand s a b =
    let p1 = Point2d.srand s a b in
    let p2 = Point2d.srand s a b in
    (p1, p2)

  let rand2 a b c d =
    let p1 = Point2d.rand a b in
    let p2 = Point2d.rand c d in
    (p1, p2)

end
module Quadratic = struct

  let pnt t (p1, p2, p3) =
    let ti = 1.0 -. t in
    Vector2d.Infix.(
      p1 * (ti *. ti) +
      p2 * (2.0 *. ti *. t) +
      p3 * (t *. t)
    )

  let drv t (p1, p2, p3) =
    Vector2d.Infix.(
      (p2 - p1) * (2.0 *. (1.0 -. t)) +
      (p3 - p2) * (2.0 *. t)
    )

  let rand a b =
    let p1 = Point2d.rand a b in
    let p2 = Point2d.rand a b in
    let p3 = Point2d.rand a b in
    (p1, p2, p3)

  let srand s a b =
    let p1 = Point2d.srand s a b in
    let p2 = Point2d.srand s a b in
    let p3 = Point2d.srand s a b in
    (p1, p2, p3)

  let rand3 a b c d e f =
    let p1 = Point2d.rand a b in
    let p2 = Point2d.rand c d in
    let p3 = Point2d.rand e f in
    (p1, p2, p3)

end
module Cubic = struct

  let pnt t (p1, p2, p3, p4) =
    let ti = 1.0 -. t in
    let ti2 = ti *. ti in
    let ti3 = ti *. ti2 in
    let t2 = t *. t in
    let t3 = t *. t2 in
    Vector2d.Infix.(
      p1 * (ti3) +
      p2 * (3.0 *. ti2 *. t) +
      p3 * (3.0 *. ti *. t2) +
      p4 * (t3)
    )

  let drv t (p1, p2, p3, p4) =
    let t2 = t *. t in
    let ti = 1.0 -. t in
    let ti2 = ti *. ti in
    let tit6 = 6.0 *. ti *. t in
    Vector2d.Infix.(
      p1 * (-3.0 *. ti2) + 
      p2 * (3.0 *. ti2 -. tit6) + 
      p3 * (tit6 -. 3.0 *. t2) +
      p4 * (3.0 *. t2)
    )

  let rand a b =
    let p1 = Point2d.rand a b in
    let p2 = Point2d.rand a b in
    let p3 = Point2d.rand a b in
    let p4 = Point2d.rand a b in
    (p1, p2, p3, p4)

  let srand s a b =
    let p1 = Point2d.srand s a b in
    let p2 = Point2d.srand s a b in
    let p3 = Point2d.srand s a b in
    let p4 = Point2d.srand s a b in
    (p1, p2, p3, p4)

  let rand4 a b c d e f g h =
    let p1 = Point2d.rand a b in
    let p2 = Point2d.rand c d in
    let p3 = Point2d.rand e f in
    let p4 = Point2d.rand g h in
    (p1, p2, p3, p4)
end
end


module CubicHermiteSpline = struct
(*
http://en.wikipedia.org/wiki/Cubic_Hermite_spline
*)

let m c p_pred p_succ =
  Vector2d.Infix.(
    ((p_succ - p_pred) / 2.0) * (1.0 -. c)
  )

let pnt c ~p_pred ~p0 ~p1 ~p_succ =
  let m0 = m c p_pred p1
  and m1 = m c p0 p_succ in
  function t ->
    (* this is the expanded version *)
    (* TODO: try the factorized version *)
    let t3 = t ** 3.0
    and t2 = t ** 2.0 in
    let two_t3 = 2.0 *. t3
    and two_t2 = 2.0 *. t2
    and three_t2 = 3.0 *. t2
    in
    let h00 = two_t3 -. three_t2 +. 1.0
    and h10 = t3 -. two_t2 +. t
    and h01 = -. two_t3 +. three_t2
    and h11 = t3 -. t2
    in
    Vector2d.Infix.(
      (p0 * h00) + (m0 * h10) + (p1 * h01) + (m1 * h11)
    )

end
end


module Curves3d = struct
module Bezier = struct
module Linear = struct

  let pnt t (p1, p2) =
    let ti = 1.0 -. t in
    Vector3d.Infix.(
      (p1 * ti) +
      (p2 * t)
    )

  let drv t (p1, p2) =
    Vector3d.Infix.(
      (p2 - p1)
    )

  let rand a b =
    let p1 = Point3d.rand a b in
    let p2 = Point3d.rand a b in
    (p1, p2)

  let srand s a b =
    let p1 = Point3d.srand s a b in
    let p2 = Point3d.srand s a b in
    (p1, p2)

  let rand2 a b c d =
    let p1 = Point3d.rand a b in
    let p2 = Point3d.rand c d in
    (p1, p2)

end
module Quadratic = struct

  let pnt t (p1, p2, p3) =
    let ti = 1.0 -. t in
    Vector3d.Infix.(
      p1 * (ti *. ti) +
      p2 * (2.0 *. ti *. t) +
      p3 * (t *. t)
    )

  let drv t (p1, p2, p3) =
    Vector3d.Infix.(
      (p2 - p1) * (2.0 *. (1.0 -. t)) +
      (p3 - p2) * (2.0 *. t)
    )

  let rand a b =
    let p1 = Point3d.rand a b in
    let p2 = Point3d.rand a b in
    let p3 = Point3d.rand a b in
    (p1, p2, p3)

  let srand s a b =
    let p1 = Point3d.srand s a b in
    let p2 = Point3d.srand s a b in
    let p3 = Point3d.srand s a b in
    (p1, p2, p3)

  let rand3 a b c d e f =
    let p1 = Point3d.rand a b in
    let p2 = Point3d.rand c d in
    let p3 = Point3d.rand e f in
    (p1, p2, p3)

end
module Cubic = struct

  let pnt t (p1, p2, p3, p4) =
    let ti = 1.0 -. t in
    let ti2 = ti *. ti in
    let ti3 = ti *. ti2 in
    let t2 = t *. t in
    let t3 = t *. t2 in
    Vector3d.Infix.(
      p1 * (ti3) +
      p2 * (3.0 *. ti2 *. t) +
      p3 * (3.0 *. ti *. t2) +
      p4 * (t3)
    )

  let drv t (p1, p2, p3, p4) =
    let t2 = t *. t in
    let ti = 1.0 -. t in
    let ti2 = ti *. ti in
    let tit6 = 6.0 *. ti *. t in
    Vector3d.Infix.(
      p1 * (-3.0 *. ti2) + 
      p2 * (3.0 *. ti2 -. tit6) + 
      p3 * (tit6 -. 3.0 *. t2) +
      p4 * (3.0 *. t2)
    )

  let rand a b =
    let p1 = Point3d.rand a b in
    let p2 = Point3d.rand a b in
    let p3 = Point3d.rand a b in
    let p4 = Point3d.rand a b in
    (p1, p2, p3, p4)

  let srand s a b =
    let p1 = Point3d.srand s a b in
    let p2 = Point3d.srand s a b in
    let p3 = Point3d.srand s a b in
    let p4 = Point3d.srand s a b in
    (p1, p2, p3, p4)

  let rand4 a b c d e f g h =
    let p1 = Point3d.rand a b in
    let p2 = Point3d.rand c d in
    let p3 = Point3d.rand e f in
    let p4 = Point3d.rand g h in
    (p1, p2, p3, p4)
end
end

module CubicHermiteSpline = struct
(*
http://en.wikipedia.org/wiki/Cubic_Hermite_spline
*)

let m c p_pred p_succ =
  Vector3d.Infix.(
    ((p_succ - p_pred) / 2.0) * (1.0 -. c)
  )

let pnt c ~p_pred ~p0 ~p1 ~p_succ =
  let m0 = m c p_pred p1
  and m1 = m c p0 p_succ in
  function t ->
    let t3 = t ** 3.0
    and t2 = t ** 2.0 in
    let two_t3 = 2.0 *. t3
    and two_t2 = 2.0 *. t2
    and three_t2 = 3.0 *. t2
    in
    let h00 = two_t3 -. three_t2 +. 1.0
    and h10 = t3 -. two_t2 +. t
    and h01 = -. two_t3 +. three_t2
    and h11 = t3 -. t2
    in
    Vector3d.Infix.(
      (p0 * h00) + (m0 * h10) + (p1 * h01) + (m1 * h11)
    )

end
end

