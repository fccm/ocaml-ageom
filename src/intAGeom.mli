(* Copyright (C) 2013 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it freely.
*)
(** Analytic Geometry with ints *)
(** Read the
    {{:https://en.wikipedia.org/wiki/Analytic_geometry}
    Wikipedia article about
    Analytic Geometry}
*)

(** {3 Types } *)

type point2d = int * int  (** (x, y) *)
type point3d = int * int * int  (** (x, y, z) *)

type vector2d = int * int  (** (x, y) *)
type vector3d = int * int * int  (** (x, y, z) *)

type rad_angle = int  (** radian *)
type deg_angle = int  (** degree *)


(** {2 Modules } *)

module Angle : sig
  module Radian : sig
    type t = rad_angle  (* radian *)

    val pi : t  (* (pi * 10_000) *)
    val pi_half : t  (* (pi * 10_000) / 2 *)
    val pi_twice : t  (* (pi * 10_000) * 2 *)

    val of_degrees : deg_angle -> t
    val to_degrees : t -> deg_angle

    val of_radians : t -> t
    val to_radians : t -> t

    val bounded : t -> t

    val add : t -> t -> t
    (** [a + b] *)

    val sub : t -> t -> t
    (** [a - b] *)

    val mul : t -> int -> t
    (** [v * k] *)

    val div : t -> int -> t
    (** [v / k] *)

    val rand : ?max:t -> unit -> t

    val mean : t list -> t

    module Infix : sig
      val ( +. ) : t -> t -> t
      val ( -. ) : t -> t -> t
      val ( *. ) : t -> int -> t
      val ( /. ) : t -> int -> t
      val ( ?. ) : ?max:t -> unit -> t
      (** equivalent to [rand] *)
    end
  end
  module Degree : sig
    type t = deg_angle  (* degree *)

    val pi : t  (*  *)
    val pi_half : t  (*  *)
    val pi_twice : t  (*  *)

    val of_degrees : t -> t
    val to_degrees : t -> t

    val of_radians : rad_angle -> t
    val to_radians : t -> rad_angle

    val bounded : t -> t

    val add : t -> t -> t
    (** [a + b] *)

    val sub : t -> t -> t
    (** [a - b] *)

    val mul : t -> int -> t
    (** [v * k] *)

    val div : t -> int -> t
    (** [v / k] *)

    val rand : ?max:t -> unit -> t

    val mean : t list -> t

    module Infix : sig
      val ( +. ) : t -> t -> t
      val ( -. ) : t -> t -> t
      val ( *. ) : t -> int -> t
      val ( /. ) : t -> int -> t
      val ( ?. ) : ?max:t -> unit -> t
      (** equivalent to [rand] *)
    end
  end
end


module Point2d : sig
  type t = point2d  (** (x, y) *)

  val of_point3d : point3d -> t

  val sq_dist : t -> t -> int
  (** Square of the distance between two point *)

  val midpoint : t -> t -> t
  (** [Point2d.midpoint a b] returns the middle point of
      the line segment [ab]
      ({{:https://en.wikipedia.org/wiki/Midpoint}wp}) *)

  val dot : t -> t -> int
  (** {{:https://en.wikipedia.org/wiki/Dot_product}
        Dot product on the Wiki} *)

  val rand : t -> t -> t
  (** [Point2d.rand a b] returns a point with coords
      between those of [a] and [b] *)

  module Infix : sig
    val ( -|- ) : t -> t -> t
    (** equivalent to [midpoint] *)

    val ( <=> ) : t -> t -> int
    (** equivalent to [sq_dist] *)

    val ( ?. ) : t -> t -> t
    (** equivalent to [rand] *)
  end
end


module Point3d : sig
  type t = point3d  (** (x, y, z) *)

  val of_point2d : ?z:int -> point2d -> t

  val sq_dist : t -> t -> int
  (** Square of the distance between two point *)

  val midpoint : t -> t -> t
  (** [Point3d.midpoint a b] returns the middle point of
      the line segment [ab]
      ({{:https://en.wikipedia.org/wiki/Midpoint}wp}) *)

  val rand : t -> t -> t
  (** [Point3d.rand a b] returns a point with coords
      between those of [a] and [b] *)

  module Infix : sig
    val ( -|- ) : t -> t -> t
    (** equivalent to [midpoint] *)

    val ( <=> ) : t -> t -> int
    (** equivalent to [distance] *)

    val ( ?. ) : t -> t -> t
    (** equivalent to [rand] *)
  end
end


module Vector2d : sig
  type t = vector2d  (** (x, y) *)

  val of_points : Point2d.t -> Point2d.t -> t
  (** [Vector2d.of_points a b] returns the vector [ab]
      from [a] to [b]. *)

  val add : t -> t -> t
  (** [a + b] *)

  val sub : t -> t -> t
  (** [a - b] *)

  val mul : t -> int -> t
  (** [v * k] *)

  val div : t -> int -> t
  (** [v / k] *)

  val dot : t -> t -> int
  (** Dot product of two vectors *)

  val length : t -> int
  (** Returns the length (magnitude) of this vector. *)

  val normalise : t -> t
  (** Normalises this vector *)

  (*
  val angle : t -> Angle.t
  (** Returns the angle of this vector. *)
  *)

  module Infix : sig
    val ( +. ) : t -> t -> t
    val ( -. ) : t -> t -> t
    val ( *. ) : t -> int -> t
    val ( /. ) : t -> int -> t

    val ( !. ) : t -> t -> int
    (** equivalent to [dot] *)

    val ( ~. ) : t -> t
    (** equivalent to [normalise] *)
  end
end


module Vector3d : sig
  type t = vector3d  (** (x, y, z) *)

  val of_points : Point3d.t -> Point3d.t -> t
  (** [Vector3d.of_points a b] returns the vector [ab]
      from [a] to [b]. *)

  val add : t -> t -> t
  (** [a + b] *)

  val sub : t -> t -> t
  (** [a - b] *)

  val mul : t -> int -> t
  (** [v * k] *)

  val div : t -> int -> t
  (** [v / k] *)

  val dot : t -> t -> int
  (** Dot product of two vectors *)

  val sq_length : t -> int
  (** Returns the square of the length of this vector. *)

  val normalise : t -> t
  (** Normalises this vector *)

  val cross : t -> t -> t
  (** Calculate the cross product between two vectors. *)

  module Infix : sig
    val ( +. ) : t -> t -> t
    val ( -. ) : t -> t -> t
    val ( *. ) : t -> int -> t
    val ( /. ) : t -> int -> t

    val ( !. ) : t -> t -> int
    (** equivalent to [dot] *)

    val ( ~. ) : t -> t
    (** equivalent to [normalise] *)
  end
end


module Curves2d : sig
  module Bezier : sig
    (** {{:http://en.wikipedia.org/wiki/B%C3%A9zier_curve}
        Bezier Curve on Wikipedia} *)

    module Linear : sig

      val interval : int * int
      (** The interval for interpolation is [(0, 1000)]
          instead of [(0.0, 1.0)] for [floats]. *)

      val pnt :
        Point2d.t * Point2d.t ->
        int -> Point2d.t

      val drv :
        Point2d.t * Point2d.t ->
        int -> Vector2d.t

      val rand :
        Point2d.t -> Point2d.t ->
        Point2d.t * Point2d.t

    end
    module Quadratic : sig

      val interval : int * int
      (** The interval for interpolation is [(0, 1000)]
          instead of [(0.0, 1.0)] for [floats]. *)

      val pnt :
        Point2d.t * Point2d.t * Point2d.t ->
        int -> Point2d.t

      val drv :
        Point2d.t * Point2d.t * Point2d.t ->
        int -> Vector2d.t

      val rand :
        Point2d.t -> Point2d.t ->
        Point2d.t * Point2d.t * Point2d.t

    end
    module Cubic : sig

      val interval : int * int
      (** The interval for interpolation is [(0, 1000)]
          instead of [(0.0, 1.0)] for [floats]. *)

      val pnt :
        Point2d.t * Point2d.t * Point2d.t * Point2d.t ->
        int -> Point2d.t

      val drv :
        Point2d.t * Point2d.t * Point2d.t * Point2d.t ->
        int -> Vector2d.t

      val rand :
        Point2d.t -> Point2d.t ->
        Point2d.t * Point2d.t * Point2d.t * Point2d.t

    end
  end
end


module Curves3d : sig
  module Bezier : sig
    (** {{:http://en.wikipedia.org/wiki/B%C3%A9zier_curve}
        Bezier Curve on Wikipedia} *)

    module Linear : sig

      val interval : int * int
      (** The interval for interpolation is [(0, 1000)]
          instead of [(0.0, 1.0)] for [floats]. *)

      val pnt :
        Point3d.t * Point3d.t ->
        int -> Point3d.t

      val drv :
        Point3d.t * Point3d.t ->
        int -> Vector3d.t

      val rand :
        Point3d.t -> Point3d.t ->
        Point3d.t * Point3d.t

    end
    module Quadratic : sig

      val interval : int * int
      (** The interval for interpolation is [(0, 1000)]
          instead of [(0.0, 1.0)] for [floats]. *)

      val pnt :
        Point3d.t * Point3d.t * Point3d.t ->
        int -> Point3d.t

      val drv :
        Point3d.t * Point3d.t * Point3d.t ->
        int -> Vector3d.t

      val rand :
        Point3d.t -> Point3d.t ->
        Point3d.t * Point3d.t * Point3d.t

    end
    module Cubic : sig

      val interval : int * int
      (** The interval for interpolation is [(0, 1000)]
          instead of [(0.0, 1.0)] for [floats]. *)

      val pnt :
        Point3d.t * Point3d.t * Point3d.t * Point3d.t ->
        int -> Point3d.t

      val drv :
        Point3d.t * Point3d.t * Point3d.t * Point3d.t ->
        int -> Vector3d.t

      val rand :
        Point3d.t -> Point3d.t ->
        Point3d.t * Point3d.t * Point3d.t * Point3d.t

    end
  end
end

