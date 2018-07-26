(* Copyright (C) 2013 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it freely.
*)
(** Analytic Geometry *)
(** Read the
    {{:https://en.wikipedia.org/wiki/Analytic_geometry}
    Wikipedia article about
    Analytic Geometry}
*)

(** {3 Types } *)

type point2d = float * float  (** (x, y) *)
type point3d = float * float * float  (** (x, y, z) *)

type vector2d = float * float  (** (x, y) *)
type vector3d = float * float * float  (** (x, y, z) *)

type angle = float  (** radian *)

(** {2 Modules } *)

module Angle : sig
  type t = angle  (* radian *)
  val pi : float

  val of_degrees : float -> t
  val to_degrees : t -> float

  val add : t -> t -> t
  (** [a + b] *)

  val sub : t -> t -> t
  (** [a - b] *)

  val mul : t -> float -> t
  (** [v * k] *)

  val div : t -> float -> t
  (** [v / k] *)

  val rand : ?max:t -> unit -> t

  val srand : Random.State.t -> ?max:t -> unit -> t

  val mean : t list -> t

  module Infix : sig
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> float -> t
    val ( / ) : t -> float -> t
    val ( ?. ) : ?max:t -> unit -> t
    (** equivalent to [rand] *)
  end
end


module Point2d : sig
  type t = point2d  (** (x, y) *)

  val of_point3d : point3d -> t

  val distance : t -> t -> float
  (** Distance between two points *)

  val midpoint : t -> t -> t
  (** [Point2d.midpoint a b] returns the middle point of
      the line segment [ab]
      ({{:https://en.wikipedia.org/wiki/Midpoint}wp}) *)

  val dot : t -> t -> float
  (** {{:https://en.wikipedia.org/wiki/Dot_product}
        Dot product on the Wiki} *)

  val rand : t -> t -> t
  (** [Point2d.rand a b] returns a point with coords
      between those of [a] and [b] *)

  val srand : Random.State.t -> t -> t -> t

  val to_string : t -> string
  val of_string : string -> t

  module Infix : sig
    val ( -|- ) : t -> t -> t
    (** equivalent to [midpoint] *)

    val ( <=> ) : t -> t -> float
    (** equivalent to [distance] *)

    val ( ?. ) : t -> t -> t
    (** equivalent to [rand] *)
  end
end


module Point3d : sig
  type t = point3d  (** (x, y, z) *)

  val of_point2d : ?z:float -> point2d -> t

  val distance : t -> t -> float
  (** Distance between two point *)

  val midpoint : t -> t -> t
  (** [Point3d.midpoint a b] returns the middle point of
      the line segment [ab]
      ({{:https://en.wikipedia.org/wiki/Midpoint}wp}) *)

  val rand : t -> t -> t
  (** [Point3d.rand a b] returns a point with coords
      between those of [a] and [b] *)

  val srand : Random.State.t -> t -> t -> t

  val to_string : t -> string
  val of_string : string -> t

  module Infix : sig
    val ( -|- ) : t -> t -> t
    (** equivalent to [midpoint] *)

    val ( <=> ) : t -> t -> float
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

  val mul : t -> float -> t
  (** [v * k] *)

  val div : t -> float -> t
  (** [v / k] *)

  val dot : t -> t -> float
  (** Dot product of two vectors *)

  val length : t -> float
  (** Returns the length (magnitude) of this vector. *)

  val normalise : t -> t
  (** Normalises this vector *)

  val angle : t -> Angle.t
  (** Returns the angle of this vector. *)

  (* TODO: check this one *)
  val cross : t -> t -> float
  (** cross-product *)

  val rot90 : t -> t
  (** rotates by 90 degrees *)

  val rot90neg : t -> t
  (** rotates by minus 90 degrees *)

  val get_line_intersection :
    seg1:t * t ->
    seg2:t * t -> t option
  (** Returns the point where the lines intersect if any intersection,
      [None] otherwise. *)

  val rand : float * float -> t
  (** return a random 2D vector *)

  val srand : Random.State.t -> float * float -> t

  val to_string : t -> string
  val of_string : string -> t

  module Infix : sig
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> float -> t
    val ( / ) : t -> float -> t

    val ( !. ) : t -> t -> float
    (** equivalent to [dot] *)

    val ( ~. ) : t -> t
    (** equivalent to [normalise] *)

    val ( ?. ) : float * float -> t
    (** equivalent to [Vector2d.rand] *)
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

  val mul : t -> float -> t
  (** [v * k] *)

  val div : t -> float -> t
  (** [v / k] *)

  val dot : t -> t -> float
  (** Dot product of two vectors *)

  val length : t -> float
  (** Returns the length (magnitude) of this vector. *)

  val normalise : t -> t
  (** Normalises this vector *)

  val cross : t -> t -> t
  (** Calculate the cross product between two vectors. *)

  val rand : float * float * float -> t
  (** return a random 3D vector *)

  val srand : Random.State.t -> float * float * float -> t

  val to_string : t -> string
  val of_string : string -> t

  module Infix : sig
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> float -> t
    val ( / ) : t -> float -> t

    val ( !. ) : t -> t -> float
    (** equivalent to [dot] *)

    val ( ~. ) : t -> t
    (** equivalent to [normalise] *)

    val ( ?. ) : float * float * float -> t
    (** equivalent to [Vector3d.rand] *)
  end
end


module Quaternion : sig
  type t = float * float * float * float

  val q : float -> float -> float -> float ->  t

  val real : t -> float
  val imag : t -> float * float * float

  val of_scalar : float -> t

  val to_list : t -> float list
  val of_list : float list -> t

  val add : t -> t -> t
  val sub : t -> t -> t
  val div : t -> t -> t
  val mul : t -> t -> t
  (** multiply 2 quaternions *)

  val addr : t -> float -> t
  val radd : float -> t -> t

  val rsub : float -> t -> t
  val subr : t -> float -> t

  val rmul : float -> t -> t
  val mulr : t -> float -> t
  (** multiply a quaternions by a number *)

  val norm : t -> float
  val conj : t -> t
  (** conjugate *)

  val neg : t -> t
  (** negative *)

  val unit : t -> t
  val reciprocal : t -> t

  val length : t -> float

  val to_string : t -> string
  val of_string : string -> t

  module Infix : sig
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t

    val ( ~. ) : t -> t
    (** equivalent to [conj] *)

    val ( |. ) : t -> float
    (** equivalent to [norm] *)
  end
end


module Curves2d : sig
  module Bezier : sig
    (** {{:http://en.wikipedia.org/wiki/B%C3%A9zier_curve}
        Bezier Curve on Wikipedia} *)

    module Linear : sig

      val pnt : float ->
        Point2d.t * Point2d.t ->
          Point2d.t

      val drv : float ->
        Point2d.t * Point2d.t ->
          Vector2d.t

      val rand :
        Point2d.t -> Point2d.t ->
          Point2d.t * Point2d.t

      val srand : Random.State.t ->
        Point2d.t -> Point2d.t ->
          Point2d.t * Point2d.t

    end
    module Quadratic : sig

      val pnt : float ->
        Point2d.t * Point2d.t * Point2d.t ->
          Point2d.t
      (** [Curves2d.Bezier.Quadratic.pnt t (p1, p2, p3)]
          returns a point on the quadratic 2D bezier curve
          defined by the points [p1], [p2] and [p3]
          with [t] in the interval [(0.0 .. 1.0)]
          *)

      val drv : float ->
        Point2d.t * Point2d.t * Point2d.t ->
          Vector2d.t
      (** derivative *)

      val rand :
        Point2d.t -> Point2d.t ->
          Point2d.t * Point2d.t * Point2d.t

      val srand : Random.State.t ->
        Point2d.t -> Point2d.t ->
          Point2d.t * Point2d.t * Point2d.t

    end
    module Cubic : sig

      val pnt : float ->
        Point2d.t * Point2d.t * Point2d.t * Point2d.t ->
          Point2d.t

      val drv : float ->
        Point2d.t * Point2d.t * Point2d.t * Point2d.t ->
          Vector2d.t
      (** derivative *)

      val rand :
        Point2d.t -> Point2d.t ->
          Point2d.t * Point2d.t * Point2d.t * Point2d.t

      val srand : Random.State.t ->
        Point2d.t -> Point2d.t ->
          Point2d.t * Point2d.t * Point2d.t * Point2d.t

    end
  end

  module CubicHermiteSpline : sig
    (** Cubic Hermite Spline *)
    (** Read the
        {{:http://en.wikipedia.org/wiki/Cubic_Hermite_spline}
        Wikipedia article about
        Cubic Hermite spline}
    *)

    val pnt : float ->
      p_pred:Point2d.t ->
      p0:Point2d.t ->
      p1:Point2d.t ->
      p_succ:Point2d.t -> float ->
        Point2d.t

  end
end


module Curves3d : sig
  module Bezier : sig
    (** {{:http://en.wikipedia.org/wiki/B%C3%A9zier_curve}
        Bezier Curve on Wikipedia} *)

    module Linear : sig

      val pnt : float ->
        Point3d.t * Point3d.t ->
          Point3d.t

      val drv : float ->
        Point3d.t * Point3d.t ->
          Vector3d.t

      val rand :
        Point3d.t -> Point3d.t ->
          Point3d.t * Point3d.t

      val srand : Random.State.t ->
        Point3d.t -> Point3d.t ->
          Point3d.t * Point3d.t

    end
    module Quadratic : sig

      val pnt : float ->
        Point3d.t * Point3d.t * Point3d.t ->
          Point3d.t

      val drv : float ->
        Point3d.t * Point3d.t * Point3d.t ->
          Vector3d.t

      val rand :
        Point3d.t -> Point3d.t ->
          Point3d.t * Point3d.t * Point3d.t

      val srand : Random.State.t ->
        Point3d.t -> Point3d.t ->
          Point3d.t * Point3d.t * Point3d.t

    end
    module Cubic : sig

      val pnt : float ->
        Point3d.t * Point3d.t * Point3d.t * Point3d.t ->
          Point3d.t

      val drv : float ->
        Point3d.t * Point3d.t * Point3d.t * Point3d.t ->
          Vector3d.t

      val rand :
        Point3d.t -> Point3d.t ->
          Point3d.t * Point3d.t * Point3d.t * Point3d.t

      val srand : Random.State.t ->
        Point3d.t -> Point3d.t ->
          Point3d.t * Point3d.t * Point3d.t * Point3d.t

    end
  end

  module CubicHermiteSpline : sig
    (** Cubic Hermite Spline *)
    (** Read the
        {{:http://en.wikipedia.org/wiki/Cubic_Hermite_spline}
        Wikipedia article about
        Cubic Hermite spline}
    *)

    val pnt : float ->
      p_pred:Point3d.t ->
      p0:Point3d.t ->
      p1:Point3d.t ->
      p_succ:Point3d.t ->
      float ->
        Point3d.t

  end
end

