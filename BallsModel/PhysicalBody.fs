namespace BallsModel
open System

[<Struct>]
type PhysicalBody =
  val pos : Point
  val speed : Vector<m / s>;
  val mass : float<kg>;

  new(pos, speed, mass) =
    {
      pos = pos;
      speed = speed;
      mass = mass;
    }

  member this.move (time : float<s>) =
    PhysicalBody (
      Point (this.pos.x + this.speed.dx * time, this.pos.y + this.speed.dy * time),
      this.speed,
      this.mass
    )

  member this.withPos (pos : Point) =
    PhysicalBody(pos, this.speed, this.mass)

  member this.withSpeed (speed : Vector<m / s>) =
    PhysicalBody(this.pos, speed, this.mass)

  static member Bounce (ph1 : PhysicalBody) (ph2 : PhysicalBody) : PhysicalBody * PhysicalBody =
    let axis = Line(ph1.pos, ph2.pos)
    let alpha = (Vector<_>.FromLine axis).angle

    let v1 = ph1.speed.rotate -alpha
    let v2 = ph2.speed.rotate -alpha

    let m1 = ph1.mass
    let m2 = ph2.mass

    let dx1 = (2.0 * (m2 * v2.dx) + (m1 - m2) * v1.dx) / (m1 + m2)
    let dx2 = (2.0 * (m1 * v1.dx) + (m2 - m1) * v2.dx) / (m1 + m2)

    let v1' = Vector(dx1, v1.dy)
    let v2' = Vector(dx2, v2.dy)

    let v1'' = v1'.rotate (alpha)
    let v2'' = v2'.rotate (alpha)

    ph1.withSpeed (v1''), ph2.withSpeed (v2'')

  static member equal (ph1 : PhysicalBody) (ph2 : PhysicalBody) : bool =
    let eps = 10.0 ** (-5.0)

    let deltas = [
      ph1.mass - ph2.mass |> float;
    ]

    deltas |> List.map abs |> List.forall (fun d -> d < eps) && Vector.equal ph1.speed ph2.speed

type Physical =
  abstract id : Guid
  abstract ph : PhysicalBody
