namespace BallsModel

[<Struct>]
type PhysicalBody =
  val v : Vector;
  val mass : decimal<kg>;

  new(moment, mass) =
    {
      v = moment;
      mass = mass;
    }

  member this.SetVector (v : Vector) =
    PhysicalBody(v, this.mass)

  static member Bounce (ph1 : PhysicalBody, ph2 : PhysicalBody) : PhysicalBody * PhysicalBody =
    let axis = Line(ph1.v.StartPoint, ph2.v.StartPoint)
    let alpha = Vector(axis).angle

    let v1 = ph1.v.Rotate (-alpha)
    let v2 = ph1.v.Rotate (-alpha)

    let m1 = ph1.mass
    let m2 = ph2.mass

    let dx1 = (2M * (m2 * v2.dx) + (m1 - m2) * v1.dx) / (m1 + m2)
    let dx2 = (2M * (m1 * v1.dx) + (m2 - m1) * v2.dx) / (m1 + m2)

    let v1' = Vector(v1.x, v1.y, dx1, v1.dy)
    let v2' = Vector(v2.x, v2.y, dx2, v2.dy)

    let v1'' = v1'.Rotate (alpha)
    let v2'' = v2'.Rotate (alpha)

    (ph1.SetVector (v1''), ph2.SetVector (v2''))

