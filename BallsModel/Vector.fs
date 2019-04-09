namespace BallsModel

open System

module private Other =
  let atan2M (dy : decimal<m>) (dx : decimal<m>) : decimal<rad> =
      let dxF = dx |> decimal |> float
      let dyF = dy |> decimal |> float

      atan2 dyF dxF |> decimal |> LanguagePrimitives.DecimalWithMeasure

[<Struct>]
type Vector =
  val x : decimal<m>;
  val y : decimal<m>;
  val ex : decimal<m>;
  val ey : decimal<m>;
  val dx : decimal<m>;
  val dy : decimal<m>;
  val length : decimal<m>;
  val angle : decimal<rad>;

  new(x : decimal<m>, y : decimal<m>, dx : decimal<m>, dy : decimal<m>) =
    {
      x = x;
      y = y;
      ex = x + dx;
      ey = y + dy;
      dx = dx;
      dy = dy;
      length =
        dx * dx + dy * dy
        |> decimal |> float |> sqrt |> decimal
        |> LanguagePrimitives.DecimalWithMeasure;

      angle = Other.atan2M dy dx;
    }

  new(x : decimal<m>, y : decimal<m>, ex : decimal<m>, ey : decimal<m>, magicSignature) =
    let dx = ex - x;
    let dy = ey - y;

    {
      x = x;
      y = y;
      ex = ex;
      ey = ey;
      dx = dx;
      dy = dy;
      length =
        (dx * dx + dy * dy)
        |> decimal |> float |> sqrt |> decimal
        |> LanguagePrimitives.DecimalWithMeasure;
      angle = Other.atan2M dy dx;
    }

  new(l : Line) = Vector(l.x1, l.y1, l.x2, l.y2, obj)

  new(
      x : decimal<m>,
      y : decimal<m>,
      dx : decimal<m>,
      dy : decimal<m>,
      ex : decimal<m>,
      ey : decimal<m>,
      length : decimal<m>,
      angle : decimal<rad>
    )
    =
    {
      x = x;
      y = y;
      ex = ex;
      ey = ey;
      dx = dx;
      dy = dy;
      length = length;
      angle = angle;
    }

  new(
      x : decimal<m>,
      y : decimal<m>,
      length : decimal<m>,
      angle : decimal<rad>,
      magicSignature,
      magicSignature2
    ) =
    let coeffX = angle |> decimal |> float |> cos |> decimal
    let coeffY = angle |> decimal |> float |> sin |> decimal
    let dx = coeffX * length
    let dy = coeffY * length

    {
      x = x;
      y = y;
      ex = x + dx;
      ey = y + dy;
      dx = dx;
      dy = dy;
      length = length;
      angle = angle;
    }

  member this.Clone : Vector =
    Vector (this.x, this.y, this.dx, this.dy, this.ex, this.ey, this.length, this.angle)

  member this.Rotate(angle : decimal<rad>) : Vector =
    Vector (this.x, this.y, this.length, this.angle + angle, obj, obj)

  member this.SetLength(length : decimal<m>) : Vector =
    Vector(this.x, this.y, length, this.angle, obj, obj)

  member this.SetStart(x : decimal<m>, y : decimal<m>) : Vector =
    let ddx = x - this.x
    let ddy = y - this.y
    Vector (x, y, this.dx, this.dy, this.ex + ddx, this.ey + ddy, this.length, this.angle)

  member this.StartPoint =
    Point(this.x, this.y)

  member this.EndPoint =
    Point(this.ex, this.ey)

  // member this.project (line: Line) =


  static member normalizeAngle (angle : decimal<rad>) : decimal<rad> =
    let s = sign angle |> float
    let a = abs (angle) |> decimal |> float
    let low =
      a - (floor (a / Math.PI / 2.0)) * Math.PI * 2.0

    if low > Math.PI
    then (low - Math.PI) * s * -1.0
    else low * s
    |> decimal |> LanguagePrimitives.DecimalWithMeasure

  static member angleBetween (v1 : Vector) (v2 : Vector) : decimal<rad> =
    v2.angle - v1.angle |> Vector.normalizeAngle

  static member angleBetweenPositive (v1 : Vector) (v2 : Vector) : decimal<rad> =
    if v1.angle > v2.angle
    then v1.angle - v2.angle
    else v2.angle - v1.angle

  static member equal (v1 : Vector) (v2 : Vector) : bool =
    let eps = 10.0 ** (-5.0) |> decimal

    let deltas = [
      v1.x - v2.x |> decimal;
      v1.y - v2.y |> decimal;
      v1.dx - v2.dx |> decimal;
      v1.dy - v2.dy |> decimal;
      v1.ex - v2.ex |> decimal;
      v1.ey - v2.ey |> decimal;
      v1.length - v2.length |> decimal;
      v1.angle - v2.angle |> decimal;
    ]

    deltas |> List.map abs |> List.forall (fun d -> d < eps)

