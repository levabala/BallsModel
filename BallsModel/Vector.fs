namespace BallsModel

open System

module private Other =
  let atan2M (dy : float<m>) (dx : float<m>) : float<rad> =
      let dxF = dx
      let dyF = dy

      atan2 dyF dxF |> LanguagePrimitives.FloatWithMeasure

[<Struct>]
type Vector =
  val x : float<m>;
  val y : float<m>;
  val ex : float<m>;
  val ey : float<m>;
  val dx : float<m>;
  val dy : float<m>;
  val length : float<m>;
  val angle : float<rad>;

  new(x : float<m>, y : float<m>, dx : float<m>, dy : float<m>) =
    {
      x = x;
      y = y;
      ex = x + dx;
      ey = y + dy;
      dx = dx;
      dy = dy;
      length =
        dx * dx + dy * dy
        |> sqrt
        |> float
        |> LanguagePrimitives.FloatWithMeasure;

      angle = Other.atan2M dy dx;
    }

  new(x : float<m>, y : float<m>, ex : float<m>, ey : float<m>, magicSignature) =
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
        |> sqrt
        |> float
        |> LanguagePrimitives.FloatWithMeasure;
      angle = Other.atan2M dy dx;
    }

  new(l : Line) = Vector(l.x1, l.y1, l.x2, l.y2, obj)

  new(
      x : float<m>,
      y : float<m>,
      dx : float<m>,
      dy : float<m>,
      ex : float<m>,
      ey : float<m>,
      length : float<m>,
      angle : float<rad>
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
      x : float<m>,
      y : float<m>,
      length : float<m>,
      angle : float<rad>,
      magicSignature,
      magicSignature2
    ) =
    let coeffX = angle |> float |> cos
    let coeffY = angle |> float |> sin
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

  member this.AsLine = 
    BallsModel.Line(this.x, this.y, this.ex, this.ey)

  member this.CalcEnd(length : float<m>) : Point =
    Point (this.x + this.dx / this.length * length, this.y + this.dy / this.length * length)

  member this.Clone : Vector =
    Vector (this.x, this.y, this.dx, this.dy, this.ex, this.ey, this.length, this.angle)

  member this.Rotate(angle : float<rad>) : Vector =
    Vector (this.x, this.y, this.length, this.angle + angle, obj, obj)

  member this.SetLength(length : float<m>) : Vector =
    Vector(this.x, this.y, length, this.angle, obj, obj)

  member this.SetStart(x : float<m>, y : float<m>) : Vector =
    let ddx = x - this.x
    let ddy = y - this.y
    Vector (x, y, this.dx, this.dy, this.ex + ddx, this.ey + ddy, this.length, this.angle)

  member this.StartPoint =
    Point(this.x, this.y)

  member this.EndPoint =
    Point(this.ex, this.ey)

  // member this.project (line: Line) =


  static member normalizeAngle (angle : float<rad>) : float<rad> =
    let s = sign angle |> float
    let a = abs (angle) |> float
    let low =
      a - (floor (a / Math.PI / 2.0)) * Math.PI * 2.0


    if low > Math.PI
    then (Math.PI * 2.0 - low) * s * -1.0
    else low * s
     |> LanguagePrimitives.FloatWithMeasure

  static member angleBetween (v1 : Vector) (v2 : Vector) : float<rad> =
    v2.angle - v1.angle |> Vector.normalizeAngle

  static member angleBetweenPositive (v1 : Vector) (v2 : Vector) : float<rad> =
    if v1.angle > v2.angle
    then v1.angle - v2.angle
    else v2.angle - v1.angle

  static member equal (v1 : Vector) (v2 : Vector) : bool =
    let eps = 10.0 ** (-5.0)

    let deltas = [
      v1.x - v2.x |> float;
      v1.y - v2.y |> float;
      v1.dx - v2.dx |> float;
      v1.dy - v2.dy |> float;
      v1.ex - v2.ex |> float;
      v1.ey - v2.ey |> float;
      v1.length - v2.length |> float;
      Vector.normalizeAngle v1.angle - Vector.normalizeAngle v2.angle |> float;
    ]

    deltas |> List.map abs |> List.forall (fun d -> d < eps)

