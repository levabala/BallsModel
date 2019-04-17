namespace BallsModel

open System


[<Struct>]
type Vector< [<Measure>] 'T> =
  val dx : float<'T>;
  val dy : float<'T>;
  val length : float<'T>;
  val angle : float<rad>;

  new(dx : float<'T>, dy : float<'T>) =
    {
      dx = dx;
      dy = dy;
      length =
        dx * dx + dy * dy
        |> sqrt
        |> float
        |> LanguagePrimitives.FloatWithMeasure;

      angle = Vector<'T>.atan2M dy dx;
    }

  new(
      dx : float<'T>,
      dy : float<'T>,
      length : float<'T>,
      angle : float<rad>
    )
    =
    {
      dx = dx;
      dy = dy;
      length = length;
      angle = angle;
    }

  static member FromLine(l : Line) =
    let dx = l.x2 - l.x1
    let dy = l.y2 - l.y1

    Vector<m>(dx, dy)

  static member FromAngle (length : float<'T>) (angle : float<rad>) =
    let dx = (angle |> float |> cos) * length
    let dy = (angle |> float |> sin) * length
    Vector<'T>(
      dx,
      dy,
      length,
      angle
    )

  static member private atan2M (dy : float<'T>) (dx : float<'T>) : float<rad> =
      atan2 dy dx |> LanguagePrimitives.FloatWithMeasure

  member this.asLine (?origin0 : Point) =
    let origin = defaultArg origin0 (Point(0.0<m>, 0.0<m>))
    let ex = (origin.x |> float) + (this.dx |> float) |> LanguagePrimitives.FloatWithMeasure
    let ey = (origin.y |> float) + (this.dy |> float) |> LanguagePrimitives.FloatWithMeasure

    Line(
      origin.x,
      origin.y,
      ex,
      ey
    )

  member this.rotate (angle : float<rad>) : Vector<'T> =
    Vector.FromAngle this.length (this.angle + angle)

  member this.withLength (length : float<'T>) : Vector<'T> =
    Vector.FromAngle length this.angle

  static member normalizeAngle (angle : float<rad>) : float<rad> =
    let s = sign angle |> float
    let a = abs (angle) |> float
    let low =
      a - (floor (a / Math.PI / 2.0)) * Math.PI * 2.0


    if low > Math.PI
    then (Math.PI * 2.0 - low) * s * -1.0
    else low * s
     |> LanguagePrimitives.FloatWithMeasure

  static member angleBetween (v1 : Vector<'T>) (v2 : Vector<'T>) : float<rad> =
    v2.angle - v1.angle |> Vector<'T>.normalizeAngle

  static member angleBetweenPositive (v1 : Vector<'T>) (v2 : Vector<'T>) : float<rad> =
    if v1.angle > v2.angle
    then v1.angle - v2.angle
    else v2.angle - v1.angle

  static member equal (v1 : Vector<'T>) (v2 : Vector<'T>) : bool =
    let eps = 10.0 ** (-5.0)

    let deltas = [
      v1.length - v2.length |> float;
      Vector<'T>.normalizeAngle v1.angle - Vector<'T>.normalizeAngle v2.angle |> float;
    ]

    deltas |> List.map abs |> List.forall (fun d -> d < eps)

  static member (+) (v : Vector<'u>, p : Point) : Point =
    Point (
      (p.x |> float) + (v.dx |> float) |> LanguagePrimitives.FloatWithMeasure,
      (p.y |> float) + (v.dy |> float) |> LanguagePrimitives.FloatWithMeasure
    )

  static member (+) (p : Point, v : Vector<'u>) : Point =
    v + p
