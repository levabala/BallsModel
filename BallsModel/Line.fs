namespace BallsModel

[<Struct>]
type Line =
  val x1 : float<m>;
  val y1 : float<m>;
  val x2 : float<m>;
  val y2 : float<m>;
  val A : float<m>;
  val B : float<m>;
  val C : float<m^2>;
  val startP : Point;
  val endP : Point;
  val length : float<m>

  new(x1, y1, x2, y2) =
    let a = y2 - y1
    let b = x1 - x2
    let c = a * x1 + b * y1
    let startP = Point(x1, y1);
    let endP = Point(x2, y2);

    {
      x1 = x1;
      y1 = y1;
      x2 = x2;
      y2 = y2;
      A = a;
      B = b;
      C = c;
      startP = startP;
      endP = endP;
      length = Point.dist startP endP
    }

  new(p1 : Point, p2 : Point) =
    Line(p1.x, p1.y, p2.x, p2.y)

  static member angleBetween (l1 : Line) (l2 : Line) : float<rad> =
    let a1 = l1.A |> float
    let b1 = l1.B |> float
    let a2 = l2.A |> float
    let b2 = l2.B |> float

    let top = (a1 * a2 + b1 * b2)
    let bottom =
      ((a1 ** 2.0 + b1 ** 2.0) |> sqrt) *
      ((a2 ** 2.0 + b2 ** 2.0) |> sqrt)

    top / bottom |> acos  |> LanguagePrimitives.FloatWithMeasure

  static member intersect (l1 : Line) (l2 : Line) : Point option =
    let det = l1.A * l2.B - l1.B * l2.A

    if det = 0.0<m^2>
    then None
    else
      let x = (l2.B * l1.C - l1.B * l2.C) / det
      let y = (l1.A * l2.C - l2.A * l1.C) / det

      Point(x, y) |> Some

  static member pointIsOnLine (p : Point) (l : Line) : bool =
    let eps : float<m^2> = 10.0 ** -6.0  |> LanguagePrimitives.FloatWithMeasure

    abs (l.A * p.x + l.B * p.y - l.C) < eps

  static member pointIsOnLineSegment (p : Point) (l : Line) : bool =
    let isOnLine = Line.pointIsOnLine p l

    if not isOnLine
    then false
    else
      let distance = max (Point.dist p l.startP) (Point.dist p l.endP)

      distance <= l.length

  static member countCollisions (l : Line) (lines : Line array) =
    Array.fold
      (fun acc item -> if Line.intersect l item <> None then acc + 1 else acc)
      0
      lines
