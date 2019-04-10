namespace BallsModel

[<Struct>]
type Point =
  val x : float<m>;
  val y : float<m>;

  new(x : float<m>, y : float<m>) =
    {
      x = x;
      y = y;
    }

  static member dist (p1 : Point) (p2 : Point) : float<m> =
    let dx = p2.x - p1.x |> float
    let dy = p2.y - p1.y |> float

    dx ** 2.0 + dy ** 2.0 |> sqrt |> float |> LanguagePrimitives.FloatWithMeasure

  static member equal (p1: Point) (p2 : Point) : bool =
    let eps = 10.0 ** (-5.0)

    let deltas = [
      p1.x - p2.x;
      p1.y - p2.y;
    ]

    deltas |> List.map float |> List.map abs |> List.forall (fun d -> d < eps)
