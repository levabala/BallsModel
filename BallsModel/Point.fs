namespace BallsModel

[<Struct>]
type Point = 
  val x: decimal<m>;
  val y: decimal<m>;

  new (x: decimal<m>, y: decimal<m>) = 
    {
      x = x;
      y = y;
    }

  static member dist (p1: Point) (p2: Point) : decimal<m> =
    let dx = p2.x - p1.x |> decimal |> float
    let dy = p2.y - p1.y |> decimal |> float

    dx ** 2.0 + dy ** 2.0 |> sqrt |> decimal |> LanguagePrimitives.DecimalWithMeasure

  