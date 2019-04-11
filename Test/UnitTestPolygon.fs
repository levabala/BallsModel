namespace Tests

open BallsModel
open NUnit.Framework
open System

[<TestFixture>]
type Polygon() =

  [<SetUp>]
  member this.Setup() =
    ()

  [<Test>]
  member this.BuildTest1() =
    let points = [|
      Point(3.0<m>, 1.0<m>);
      Point(6.0<m>, 2.0<m>);
      Point(5.0<m>, 6.0<m>);
      Point(2.0<m>, 5.0<m>);
      Point(1.0<m>, 3.0<m>);
    |]

    let polygon = Polygone(points)

    let linesExpected = [|
      Line(
        Point(3.0<m>, 1.0<m>),
        Point(6.0<m>, 2.0<m>)
      );
      Line(
        Point(6.0<m>, 2.0<m>),
        Point(5.0<m>, 6.0<m>)
      );
      Line(
        Point(5.0<m>, 6.0<m>),
        Point(2.0<m>, 5.0<m>)
      );
      Line(
        Point(2.0<m>, 5.0<m>),
        Point(1.0<m>, 3.0<m>)
      );
      Line(
        Point(1.0<m>, 3.0<m>),
        Point(3.0<m>, 1.0<m>)
      );
    |]

    let normalsAnglesExpected =
      linesExpected |>
      Array.map (fun l -> float (BallsModel.Vector(l).angle) - Math.PI / 2.0 |> LanguagePrimitives.FloatWithMeasure)

    polygon.normals |> Seq.indexed |> Seq.forall (fun (i, n) ->
        abs (n.angle - normalsAnglesExpected.[i]) |> float < 10.0 ** (-5.0)
    )
    |> Assert.IsTrue
