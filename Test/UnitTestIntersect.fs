namespace Tests

open BallsModel
open NUnit.Framework
open System


[<TestFixture>]
type Intersect() =

  [<SetUp>]
  member this.Setup() =
    ()

  [<Test>]
  member this.Line2LineTest1() =
    let l1 = BallsModel.Line(0.0<m>, 0.0<m>, 6.0<m>, 2.0<m>)
    let l2 = BallsModel.Line(4.0<m>, 0.0<m>, 2.0<m>, 2.0<m>)

    let interP = Intersect.intersect (l1, l2)
    let interPExpected = Point(3.0<m>, 1.0<m>)

    match interP with
    | Some p -> Assert.IsTrue (p.x = interPExpected.x && p.y = interPExpected.y)
    | None -> Assert.Fail()

  [<Test>]
  member this.Line2LineTest2() =
    let l1 = BallsModel.Line(0.0<m>, 0.0<m>, 6.0<m>, 2.0<m>)
    let l2 = BallsModel.Line(4.0<m>, 0.0<m>, 2.0<m>, 2.0<m>)

    let interP = Intersect.intersect (l1, l2)
    let interPExpected = Point(2.0<m>, 0.0<m>)

    match interP with
    | Some p -> Assert.IsTrue (p.x <> interPExpected.x && p.y <> interPExpected.y)
    | None -> Assert.Fail()

  [<Test>]
  member this.Line2LineSegmemtTest1() =
    let l1 = BallsModel.Line(0.0<m>, 0.0<m>, 1.0<m>, 2.0<m>)
    let l2 = BallsModel.Line(4.0<m>, 0.0<m>, 2.0<m>, 2.0<m>)

    let interP = Intersect.intersect (l1, l2, true)

    match interP with
    | Some p -> Assert.Fail()
    | None -> Assert.Pass()

  [<Test>]
  member this.Line2LineSegmemtTest2() =
    let l1 = BallsModel.Line(0.0<m>, 0.0<m>, 2.0<m>, 2.0<m>)
    let l2 = BallsModel.Line(4.0<m>, 0.0<m>, 2.0<m>, 2.0<m>)

    let interP = Intersect.intersect (l1, l2, true)
    let interPExpected = Point(2.0<m>, 2.0<m>)

    match interP with
    | Some p -> Assert.IsTrue (p.x = interPExpected.x && p.y = interPExpected.y)
    | None -> Assert.Fail()

  [<Test>]
  member this.Line2PolygonTest1() =
    let points = [|
      Point(3.0<m>, 1.0<m>);
      Point(6.0<m>, 2.0<m>);
      Point(5.0<m>, 6.0<m>);
      Point(2.0<m>, 5.0<m>);
      Point(1.0<m>, 3.0<m>);
    |]

    let polygon = Polygone(points)
    let line = Line(2.0<m>, 1.0<m>, 7.0<m>, 4.0<m>)

    let interPoints = Intersect.intersect (line, polygon)
    let interPointsExp = [|
      (Intersect.intersect (
        line,
        Line (
          Point(6.0<m>, 2.0<m>),
          Point(5.0<m>, 6.0<m>)
        );
      )).Value;
      (Intersect.intersect (
        line,
        Line (
          Point(1.0<m>, 3.0<m>),
          Point(3.0<m>, 1.0<m>)
        )
      )).Value;
    |]

    interPoints
    |> Seq.indexed
    |> Seq.forall (fun (i, p) -> Point.equal p interPointsExp.[i])
    |> Assert.IsTrue

  [<Test>]
  member this.Line2PolygonTest2() =
    let points = [|
      Point(3.0<m>, 1.0<m>);
      Point(6.0<m>, 2.0<m>);
      Point(5.0<m>, 6.0<m>);
      Point(2.0<m>, 5.0<m>);
      Point(1.0<m>, 3.0<m>);
    |]

    let polygon = Polygone(points)
    let line = Line(0.0<m>, 0.0<m>, 7.0<m>, 0.0<m>)

    let interPoints = Intersect.intersect (line, polygon)

    Assert.AreEqual(interPoints.Length, 0)

  [<Test>]
  member this.Line2CircleTest1() =
    let line = BallsModel.Line(1.0<m>, 2.0<m>, 8.0<m>, 2.0<m>)
    let circle = BallsModel.Circle(5.0<m>, 2.0<m>, 1.0<m>)

    let interPoints = Intersect.intersect (line, circle)
    let interPointsExp = [|
      Point(6.0<m>, 2.0<m>);
      Point(4.0<m>, 2.0<m>);
    |]

    Assert.IsTrue (
      Point.equal interPoints.[0] interPointsExp.[0] &&
      Point.equal interPoints.[1] interPointsExp.[1]
    )

  [<Test>]
  member this.Line2CircleTest2() =
    let line = BallsModel.Line(1.0<m>, 1.0<m>, 8.0<m>, 1.0<m>)
    let circle = BallsModel.Circle(5.0<m>, 2.0<m>, 1.0<m>)

    let interPoints = Intersect.intersect (line, circle)
    let interPointsExp = [|
      Point(5.0<m>, 1.0<m>);
    |]

    Assert.IsTrue (
      Point.equal interPoints.[0] interPointsExp.[0]
    )

