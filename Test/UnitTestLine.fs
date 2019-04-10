namespace Tests

open System
open BallsModel
open NUnit.Framework

[<TestFixture>]
type Line() =

  [<SetUp>]
  member this.Setup() =
    ()

  [<Test>]
  member this.IntersectCheck1() =
    let l1 = BallsModel.Line(0.0<m>, 0.0<m>, 6.0<m>, 2.0<m>)
    let l2 = BallsModel.Line(4.0<m>, 0.0<m>, 2.0<m>, 2.0<m>)

    let interP = Line.intersect l1 l2
    let interPExpected = Point(3.0<m>, 1.0<m>)

    match interP with
    | Some p -> Assert.IsTrue (p.x = interPExpected.x && p.y = interPExpected.y)
    | None -> Assert.Fail()

  [<Test>]
  member this.IntersectCheck2() =
    let l1 = BallsModel.Line(0.0<m>, 0.0<m>, 6.0<m>, 2.0<m>)
    let l2 = BallsModel.Line(4.0<m>, 0.0<m>, 2.0<m>, 2.0<m>)

    let interP = Line.intersect l1 l2
    let interPExpected = Point(2.0<m>, 0.0<m>)

    match interP with
    | Some p -> Assert.IsTrue (p.x <> interPExpected.x && p.y <> interPExpected.y)
    | None -> Assert.Fail()

  [<Test>]
  member this.AngleCheck1() =
    let l1 = BallsModel.Line(0.0<m>, 0.0<m>, 6.0<m>, 0.0<m>)
    let l2 = BallsModel.Line(0.0<m>, 0.0<m>, 2.0<m>, 2.0<m>)

    let angle = Line.angleBetween l1 l2 |> float
    let angleExpected = Math.PI / 4.0

    Assert.AreEqual(angleExpected, angle, 10.0 ** (-5.0))

  [<Test>]
  member this.AngleCheck2() =
    let l1 = BallsModel.Line(0.0<m>, 0.0<m>, 6.0<m>, 0.0<m>)
    let l2 = BallsModel.Line(0.0<m>, 0.0<m>, -2.0<m>, 2.0<m>)

    let angle = Line.angleBetween l1 l2 |> float
    let angleExpected = Math.PI / 4.0 * 3.0

    Assert.AreEqual(angleExpected, angle, 10.0 ** (-5.0))

  [<Test>]
  member this.AngleCheck3() =
    let l1 = BallsModel.Line(0.0<m>, 0.0<m>, 6.0<m>, 0.0<m>)
    let l2 = BallsModel.Line(0.0<m>, 0.0<m>, 7.0<m>, 0.0<m>)

    let angle = Line.angleBetween l1 l2 |> float
    let angleExpected = 0.0

    Assert.AreEqual(angleExpected, angle, 10.0 ** (-5.0))
