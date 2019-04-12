namespace Tests

open BallsModel
open NUnit.Framework
open System

[<TestFixture>]
type Line() =

  [<SetUp>]
  member this.Setup() =
    ()  

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
