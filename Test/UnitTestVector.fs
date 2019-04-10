namespace Tests

open BallsModel
open NUnit.Framework
open System

[<TestFixture>]
type Vector() =

  [<SetUp>]
  member this.Setup() =
    ()

  [<Test>]
  member this.ContructorsTest() =
    let v1 =
      BallsModel.Vector(
        1.0<m>,
        1.0<m>,
        2.0<m>,
        1.0<m>,
        3.0<m>,
        2.0<m>,
        sqrt 5.0 |> LanguagePrimitives.FloatWithMeasure,
        0.4636476<rad>
      )

    let v2 = BallsModel.Vector(BallsModel.Line(v1.x, v1.y, v1.ex, v1.ey))
    let v3 = BallsModel.Vector(v1.x, v1.y, v1.dx, v1.dy)
    let v4 = BallsModel.Vector(v1.x, v1.y, v1.ex, v1.ey, obj)
    let v5 = BallsModel.Vector(v1.x, v1.y, v1.length, v1.angle, obj, obj)

    let vectors = [v2; v3; v4; v5]
    let passed = vectors |> List.map (fun v -> Vector.equal v v1)

    Assert.IsTrue(passed |> List.forall (fun p -> p))

  [<Test>]
  member this.RotateTest1() =
    let v1 = BallsModel.Vector(1.0<m>, 1.0<m>, 2.0<m>, 1.0<m>)

    let angle = Math.PI / 2.0 |> LanguagePrimitives.FloatWithMeasure
    let v2 = v1.Rotate(angle)
    let v2Exp =
      BallsModel.Vector(
        1.0<m>,
        1.0<m>,
        sqrt 5.0 |> LanguagePrimitives.FloatWithMeasure,
        0.4636476<rad> + angle,
        obj,
        obj
      )

    Assert.IsTrue(Vector.equal v2 v2Exp)


  [<Test>]
  member this.RotateTest2() =
    let v1 = BallsModel.Vector(1.0<m>, 1.0<m>, 2.0<m>, 1.0<m>)

    let angle = Math.PI |> LanguagePrimitives.FloatWithMeasure
    let v2 = v1.Rotate(angle)
    let v2Exp =
      BallsModel.Vector(
        1.0<m>,
        1.0<m>,
        -2.0<m>,
        -1.0<m>,
        -1.0<m>,
        0.0<m>,
        sqrt 5.0 |> LanguagePrimitives.FloatWithMeasure,
        0.4636476<rad> + angle
      )

    Assert.IsTrue(Vector.equal v2 v2Exp)

  [<Test>]
  member this.CalcEndTest1() =
    let v1 = BallsModel.Vector(1.0<m>, 1.0<m>, 2.0<m>, 1.0<m>)
    let newLength = 4.0 ** 2.0 + 2.0 ** 2.0 |> sqrt |> LanguagePrimitives.FloatWithMeasure

    let point = v1.CalcEnd(newLength)
    let pointExp = Point(5.0<m>, 3.0<m>)

    Assert.IsTrue(Point.equal point pointExp)
