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
      BallsModel.Vector (
        1.0<m>,
        2.0<m>,
        sqrt 5.0 |> LanguagePrimitives.FloatWithMeasure,
        1.107148718<rad>
      )

    let v2 = BallsModel.Line(0.0<m>, 0.0<m>, v1.dx, v1.dy) |> BallsModel.Vector<m>.FromLine
    let v3 = BallsModel.Vector(v1.dx, v1.dy)
    let v4 = BallsModel.Vector<m>.FromAngle v1.length v1.angle

    let vectors = [ v2; v3; v4 ]
    let passed = vectors |> List.map (fun v -> Vector.equal v v1)

    Assert.IsTrue(passed |> List.forall (fun p -> p))

  [<Test>]
  member this.RotateTest1() =
    let v1 = BallsModel.Vector(1.0<m>, 2.0<m>)

    let angle = Math.PI / 2.0 |> LanguagePrimitives.FloatWithMeasure
    let v2 = v1.rotate (angle)
    let v2Exp =
      (
        sqrt 5.0 |> LanguagePrimitives.FloatWithMeasure,
        1.107148718<rad> + angle
      ) ||> BallsModel.Vector<m>.FromAngle

    Assert.IsTrue(Vector.equal v2 v2Exp)


  [<Test>]
  member this.RotateTest2() =
    let v1 = BallsModel.Vector(1.0<m>, 2.0<m>)

    let angle = Math.PI |> LanguagePrimitives.FloatWithMeasure
    let v2 = v1.rotate (angle)
    let v2Exp =
      BallsModel.Vector(-1.0<m>, -2.0<m>)

    Assert.IsTrue(Vector.equal v2 v2Exp)
