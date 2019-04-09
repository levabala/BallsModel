namespace Tests

open BallsModel
open NUnit.Framework

[<TestFixture>]
type Vector() =

  [<SetUp>]
  member this.Setup() =
    ()

  [<Test>]
  member this.ContructorsTest() =
    let v1 =
      BallsModel.Vector (
        1M<m>,
        1M<m>,
        2M<m>,
        1M<m>,
        3M<m>,
        2M<m>,
        sqrt 5.0 |> decimal |> LanguagePrimitives.DecimalWithMeasure,
        0.4636476M<rad>
      )

    let v2 = BallsModel.Vector (BallsModel.Line(v1.x, v1.y, v1.ex, v1.ey))
    let v3 = BallsModel.Vector(v1.x, v1.y, v1.dx, v1.dy)
    let v4 = BallsModel.Vector(v1.x, v1.y, v1.ex, v1.ey, obj)
    let v5 = BallsModel.Vector (v1.x, v1.y, v1.length, v1.angle, obj, obj)

    let vectors = [ v2; v3; v4; v5 ]
    let passed = vectors |> List.map (fun v -> Vector.equal v v1)

    Assert.IsTrue(passed |> List.forall (fun p -> p))
