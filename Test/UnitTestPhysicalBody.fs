namespace Tests

open BallsModel
open NUnit.Framework

[<TestFixture>]
type PhysicalBody() =

  [<SetUp>]
  member this.Setup() =
    ()

  [<Test>]
  member this.BounceTestSameMassHorizontal() =
    let ph1 =
      BallsModel.PhysicalBody (
        BallsModel.Vector(1.0<m>, 1.0<m>, 2.0<m>, 0.0<m>),
        1.0<kg>
      )

    let ph2 =
      BallsModel.PhysicalBody (
        BallsModel.Vector(5.0<m>, 1.0<m>, -2.0<m>, 0.0<m>),
        1.0<kg>
      )

    let (ph1', ph2') = PhysicalBody.Bounce ph1 ph2
    let ph1'Exp =
      BallsModel.PhysicalBody (
        BallsModel.Vector(1.0<m>, 1.0<m>, -2.0<m>, 0.0<m>),
        1.0<kg>
      )

    let ph2'Exp =
      BallsModel.PhysicalBody (
        BallsModel.Vector(5.0<m>, 1.0<m>, 2.0<m>, 0.0<m>),
        1.0<kg>
      )

    let passed = [ PhysicalBody.equal ph1' ph1'Exp; PhysicalBody.equal ph2' ph2'Exp ]
    Assert.IsTrue(passed |> List.forall (fun p -> p))

  [<Test>]
  member this.BounceTestDiffMassHorizontal() =
    let ph1 =
      BallsModel.PhysicalBody (
        BallsModel.Vector(1.0<m>, 1.0<m>, 2.0<m>, 0.0<m>),
        1.0<kg>
      )

    let ph2 =
      BallsModel.PhysicalBody (
        BallsModel.Vector(5.0<m>, 1.0<m>, -2.0<m>, 0.0<m>),
        2.0<kg>
      )

    let (ph1', ph2') = PhysicalBody.Bounce ph1 ph2
    let ph1'Exp =
      BallsModel.PhysicalBody (
        BallsModel.Vector (1.0<m>, 1.0<m>, -3.33333333333<m>, 0.0<m>),
        1.0<kg>
      )

    let ph2'Exp =
      BallsModel.PhysicalBody (
        BallsModel.Vector (5.0<m>, 1.0<m>, 0.666666666<m>, 0.0<m>),
        2.0<kg>
      )

    let passed = [ PhysicalBody.equal ph1' ph1'Exp; PhysicalBody.equal ph2' ph2'Exp ]
    Assert.IsTrue(passed |> List.forall (fun p -> p))

  [<Test>]
  member this.BounceTestSameMassVertical() =
    let ph1 =
      BallsModel.PhysicalBody (
        BallsModel.Vector(1.0<m>, 1.0<m>, 0.0<m>, 2.0<m>),
        1.0<kg>
      )

    let ph2 =
      BallsModel.PhysicalBody (
        BallsModel.Vector(1.0<m>, 5.0<m>, 0.0<m>, -2.0<m>),
        1.0<kg>
      )

    let (ph1', ph2') = PhysicalBody.Bounce ph1 ph2
    let ph1'Exp =
      BallsModel.PhysicalBody (
        BallsModel.Vector(1.0<m>, 1.0<m>, 0.0<m>, -2.0<m>),
        1.0<kg>
      )

    let ph2'Exp =
      BallsModel.PhysicalBody (
        BallsModel.Vector(1.0<m>, 5.0<m>, 0.0<m>, 2.0<m>),
        1.0<kg>
      )

    let passed = [ PhysicalBody.equal ph1' ph1'Exp; PhysicalBody.equal ph2' ph2'Exp ]
    Assert.IsTrue(passed |> List.forall (fun p -> p))

  [<Test>]
  member this.BounceTestDiffMassVertical() =
    let ph1 =
      BallsModel.PhysicalBody (
        BallsModel.Vector(1.0<m>, 1.0<m>, 0.0<m>, 2.0<m>),
        1.0<kg>
      )

    let ph2 =
      BallsModel.PhysicalBody (
        BallsModel.Vector(1.0<m>, 5.0<m>, 0.0<m>, -2.0<m>),
        2.0<kg>
      )

    let (ph1', ph2') = PhysicalBody.Bounce ph1 ph2
    let ph1'Exp =
      BallsModel.PhysicalBody (
        BallsModel.Vector (1.0<m>, 1.0<m>, 0.0<m>, -3.33333333333<m>),
        1.0<kg>
      )

    let ph2'Exp =
      BallsModel.PhysicalBody (
        BallsModel.Vector (1.0<m>, 5.0<m>, 0.0<m>, 0.666666666<m>),
        2.0<kg>
      )

    let passed = [ PhysicalBody.equal ph1' ph1'Exp; PhysicalBody.equal ph2' ph2'Exp ]
    Assert.IsTrue(passed |> List.forall (fun p -> p))

  [<Test>]
  member this.BounceTestSameMass1() =
    let ph1 =
      BallsModel.PhysicalBody (
        BallsModel.Vector(1.0<m>, 1.0<m>, 1.0<m>, 1.0<m>),
        1.0<kg>
      )

    let ph2 =
      BallsModel.PhysicalBody (
        BallsModel.Vector(3.0<m>, 3.0<m>, -1.0<m>, -1.0<m>),
        1.0<kg>
      )

    let (ph1', ph2') = PhysicalBody.Bounce ph1 ph2
    let ph1'Exp =
      BallsModel.PhysicalBody (
        BallsModel.Vector(1.0<m>, 1.0<m>, -1.0<m>, -1.0<m>),
        1.0<kg>
      )

    let ph2'Exp =
      BallsModel.PhysicalBody (
        BallsModel.Vector(3.0<m>, 3.0<m>, 1.0<m>, 1.0<m>),
        1.0<kg>
      )

    let passed = [ PhysicalBody.equal ph1' ph1'Exp; PhysicalBody.equal ph2' ph2'Exp ]
    Assert.IsTrue(passed |> List.forall (fun p -> p))

  [<Test>]
  member this.BounceTestDiffMass1() =
    let ph1 =
      BallsModel.PhysicalBody (
        BallsModel.Vector(1.0<m>, 1.0<m>, 1.0<m>, 1.0<m>),
        1.0<kg>
      )

    let ph2 =
      BallsModel.PhysicalBody (
        BallsModel.Vector(3.0<m>, 3.0<m>, -1.0<m>, -1.0<m>),
        2.0<kg>
      )

    let (ph1', ph2') = PhysicalBody.Bounce ph1 ph2
    let l1 = -2.3570226033333<m>;
    let ph1'Exp =
      BallsModel.PhysicalBody (
        BallsModel.Vector (1.0<m>, 1.0<m>, l1 / sqrt 2.0, l1 / sqrt 2.0),
        1.0<kg>
      )

    let l2 = 0.47140452066667<m>
    let ph2'Exp =
      BallsModel.PhysicalBody (
        BallsModel.Vector (3.0<m>, 3.0<m>, l2 / sqrt 2.0, l2 / sqrt 2.0),
        2.0<kg>
      )

    let passed = [ PhysicalBody.equal ph1' ph1'Exp; PhysicalBody.equal ph2' ph2'Exp ]
    Assert.IsTrue(passed |> List.forall (fun p -> p))
