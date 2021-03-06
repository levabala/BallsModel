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
        Point(1.0<m>, 1.0<m>),
        BallsModel.Vector(2.0<m/s>, 0.0<m/s>),
        1.0<kg>
      )

    let ph2 =
      BallsModel.PhysicalBody (
        Point(5.0<m>, 1.0<m>),
        BallsModel.Vector(-2.0<m/s>, 0.0<m/s>),
        1.0<kg>
      )

    let (ph1', ph2') = PhysicalBody.Bounce ph1 ph2
    let ph1'Exp =
      BallsModel.PhysicalBody (
        Point(1.0<m>, 1.0<m>),
        BallsModel.Vector(-2.0<m/s>, 0.0<m/s>),
        1.0<kg>
      )

    let ph2'Exp =
      BallsModel.PhysicalBody (
        Point(5.0<m>, 1.0<m>),
        BallsModel.Vector(2.0<m/s>, 0.0<m/s>),
        1.0<kg>
      )

    let passed = [ PhysicalBody.equal ph1' ph1'Exp; PhysicalBody.equal ph2' ph2'Exp ]
    Assert.IsTrue(passed |> List.forall (fun p -> p))

  [<Test>]
  member this.BounceTestDiffMassHorizontal() =
    let ph1 =
      BallsModel.PhysicalBody (
        Point(1.0<m>, 1.0<m>),
        BallsModel.Vector(2.0<m/s>, 0.0<m/s>),
        1.0<kg>
      )

    let ph2 =
      BallsModel.PhysicalBody (
        Point(5.0<m>, 1.0<m>),
        BallsModel.Vector(-2.0<m/s>, 0.0<m/s>),
        2.0<kg>
      )

    let (ph1', ph2') = PhysicalBody.Bounce ph1 ph2
    let ph1'Exp =
      BallsModel.PhysicalBody (
        Point(1.0<m>, 1.0<m>),
        BallsModel.Vector(-3.33333333333<m/s>, 0.0<m/s>),
        1.0<kg>
      )

    let ph2'Exp =
      BallsModel.PhysicalBody (
        Point(5.0<m>, 1.0<m>),
        BallsModel.Vector(0.666666666<m/s>, 0.0<m/s>),
        2.0<kg>
      )

    let passed = [ PhysicalBody.equal ph1' ph1'Exp; PhysicalBody.equal ph2' ph2'Exp ]
    Assert.IsTrue(passed |> List.forall (fun p -> p))

  [<Test>]
  member this.BounceTestSameMassVertical() =
    let ph1 =
      BallsModel.PhysicalBody (
        Point(1.0<m>, 1.0<m>),
        BallsModel.Vector(0.0<m/s>, 2.0<m/s>),
        1.0<kg>
      )

    let ph2 =
      BallsModel.PhysicalBody (
        Point(1.0<m>, 5.0<m>),
        BallsModel.Vector(0.0<m/s>, -2.0<m/s>),
        1.0<kg>
      )

    let (ph1', ph2') = PhysicalBody.Bounce ph1 ph2
    let ph1'Exp =
      BallsModel.PhysicalBody (
        Point(1.0<m>, 1.0<m>),
        BallsModel.Vector(0.0<m/s>, -2.0<m/s>),
        1.0<kg>
      )

    let ph2'Exp =
      BallsModel.PhysicalBody (
        Point(1.0<m>, 5.0<m>),
        BallsModel.Vector(0.0<m/s>, 2.0<m/s>),
        1.0<kg>
      )

    let passed = [ PhysicalBody.equal ph1' ph1'Exp; PhysicalBody.equal ph2' ph2'Exp ]
    Assert.IsTrue(passed |> List.forall (fun p -> p))

  [<Test>]
  member this.BounceTestDiffMassVertical() =
    let ph1 =
      BallsModel.PhysicalBody (
        Point(1.0<m>, 1.0<m>),
        BallsModel.Vector(0.0<m/s>, 2.0<m/s>),
        1.0<kg>
      )

    let ph2 =
      BallsModel.PhysicalBody (
        Point(1.0<m>, 5.0<m>),
        BallsModel.Vector(0.0<m/s>, -2.0<m/s>),
        2.0<kg>
      )

    let (ph1', ph2') = PhysicalBody.Bounce ph1 ph2
    let ph1'Exp =
      BallsModel.PhysicalBody (
        Point(1.0<m>, 1.0<m>),
        BallsModel.Vector(0.0<m/s>, -3.33333333333<m/s>),
        1.0<kg>
      )

    let ph2'Exp =
      BallsModel.PhysicalBody (
        Point(1.0<m>, 5.0<m>),
        BallsModel.Vector(0.0<m/s>, 0.666666666<m/s>),
        2.0<kg>
      )

    let passed = [ PhysicalBody.equal ph1' ph1'Exp; PhysicalBody.equal ph2' ph2'Exp ]
    Assert.IsTrue(passed |> List.forall (fun p -> p))

  [<Test>]
  member this.BounceTestSameMass1() =
    let ph1 =
      BallsModel.PhysicalBody (
        Point(1.0<m>, 1.0<m>),
        BallsModel.Vector(1.0<m/s>, 1.0<m/s>),
        1.0<kg>
      )

    let ph2 =
      BallsModel.PhysicalBody (
        Point(3.0<m>, 3.0<m>),
        BallsModel.Vector(-1.0<m/s>, -1.0<m/s>),
        1.0<kg>
      )

    let (ph1', ph2') = PhysicalBody.Bounce ph1 ph2
    let ph1'Exp =
      BallsModel.PhysicalBody (
        Point(1.0<m>, 1.0<m>),
        BallsModel.Vector(-1.0<m/s>, -1.0<m/s>),
        1.0<kg>
      )

    let ph2'Exp =
      BallsModel.PhysicalBody (
        Point(3.0<m>, 3.0<m>),
        BallsModel.Vector(1.0<m/s>, 1.0<m/s>),
        1.0<kg>
      )

    let passed = [ PhysicalBody.equal ph1' ph1'Exp; PhysicalBody.equal ph2' ph2'Exp ]
    Assert.IsTrue(passed |> List.forall (fun p -> p))

  [<Test>]
  member this.BounceTestDiffMass1() =
    let ph1 =
      BallsModel.PhysicalBody (
        Point(1.0<m>, 1.0<m>),
        BallsModel.Vector(1.0<m/s>, 1.0<m/s>),
        1.0<kg>
      )

    let ph2 =
      BallsModel.PhysicalBody (
        Point(3.0<m>, 3.0<m>),
        BallsModel.Vector(-1.0<m/s>, -1.0<m/s>),
        2.0<kg>
      )

    let (ph1', ph2') = PhysicalBody.Bounce ph1 ph2
    let l1 = -2.3570226033333<m/s>;
    let ph1'Exp =
      BallsModel.PhysicalBody (
        Point(1.0<m>, 1.0<m>),
        BallsModel.Vector(l1 / sqrt 2.0, l1 / sqrt 2.0),
        1.0<kg>
      )

    let l2 = 0.47140452066667<m/s>
    let ph2'Exp =
      BallsModel.PhysicalBody (
        Point(3.0<m>, 3.0<m>),
        BallsModel.Vector(l2 / sqrt 2.0, l2 / sqrt 2.0),
        2.0<kg>
      )

    let passed = [ PhysicalBody.equal ph1' ph1'Exp; PhysicalBody.equal ph2' ph2'Exp ]
    Assert.IsTrue(passed |> List.forall (fun p -> p))
