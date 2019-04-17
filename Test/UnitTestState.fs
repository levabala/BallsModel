namespace Tests

open BallsModel
open NUnit.Framework

[<TestFixture>]
type StateTests() =

  [<SetUp>]
  member this.Setup() =
    ()

  [<Test>]
  member this.StateTest1() =
    let balls = [|
      Ball (
        BallsModel.Vector(1.0<m/s>, 0.0<m/s>),
        1.0<kg>,
        BallsModel.Circle(1.0<m>, 2.0<m>, 1.0<m>)
      )
    |]

    let walls = [|
      Wall(
        [|
          Point(4.0<m>, 1.0<m>);
          Point(6.0<m>, 1.0<m>);
          Point(6.0<m>, 3.0<m>);
          Point(4.0<m>, 3.0<m>);
        |],
        100.0<kg>
      )
    |]

    let s1 = State(balls, walls, 1.0<s>)
    let s2 = s1.nextState

    match s2 with
    | Some s ->
      let dx' = s.balls.[0].ph.speed.dx |> float
      let dx'Exp = -0.98019801980198
      Assert.AreEqual(dx', dx'Exp, 10.0 ** (-5.0))

    | None -> Assert.Fail()


