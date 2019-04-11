namespace BallsModel

[<Struct>]
type State =
  val balls : Ball array;
  val walls : Wall array;