namespace BallsModel

[<Struct>]
type Ball =
  val ph: PhysicalBody;
  val frame: Circle;

  new (moment, mass, frame) =
    {
      ph = PhysicalBody(moment, mass)
      frame = frame;
    }  