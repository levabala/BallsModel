namespace BallsModel

[<Struct>]
type Wall =
  val ph: PhysicalBody;
  val frame: Polygone;

  new (moment, mass, points) = 
    {
      ph = PhysicalBody(moment, mass);
      frame = Polygone(points)
    }  
    