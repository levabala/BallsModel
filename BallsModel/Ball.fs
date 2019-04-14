namespace BallsModel
open System

[<Struct>]
type Ball =
  val id: Guid;
  val ph: PhysicalBody;
  val frame: Circle;

  new (moment, mass, frame, ?id0) =
    let id = defaultArg id0 (Guid.NewGuid())

    {
      id = id;
      ph = PhysicalBody(moment, mass)
      frame = frame;
    }

  new (id, ph, frame) = 
    {
      id = id;
      ph = ph;
      frame = frame;
    }