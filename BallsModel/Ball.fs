namespace BallsModel
open System

[<Struct>]
type Ball =
  val id: Guid;
  val ph: PhysicalBody;
  val frame: Circle;

  interface Physical with
      member this.id = this.id
      member this.ph = this.ph

  new (speed : Vector<m/s>, mass, frame : Circle, ?id0) =
    let id = defaultArg id0 (Guid.NewGuid())    

    {
      id = id;
      ph = PhysicalBody(frame.asPoint, speed, mass)
      frame = frame;
    }

  new (id, ph, frame) = 
    {
      id = id;
      ph = ph;
      frame = frame;
    }

  member this.move (time: float<s>) =
    let newPh = this.ph.move time
    Ball(this.id, newPh, Circle(newPh.pos.x, newPh.pos.y, this.frame.radius))  