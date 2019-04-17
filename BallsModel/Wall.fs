namespace BallsModel

open System

[<Struct>]
type Wall =
  val id: Guid;
  val ph: PhysicalBody;
  val frame: Polygone;

  interface Physical with
      member this.id = this.id
      member this.ph = this.ph

  new (points : Point array, mass : float<kg>, ?id0) = 
    let id = defaultArg id0 (Guid.NewGuid())    
    
    {
      id = id;
      ph = PhysicalBody(points.[0], Vector<m/s>(0.0<m/s>, 0.0<m/s>), mass);
      frame = Polygone(points)
    }

  member this.phRelativeTo (p : Point) : PhysicalBody =
    this.ph.withPos p
    