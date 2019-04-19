namespace BallsModel

[<Struct>]
type Square = 
  val origin : Point;
  val endPoint : Point;
  val width : float<m>;
  val height : float<m>;

  new (origin : Point, width, height) = 
    let endPoint = Point(origin.x + width, origin.y)
    {
      origin = origin;
      endPoint = endPoint;
      width = width;
      height = height;
    }

  member this.checkPointWithin (p : Point) = 
    p.x >  this.origin.x && p.x < this.endPoint.x &&
    p.y >  this.origin.y && p.y < this.endPoint.y