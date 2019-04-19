namespace BallsModel

[<Struct>]
type Collision =
  val ball : Ball;
  val wall : Wall;
  val wallLine : Line;
  val intersectionPoint : Point;
  val timeStamp : float<s>

  new (ball, wall, wallLine, intersectionPoint, timeStamp) =
    {
      ball = ball;
      wall = wall;
      wallLine = wallLine;
      intersectionPoint = intersectionPoint;
      timeStamp = timeStamp;
    }

  member this.asTuple = 
    (this.ball, this.wall, this.wallLine, this.intersectionPoint, this.timeStamp)