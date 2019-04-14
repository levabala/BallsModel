namespace BallsModel

[<Struct>]
type State =
  val balls : Ball array;
  val walls : Wall array;
  val timeStamp : float<s>;

  new (balls, walls, timeStamp) = 
    {
      balls = balls;
      walls = walls;
      timeStamp = timeStamp;
    }
  
  member this.calcNext =              
    let interections = 
      this.balls 
      |> Array.map (
        fun ball ->                     
          let v = ball.ph.v
          (
            this.walls |>
            Array.map (
              fun wall -> 
                (wall.ph, Intersect.intersect (v, wall), ball)
            ),
            this.balls |>
            Array.map (
              fun ball -> 
                (ball.ph, Intersect.intersect (v, ball), ball)
            )
          ) ||> Array.append
      )
      |> Array.concat

    let closestIntersection = 
      interections |> Array.minBy (fun (ph, p, ball) -> Point.dist ball.frame.Point p)

    let (ph, p, ball) = closestIntersection
    let distToIntersect = Point.dist ball.frame.Point p
    let timeToIntersect = distToIntersect / ball.ph.v.length
    
        