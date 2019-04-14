namespace BallsModel
open System

[<Struct>]
type State =
  val balls : Ball array;
  val walls : Wall array;
  val timeStamp : float<s>;

  new(balls, walls, timeStamp) =
    {
      balls = balls;
      walls = walls;
      timeStamp = timeStamp;
    }

  member this.nextState : State =
    let balls = this.balls
    let walls = this.walls

    let intersections =
      balls
      |> Array.map
        (fun ball ->
          let rightV =
            ball.ph.speed.rotate(
              Math.PI / 2.0 |> LanguagePrimitives.FloatWithMeasure
            ) .withLength(ball.frame.radius / 1.0<s>)

          let leftV =
            ball.ph.speed.rotate(
              -Math.PI / 2.0 |> LanguagePrimitives.FloatWithMeasure
            ) .withLength(ball.frame.radius / 1.0<s>)

          let interLines = [|
            ball.ph.speed.asLine();
            ball.ph.speed.asLine (ball.frame.asPoint + rightV);
            ball.ph.speed.asLine (ball.frame.asPoint + leftV);
          |]

          let intersections : (Wall * Point array) array =
            walls
            |> Array.map
              (fun wall ->
                  let interPoints =
                    wall.frame.lines
                    |> Array.map
                      (fun wallLine ->
                        interLines
                        |> Array.map
                          (fun ballLine ->
                              Intersect.intersect (ballLine, wallLine, false, true)
                          )
                      )
                    |> Array.concat
                    |> Array.where
                      (fun interP ->
                        match interP with
                        | Some p -> true
                        | None -> false
                      )
                    |> Array.map (fun interP -> interP.Value)

                  (wall, interPoints)
              )
            |> Array.where
              (fun (wall, points) -> points.Length > 0)

          if intersections.Length = 0
          then None
          else
            let closestIntersection =
              intersections
              |> Array.minBy
                (fun (wall, points) ->
                  (
                    ball.frame.asPoint,
                    (
                      points
                      |> Array.minBy (fun p -> Point.dist ball.frame.asPoint p)
                    )
                  )
                  ||> Point.dist
                )

            let (interWall, interPoints) = closestIntersection
            let closestPoint = interPoints |> Array.minBy (fun p -> Point.dist ball.frame.asPoint p)

            let distToIntersect = Point.dist ball.frame.asPoint closestPoint
            let timeToIntersect = distToIntersect / ball.ph.speed.length

            Some (ball, interWall, distToIntersect, timeToIntersect)
      )

    let closestIntersection =
      intersections
      |> Array.where (fun t -> t.IsSome)
      |> Array.map (fun t -> t.Value)
      |> Array.minBy (fun (_, _, _, timeToIntersect) -> timeToIntersect)

    let (ball, interWall, distToIntersect, timeToIntersect) = closestIntersection

    let newBalls =
      this.balls
      |> Array.map
        (fun b ->
          let newBall =
            if b.id = ball.id
            then
              let (newPh, _) = PhysicalBody.Bounce b.ph interWall.ph

              Ball(b.id, newPh, b.frame)
            else b

          timeToIntersect |> newBall.move
        )

    let newWalls = this.walls
    let newTimeStamp = this.timeStamp + timeToIntersect

    State(newBalls, newWalls, newTimeStamp)
