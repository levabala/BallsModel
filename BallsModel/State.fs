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

  member this.nextState : State option =
    let balls = this.balls
    let walls = this.walls

    let PI = System.Math.PI

    let intersections =
      balls
      |> Array.map
        (fun ball ->
          let rightV =
            ball.ph.speed.rotate(
              PI / 2.0 |> LanguagePrimitives.FloatWithMeasure
            ) .withLength(ball.frame.radius / 1.0<s>)

          let leftV =
            ball.ph.speed.rotate(
              -PI / 2.0 |> LanguagePrimitives.FloatWithMeasure
            ) .withLength(ball.frame.radius / 1.0<s>)

          let mainLine = ball.ph.speed.asLine (ball.frame.asPoint)
          let interLines = [|
            mainLine;
            ball.ph.speed.asLine (ball.frame.asPoint + rightV);
            ball.ph.speed.asLine (ball.frame.asPoint + leftV);
          |]

          let intersections : (Wall * ((Point * Line) array)) array =
            walls
            |> Array.map
              (fun wall ->
                  let abstractInterPoints : (Point * Vector<m> * Line) array =
                    wall.frame.lines
                    |> Array.indexed
                    |> Array.map
                      (fun (i, wallLine) ->
                        let normal = wall.frame.normals.[i]

                        let point =
                          interLines
                          |> Array.map
                            (fun ballLine ->
                                Intersect.intersect (ballLine, wallLine, false, true)
                            )
                          |> Array.where
                            (fun interP ->
                              match interP with
                              | Some p -> true
                              | None -> false
                            )
                          |> Array.map (fun interP -> interP.Value)
                          |> Array.tryLast

                        (point, normal, wallLine)
                      )
                    |> Array.where
                      (fun (p, n, l) -> p.IsSome)
                    |> Array.map
                      (fun (p, n, l) -> (p.Value, n, l))

                  let realInterPoints =
                    abstractInterPoints
                    |> Array.map
                      (fun (abstractPoint, normal, wallLine) ->
                        let pointTop = (normal.withLength ball.frame.radius) + abstractPoint
                        let verticalLine = (Vector<_>.FromLine wallLine) .asLine pointTop
                        let ballCenter = Intersect.intersect (mainLine, verticalLine)
                        match ballCenter with
                        | Some p -> (p, wallLine)
                        | None -> failwith "cannot build ball center after bouncing"
                      )

                  (wall, realInterPoints)
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
                      |> Array.map (fun (p, l) -> p)
                      |> Array.minBy (fun p -> Point.dist ball.frame.asPoint p)                      
                    )
                  )
                  ||> Point.dist
                )

            let (interWall, interPoints) = closestIntersection
            let (closestPoint, closestWallLine) = 
              interPoints 
              |> Array.minBy (fun (p, l) -> Point.dist ball.frame.asPoint p)

            let distToIntersect = Point.dist ball.frame.asPoint closestPoint
            let timeToIntersect = distToIntersect / ball.ph.speed.length

            Some (ball, interWall, closestPoint, closestWallLine, distToIntersect, timeToIntersect)
      )
      |> Array.where (fun t -> t.IsSome)
      |> Array.map (fun t -> t.Value)

    if intersections.Length = 0
    then None
    else
      let closestIntersection =
        intersections
        |> Array.minBy (fun (_, _, _, _, _, timeToIntersect) -> timeToIntersect)

      let (ball, interWall, interPoint, closestWallLine, distToIntersect, timeToIntersect) = closestIntersection

      let interLine = closestWallLine
      let perpVector =
        (Vector<_>.FromLine interLine) .rotate(Math.PI / 2.0 |> LanguagePrimitives.FloatWithMeasure)
      let perpLine = perpVector.asLine interPoint
      let perpInterPoint = (Intersect.intersect (perpLine, interLine)).Value

      let relativeWallPh = interWall.phRelativeTo perpInterPoint

      let newBalls =
        this.balls
        |> Array.map
          (fun b ->
            let movedBall = timeToIntersect |> b.move
            if movedBall.id = ball.id
            then
              let (newPh, newWallPh) = PhysicalBody.Bounce movedBall.ph relativeWallPh

              Ball(movedBall.id, newPh, movedBall.frame)
            else movedBall
          )

      let newWalls = this.walls
      let newTimeStamp = this.timeStamp + timeToIntersect

      Some(State(newBalls, newWalls, newTimeStamp))
