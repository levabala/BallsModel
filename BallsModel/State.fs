namespace BallsModel
open System

[<Struct>]
type State =
  val balls : Ball array;
  val walls : Wall array;
  val calculatedCollisions : Map<Guid, Collision>;
  val timeStamp : float<s>;

  new(balls, walls, timeStamp, ?calculatedCollisions0) =
    let collisions = defaultArg calculatedCollisions0 Map.empty
    {
      balls = balls;
      walls = walls;
      timeStamp = timeStamp;
      calculatedCollisions = collisions;
    }

  member this.addWall (wall : Wall) =
    State (this.balls, Array.append this.walls [| wall |], this.timeStamp)

  member this.nextState (timeGate0 : float<s> option) : State option =
    let timeGate = defaultArg timeGate0 (1.0<s> * (10.0 ** -1.0))

    let collisionFor (ball : Ball) (walls : Wall array) (timeStamp : float<s>) : Collision option =
      let rightV =
        ball.ph.speed.rotate(
          Math.PI / 2.0 |> LanguagePrimitives.FloatWithMeasure
        ) .withLength(ball.frame.radius / 1.0<s>)

      let leftV =
        ball.ph.speed.rotate(
          -Math.PI / 2.0 |> LanguagePrimitives.FloatWithMeasure
        ) .withLength(ball.frame.radius / 1.0<s>)

      let rightSignX = sign ball.ph.speed.dx
      let rightSignY = sign ball.ph.speed.dy
      let pointIsOnRightSide (p : Point) =
       sign (p.x - ball.frame.x) = rightSignX &&
       sign (p.y - ball.frame.y) = rightSignY

      let mainLine = ball.ph.speed.asLine (ball.frame.asPoint)
      let rightP = ball.frame.asPoint + rightV
      let leftP = ball.frame.asPoint + leftV
      let interLines = [|
        mainLine;
        ball.ph.speed.asLine rightP;
        ball.ph.speed.asLine leftP;
      |]

      let intersections : (Wall * (Point * Line) array) array =
        walls
        |> Array.map
          (fun wall ->
            let abstractInterPoints : (Point * Vector<m> * Line) array =
              wall.frame.lines
              |> Array.indexed
              |> Array.map
                (fun (i, wallLine) ->
                  let normal = wall.frame.normals.[i]

                  let points =
                    interLines
                    |> Array.map
                      (fun ballLine ->
                          Intersect.intersect (ballLine, wallLine, false, true)
                      )
                    |> Array.where
                      (fun interP ->
                        interP.Length <> 0
                      )
                    |> Array.map (fun interP -> interP.[0])

                  let point =
                    points
                    |> Array.where (fun interP -> pointIsOnRightSide interP)
                    |> Array.tryLast

                  (point, normal, wallLine)
                )
              |> Array.where
                (fun (p, n, l) -> p.IsSome)
              |> Array.map
                (fun (p, n, l) -> (p.Value, n, l))
              // |> Array.append wallInterPoints


            let realInterPoints =
              abstractInterPoints
              |> Array.map
                (fun (abstractPoint, normal, wallLine) ->
                  let pointTop = (normal.withLength ball.frame.radius) + abstractPoint
                  let vv = Vector<_>.FromLine
                  let wallV = vv wallLine
                  let verticalLine = () .asLine pointTop
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

        let collision =
          Collision (ball, interWall, closestWallLine, closestPoint, timeStamp + timeToIntersect)

        Some collision

    let balls = this.balls
    let walls = this.walls
    let timeStamp = this.timeStamp
    let calculatedCollisions = this.calculatedCollisions

    let collisions : Collision array =
      balls
      |> Array.Parallel.map
        (fun b ->
          let bufferedCollision =
            calculatedCollisions
            |> Map.tryFind b.id

          match bufferedCollision with
          | Some c -> bufferedCollision
          | None -> collisionFor b walls timeStamp
        )
      |> Array.where (fun t -> t.IsSome)
      |> Array.Parallel.map (fun t -> t.Value)

    if collisions.Length = 0
    then None
    else
      let closestCollision =
        collisions
        |> Array.minBy (fun col -> col.timeStamp)

      let followingCollisions =
        collisions
        |> Array.where
          (fun col ->
            (col.timeStamp - closestCollision.timeStamp < timeGate) &&
            col.ball.id <> closestCollision.ball.id
          )

      let processingCollisions = [| closestCollision |] |> Array.append followingCollisions

      let bouncings = Seq.toArray <| seq {
        for collision in processingCollisions do
          let (ball, interWall, closestWallLine, interPoint, collisionTimeStamp) = collision.asTuple

          let timeToIntersect = collisionTimeStamp - timeStamp
          let interLine = closestWallLine
          let perpVector =
            (Vector<_>.FromLine interLine) .rotate(Math.PI / 2.0 |> LanguagePrimitives.FloatWithMeasure)
          let perpLine = perpVector.asLine interPoint
          let perpInterPoint = (Intersect.intersect (perpLine, interLine)).Value

          let relativeWallPh = interWall.phRelativeTo perpInterPoint

          yield (ball, timeToIntersect, relativeWallPh)
      }


      let timeToIntersect = closestCollision.timeStamp - timeStamp

      let newBalls =
        this.balls
        |> Array.map
          (fun b ->
            let bounceTuple = bouncings |> Array.tryFind (fun (bBall, _, _) -> bBall.id = b.id)
            match bounceTuple with
            | Some(bBall, timeToIntersectLocal, bWallPh) ->
                let movedBall = timeToIntersectLocal |> b.move
                let (newPh, newWallPh) = PhysicalBody.Bounce movedBall.ph bWallPh
                Ball(movedBall.id, newPh, movedBall.frame)
            | None -> timeToIntersect |> b.move
          )

      let bouncedBallIds = bouncings |> Array.map (fun (b, _, _) -> b.id)
      let newWalls = this.walls
      let newTimeStamp = this.timeStamp + timeToIntersect

      let finalCollisions =
        collisions
        |> Array.where
          (fun col -> not (bouncedBallIds |> Array.contains col.ball.id))
        |> Array.Parallel.map
          (fun c -> (c.ball.id, c))
        |> Map.ofArray

      State (newBalls, newWalls, newTimeStamp, finalCollisions) |> Some

// let wallInterPoints =
//   let wallPoints = wall.frame.points |> Array.take (wall.frame.points.Length - 2)
//   let wallPointsR =
//     wallPoints
//     |> Array.indexed
//     |> Array.map
//       (fun (i, wallP) ->
//         let pointV =
//           Vector(
//             wallP.x - ball.frame.x,
//             wallP.y - ball.frame.y
//           ) .rotate(-ball.ph.speed.angle)

//         let relativeP = ball.frame.asPoint + pointV
//         (relativeP, i)
//       )

//   let ty = ball.frame.y - ball.frame.radius
//   let by = ball.frame.y + ball.frame.radius
//   let collisingPoints =
//     wallPointsR
//     |> Array.where
//       (fun (p, i) ->
//         p.y >= ty && p.y <= by
//       )
//     |> Array.map
//       (fun (p, i) ->
//         (wall.frame.points.[i], wall.frame.normals.[i], wall.frame.lines.[i])
//       )

//   collisingPoints
