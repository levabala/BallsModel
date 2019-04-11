namespace BallsModel

type Intersect() =
  static member intersect (l1 : Line, l2 : Line, ?likeSegments0 : bool) : Point option =
    let likeSegments = defaultArg likeSegments0 false
    let det = l1.A * l2.B - l1.B * l2.A

    if det = 0.0<m^2>
    then None
    else
      let x = (l2.B * l1.C - l1.B * l2.C) / det
      let y = (l1.A * l2.C - l2.A * l1.C) / det

      if likeSegments
      then
        let p = Point(x, y)
        let insideSegments =
          Point.dist p l1.startP <= l1.length &&
          Point.dist p l1.endP <= l1.length &&
          Point.dist p l2.startP <= l2.length &&
          Point.dist p l2.endP <= l2.length

        if insideSegments
        then p |> Some
        else None

      else
        Point(x, y) |> Some

  static member intersect (l1 : Line, lines : Line array, ?likeSegments0 : bool) : Point array =
    let likeSegments = defaultArg likeSegments0 false

    seq {
      for line in lines do
        yield Intersect.intersect (l1, line, likeSegments)
    }
    |> Seq.where (fun p -> p <> None)
    |> Seq.map (fun p -> p.Value)
    |> Seq.toArray

  static member intersect (l1 : Line, polygone : Polygone) : Point array =
    Intersect.intersect (l1, polygone.lines, true)

  static member intersect (l1 : Line, wall : Wall) : Point array =
    Intersect.intersect (l1, wall.frame)
