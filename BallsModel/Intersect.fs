namespace BallsModel

type Intersect() =
  static member intersect (l1 : Line, l2 : Line, ?likeSegments0 : bool, ?likeSegments1 : bool) : Point option =
    let likeSegment1 = defaultArg likeSegments0 false
    let likeSegment2 = defaultArg likeSegments1 false

    let det = l1.A * l2.B - l1.B * l2.A

    if det = 0.0<m^2>
    then None
    else
      let x = (l2.B * l1.C - l1.B * l2.C) / det
      let y = (l1.A * l2.C - l2.A * l1.C) / det

      if likeSegment1 || likeSegment2
      then
        let p = Point(x, y)
        let insideSegments =
          (
            not likeSegment1 ||
            Point.dist p l1.startP <= l1.length &&
            Point.dist p l1.endP <= l1.length
          ) &&
          (
            not likeSegment2 ||
            Point.dist p l2.startP <= l2.length &&
            Point.dist p l2.endP <= l2.length
          )

        if insideSegments
        then p |> Some
        else None

      else
        Point(x, y) |> Some

  static member intersect (l1 : Line, lines : Line array, ?likeSegments0 : bool, ?likeSegments1 : bool) : Point array =
    let likeSegment1 = defaultArg likeSegments0 false
    let likeSegment2 = defaultArg likeSegments1 false

    seq {
      for line in lines do
        yield Intersect.intersect (l1, line, likeSegment1, likeSegment2)
    }
    |> Seq.where (fun p -> p <> None)
    |> Seq.map (fun p -> p.Value)
    |> Seq.toArray

  static member intersect (l1 : Line, polygone : Polygone) : Point array =
    Intersect.intersect (l1, polygone.lines, true, true)

  static member intersect (l1 : Line, wall : Wall) : Point array =
    Intersect.intersect (l1, wall.frame)

  static member intersect (v1 : Vector<_>, polygone : Polygone) : Point array =
    Intersect.intersect (v1.asLine(), polygone.lines, true)

  static member intersect (v1 : Vector<_>, wall : Wall) : Point array =
    Intersect.intersect (v1.asLine(), wall.frame)

  static member intersect (l1 : Line, c : Circle) : Point array =
    // source:
    // http://mathworld.wolfram.com/Circle-LineIntersection.html

    let x1 = l1.x1 - c.x
    let x2 = l1.x2 - c.x
    let y1 = l1.y1 - c.y
    let y2 = l1.y2 - c.y

    let r = c.radius |> float
    let dx = x2 - x1 |> float
    let dy = y2 - y1 |> float
    let dr = (dx |> float) ** 2.0 + (dy |> float) ** 2.0 |> sqrt
    let D = x1 * y2 - x2 * y1 |> float

    let sgn (v : float) : float = if v < 0.0 then -1.0 else 1.0

    let det = sqrt (r ** 2.0 * dr ** 2.0 - D ** 2.0)
    let rightX = (sgn dy) * dx * det
    let rightY = (abs dy) * det

    let leftX = D * dy
    let leftY = -D * dx

    let bottom = dr ** 2.0

    if det < 0.0
    then
      [||]
    else
      let x1 = (leftX + rightX) / bottom |> LanguagePrimitives.FloatWithMeasure
      let y1 = (leftY + rightY) / bottom |> LanguagePrimitives.FloatWithMeasure

      let p1 = Point(x1 + c.x, y1 + c.y)

      if det = 0.0
      then [| p1 |]
      else
        let x2 = (leftX - rightX) / bottom |> LanguagePrimitives.FloatWithMeasure
        let y2 = (leftY - rightY) / bottom |> LanguagePrimitives.FloatWithMeasure

        let p2 = Point(x2 + c.x, y2 + c.y)

        [| p1; p2 |]


  static member intersect (l1 : Line, b : Ball) : Point array =
    Intersect.intersect (l1, b.frame)

  static member intersect (v1 : Vector<_>, c : Circle) : Point array =
    Intersect.intersect (v1.asLine(), c)

  static member intersect (v1 : Vector<_>, b : Ball) : Point array =
    Intersect.intersect (v1.asLine(), b)
