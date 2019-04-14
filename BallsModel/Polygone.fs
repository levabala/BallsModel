namespace BallsModel

open System

exception NotEnoughPointsException of string

[<Struct>]
type Polygone =
  val points : Point array;
  val lines : Line array;
  val normals : Vector<m> array;

  new(points : Point array) =
    if points.Length = 0
    then
      failwith "Too little points amount"

    let closed = Array.append points [| points.[0] |]

    let pairs = seq {
      for i = 0 to closed.Length - 2 do
        yield (closed.[i], closed.[i + 1])
    }

    let lines : Line array = pairs |> Seq.map Line |> Seq.toArray
    let smallestLine =
      Array.fold
        (
          fun (acc : Line) (item : Line) ->
            if abs (item.length) < abs (acc.length) && item.length <> 0.0<m>
            then item
            else acc
        )
        lines.[0]
        lines

    if points.Length < 3
    then
      {
        points = points;
        lines = lines;
        normals = [||]
      }
    else
      let angle = (Vector<_>.FromLine(lines.[0]), Vector<_>.FromLine(lines.[1])) ||> Vector.angleBetween
      let clockwise = angle >= 0.0<rad>

      if not clockwise
      then Polygone(Array.rev closed)
      else
        let normals : Vector<m> array = Seq.toArray <| seq {
          for line in lines do
            let centerP = Point (line.x1 + (line.x2 - line.x1) / 2.0, line.y1 + (line.y2 - line.y1) / 2.0)
            let leftV =
              Vector<_>.FromLine(line)
                // .SetStart(center.x, center.y)
                .withLength(line.length / 2.0)
                .rotate(
                  Math.PI / -2.0
                  |> LanguagePrimitives.FloatWithMeasure
                )

            let leftVEnd = centerP + leftV
            let leftXSign = leftVEnd.x - centerP.x |> sign
            let leftYSign = leftVEnd.y - centerP.y |> sign
            let pointIsLeft (p : Point) =
              (p.x - centerP.x) |> sign = leftXSign &&
              (p.y - centerP.y) |> sign = leftYSign

            yield leftV
        }


        {
          points = points;
          lines = lines;
          normals = normals;
        }
