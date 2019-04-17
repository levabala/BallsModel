#load "Measures.fs"
#load "Point.fs"
#load "Line.fs"
#load "Vector.fs"
#load "PhysicalBody.fs"
#load "Circle.fs"
#load "Ball.fs"
#load "Polygone.fs"
#load "Wall.fs"
#load "Intersect.fs"
#load "State.fs"

open BallsModel

let balls = [|
  Ball (
    BallsModel.Vector(1.0<m/s>, 1.0<m/s>),
    1.0<kg>,
    BallsModel.Circle(2.0<m>, 1.0<m>, 1.0<m>)
  )
|]

let walls = [|
  Wall(
    [|
      Point(4.0<m>, 1.0<m>);
      Point(6.0<m>, 1.0<m>);
      Point(6.0<m>, 3.0<m>);
      Point(4.0<m>, 3.0<m>);
    |],
    100.0<kg>
  )
|]

let s1 = State(balls, walls, 1.0<s>)
let s2 = s1.nextState

