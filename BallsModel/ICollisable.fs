namespace BallsModel

type ICollisable< [<Measure>] 'a> =
  | Circle of Circle
  // | Square of Square
  | Polygone of Polygone
  | Line of Line
  | Vector of Vector<'a>
