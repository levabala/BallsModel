namespace BallsModel

type Circle = 
  struct
    val x: float<m>;
    val y: float<m>;
    val radius: float<m>
    new (x, y, radius) = {
      x = x; 
      y = y; 
      radius = radius;
    }
    
    member this.asPoint = 
      Point(this.x, this.y)
  end