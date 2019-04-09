namespace BallsModel

type Circle = 
  struct
    val x: decimal<m>;
    val y: decimal<m>;
    val radius: decimal<m>
    new (x, y, radius) = {
      x = x; 
      y = y; 
      radius = radius;
    }

  end