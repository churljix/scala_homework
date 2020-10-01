import Ordering.Double.IeeeOrdering

object ClassesAndTraitsHw2 extends App {

    sealed trait Shape2D extends Located2D with Bounded2D with Form2D


    sealed trait Located2D {
      def x: Double
      def y: Double
    }

    sealed trait Bounded2D {
      def minX: Double
      def maxX: Double
      def minY: Double
      def maxY: Double
    }

    final case class Point2D(x: Double, y: Double) extends Shape2D {
      override def minX: Double = x
      override def maxX: Double = x
      override def minY: Double = y
      override def maxY: Double = y
      override def area: Double = 0
    }

    final case class Circle2D(center: Point2D, radius: Double) extends Shape2D {
      override def x: Double = center.x
      override def y: Double = center.y
      override def minX: Double = x - radius
      override def maxX: Double = x + radius
      override def minY: Double = y - radius
      override def maxY: Double = y + radius
      override def area: Double = Math.PI * radius * radius
    }

    final case class Rectangle2D(center: Point2D, width: Double, height: Double) extends Shape2D {
      override def x: Double = center.x
      override def y: Double = center.y
      override def minX: Double = x - width/2
      override def maxX: Double = x + width/2
      override def minY: Double = y - height/2
      override def maxY: Double = y + height/2
      override def area: Double = width * height
    }

    // Add additional 2D shapes such as triangle and square.
    final case class Triangle(ver1: Point2D, ver2: Point2D, ver3: Point2D) extends Shape2D {
      override def x: Double = (ver1.x + ver2.x + ver3.x) / 3
      override def y: Double = (ver1.y + ver2.y + ver3.y) / 3
      override def minX: Double = List(ver1.x, ver2.x, ver3.x).sorted.head
      override def maxX: Double = List(ver1.x, ver2.x, ver3.x).sorted.last
      override def minY: Double = List(ver1.y, ver2.y, ver3.y).sorted.head
      override def maxY: Double = List(ver1.y, ver2.y, ver3.y).sorted.last
      override def area: Double = ((ver1.x * (ver2.y - ver3.y)) + (ver2.x * (ver3.y - ver1.y)) + (ver3.x* (ver1.y - ver2.y))) / 2
    }

    final case class Square(center: Point2D, side: Double) extends Shape2D {
      override def x: Double = center.x
      override def y: Double = center.y
      override def minX: Double = x - side/2
      override def maxX: Double = x + side/2
      override def minY: Double = y - side/2
      override def maxY: Double = y + side/2
      override def area: Double = side * side
    }

    // Add method `area` to 2D shapes.

    sealed trait Form2D {
      def area: Double
    }

    //3D shapes classes
   sealed trait Shape3D extends Located3D with Form3D

   sealed trait Located3D {
     def x: Double
     def y: Double
     def z: Double
   }

  // (point, sphere, cube, cuboid, tetrahedron
  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def surfaceArea: Double = 0
    override def volume: Double = 0
  }
  final case class Sphere(center: Point3D, radius: Double) extends Shape3D {
    override def surfaceArea: Double = 4 * Math.PI * Math.pow(radius, 2)
    override def volume: Double = (4/3) * Math.PI * Math.pow(radius, 2)
    override def x: Double = center.x
    override def y: Double = center.y
    override def z: Double = center.z
  }
  final case class Cube(center: Point3D, side: Double) extends Shape3D {
    override def surfaceArea: Double = 6 * Math.pow(side, 2)
    override def volume: Double = Math.pow(side, 3)
    override def x: Double = center.x
    override def y: Double = center.y
    override def z: Double = center.z
  }
  final case class Cuboid(center: Point3D, width: Double, height: Double, length: Double) extends Shape3D {
    override def surfaceArea: Double = (width * length) +  (length * height) + (width * height) * 2
    override def volume: Double = width * length * height
    override def x: Double = center.x
    override def y: Double = center.y
    override def z: Double = center.z
  }
  final case class Tetrahedron(center: Point3D, side: Double) extends Shape3D {
    override def surfaceArea: Double = Math.sqrt(3) * Math.pow(side, 2)
    override def volume: Double = Math.pow(side, 3) / (6 * Math.sqrt(2))
    override def x: Double = center.x
    override def y: Double = center.y
    override def z: Double = center.z
  }

    // Add methods `surfaceArea` and `volume` to 3D shapes.
  sealed trait Form3D {
    def surfaceArea: Double
    def volume: Double
  }

  // Pattern matching and exhaustiveness checking
  def describe(x: Shape2D): String = x match {
    case Point2D(x, y) => s"Point(x = $x, y = $y)"
    case Circle2D(center, radius) => s"Circle(center = $center, radius = $radius)"
    case Rectangle2D(center,width,height) => s"Rectangle(center = $center, width = $width, height = $height)"
    case Triangle(ver1, ver2, ver3) => s"Triangle(ver1 = $ver1, ver2 = $ver2, ver3 = $ver3)"
    case Square(center, side) => s"Circle(center = $center, side = $side)"
  }
}