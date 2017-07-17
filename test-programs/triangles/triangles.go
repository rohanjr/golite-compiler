// Sample Go program to print ASCII triangles in a grid
// By Rohan Jacob Rao

package main;

/* Test whether the points (px1, py1) and (px2, py2) are on the same side of
   (and not lying on) the line passing through (lx1, ly1) and (lx2, ly2). */
func onSameSide(px1, py1, px2, py2, lx1, ly1, lx2, ly2 float64) bool {
   // vector of line from (lx1, ly1) to (lx2, ly2)
   var vx float64 = lx2 - lx1;
   var vy float64 = ly2 - ly1;

   /* 2d cross product of each vector from (lx1, ly1) to (px1, py1) and to
      (px2, py2) with the vector (vx, vy) */
   var cp1 float64 = (px1 - lx1) * vy - (py1 - ly1) * vx;
   var cp2 float64 = (px2 - lx1) * vy - (py2 - ly1) * vx;

   /* sign of cross product indicates on which side of the line the point lies
      - return true iff the products have the same sign */
   return (cp1 * cp2 > 0.);
}

/* Test whether the point (x, y) is inside (not on the boundary of) the
   triangle with vertices (vx1, vy1), (vx2, vy2), and (vx3, vy3). */
func inTriangle(x, y, vx1, vy1, vx2, vy2, vx3, vy3 float64) bool {
   return (onSameSide(x, y, vx3, vy3, vx1, vy1, vx2, vy2) &&
           onSameSide(x, y, vx1, vy1, vx2, vy2, vx3, vy3) &&
           onSameSide(x, y, vx2, vy2, vx3, vy3, vx1, vy1));
}

func printGrid(r, c int, xmin, ymin, xmax, ymax, x1, y1, x2, y2, x3, y3 float64) {
   // incremental distances in x and y
   var dx float64 = (xmax - xmin) / float64(c - 1);
   var dy float64 = (ymax - ymin) / float64(r - 1);

   // print grid
   for i := r; i >= -1; i-- {
      for j := -1; j <= c; j++ {
         if i == r || i == -1 {
            if j == -1 || j == c {
               print("+")
            } else {
               print("-")
            }
         } else if j == -1 || j == c {
            print("|");
         } else {
            var x float64 = xmin + float64(j) * dx;
            var y float64 = ymin + float64(i) * dy;
            if inTriangle(x, y, x1, y1, x2, y2, x3, y3) {
               print("*")
            } else {
               print(" ")
            }
         }
      }
      print("\n");
   }
}

func main() {
   printGrid(10, 10, 0., 0., 9., 9., 1., 1., 8., 1., 8., 8.)
}
