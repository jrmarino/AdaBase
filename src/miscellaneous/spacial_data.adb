--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body Spacial_Data is


   ---------------------------
   --  initialize_as_point  --
   ---------------------------
   function initialize_as_point (point : Geometric_Point) return Geometry
   is
      product : Geometry := (single_point, 1, point);
   begin
      return product;
   end initialize_as_point;


   ----------------------------
   --  initialize_as_circle  --
   ----------------------------
   function initialize_as_circle (circle : Geometric_Circle) return Geometry
   is
      product : Geometry := (single_circle, 1, circle);
   begin
      return product;
   end initialize_as_circle;


   --------------------------
   --  initialize_as_line  --
   --------------------------
   function initialize_as_line (line : Geometric_Line) return Geometry
   is
      product : Geometry := (single_line, 1, line);
   begin
      return product;
   end initialize_as_line;


   ---------------------------------
   --  initialize_as_line_string  --
   ---------------------------------
   function initialize_as_line_string (line_string : Geometric_Line_String)
                                       return Geometry
   is
      product : Geometry := (single_line_string, 1, line_string);
   begin
      return product;
   end initialize_as_line_string;


   -----------------------------------
   --  initialize_as_infinite_line  --
   -----------------------------------
   function initialize_as_infinite_line (two_points_on_line : Geometric_Line)
                                         return Geometry
   is
      product : Geometry := (single_infinite_line, 1, two_points_on_line);
   begin
      return product;
   end initialize_as_infinite_line;


   -----------------------------
   --  initialize_as_polygon  --
   -----------------------------
   function initialize_as_polygon (polygon : Geometric_Polygon)
                                   return Geometry
   is
      product : Geometry := (single_polygon, 1, polygon);
   begin
      return product;
   end initialize_as_polygon;


end Spacial_Data;
