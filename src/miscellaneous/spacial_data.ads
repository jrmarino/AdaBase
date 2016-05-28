--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package Spacial_Data is

   type Collection_Type is (single_point,
                            single_line,
                            single_line_string,
                            single_infinite_line,
                            single_circle,
                            single_polygon,
                            multi_point,
                            multi_line_string,
                            multi_polygon,
                            heterogeneous);

   type Geometry (contents : Collection_Type; items : Positive) is private;
   type Geometric_Real is digits 18;

   type Geometric_Point is
      record
         X : Geometric_Real;
         Y : Geometric_Real;
      end record;

   type Geometric_Circle is
      record
         center_point : Geometric_Point;
         radius       : Geometric_Real;
      end record;

   type Geometric_Point_set is array (Positive range <>) of Geometric_Point;
   subtype Geometric_Polygon is Geometric_Point_set;
   subtype Geometric_Line_String is Geometric_Point_set;
   subtype Geometric_Line is Geometric_Point_set (1 .. 2);

   --------------------------------
   --  Initialization functions  --
   --------------------------------
   function initialize_as_point   (point   : Geometric_Point) return Geometry;
   function initialize_as_line    (line    : Geometric_Line) return Geometry;
   function initialize_as_circle  (circle  : Geometric_Circle) return Geometry;
   function initialize_as_polygon (polygon : Geometric_Polygon)
                                   return Geometry;
   function initialize_as_line_string (line_string : Geometric_Line_String)
                                       return Geometry;
   function initialize_as_infinite_line (two_points_on_line : Geometric_Line)
                                         return Geometry;

private

   type Geometric_Shape is (point, line, line_string, infinite_line,
                            circle, polygon);


   subtype Geometric_Point_Collection is Geometric_Point_set;

   type Homogeneous_Collection_Unit is
      record
         point    : Geometric_Point;
         shape_id : Positive;
      end record;

   type Heterogeneous_Collection_Unit is
      record
         point    : Geometric_Point;
         shape    : Geometric_Shape;
         shape_id : Positive;
      end record;

   type Homogeneous_Collection is
       array (Positive range <>) of Homogeneous_Collection_Unit;

   type Heterogeneous_Collection is
       array (Positive range <>) of Heterogeneous_Collection_Unit;

   type Geometry (contents : Collection_Type; items : Positive) is tagged
      record
         case contents is
            when single_point =>
               point : Geometric_Point;
            when single_circle =>
               circle : Geometric_Circle;
            when single_line =>
               line : Geometric_Line;
            when single_infinite_line =>
               infinite_line : Geometric_Line;
            when single_line_string =>
               line_string : Geometric_Line_String (1 .. items);
            when single_polygon =>
               polygon : Geometric_Polygon (1 .. items);
            when multi_point =>
               set_points : Geometric_Point_Collection (1 .. items);
            when multi_line_string =>
               set_line_strings : Homogeneous_Collection (1 .. items);
            when multi_polygon =>
               set_polygons : Homogeneous_Collection (1 .. items);
            when heterogeneous =>
               set_heterogeneous : Heterogeneous_Collection (1 .. items);
         end case;
      end record;

end Spacial_Data;
