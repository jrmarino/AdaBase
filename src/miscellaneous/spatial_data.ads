--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package Spatial_Data is

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

   type Geometric_Shape is (point_shape,
                            line_shape,
                            line_string_shape,
                            infinite_line_shape,
                            circle_shape,
                            polygon_shape);

   type Geometry (contents : Collection_Type; items : Positive) is
     tagged private;

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

   type Slope_Intercept is
      record
         slope       : Geometric_Real;
         y_intercept : Geometric_Real;
         vertical    : Boolean;
      end record;

   type Standard_Form is
      record
         A : Geometric_Real;
         B : Geometric_Real;
         C : Geometric_Real;
      end record;

   type Geometric_Point_set is array (Positive range <>) of Geometric_Point;
   subtype Geometric_Polygon is Geometric_Point_set;
   subtype Geometric_Line_String is Geometric_Point_set;
   subtype Geometric_Line is Geometric_Point_set (1 .. 2);

   Origin_Point : constant Geometric_Point := (0.0, 0.0);

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

   -----------------------------------
   --  Build collections functions  --
   -----------------------------------
   procedure append_point (collection : out Geometry; point : Geometric_Point);
   procedure append_line  (collection : out Geometry; line : Geometric_Line);
   procedure append_line_string (collection  : out Geometry;
                                 line_string : Geometric_Line_String);
   procedure append_polygon_hole (collection : out Geometry;
                                  polygon    : Geometric_Polygon);
   procedure append_polygon      (collection : out Geometry;
                                  polygon    : Geometric_Polygon);

   ---------------------------
   --  Retrieval functions  --
   ---------------------------
   function size_of_collection    (collection : Geometry) return Positive;
   function collection_item_shape (collection : Geometry;
                                   index      : Positive := 1)
                                   return Geometric_Shape;
   function retrieve_polygon     (collection : Geometry; index : Positive := 1)
                                  return Geometric_Polygon;
   function retrieve_hole        (collection : Geometry; index : Positive := 1;
                                  hole_index : Positive)
                                  return Geometric_Polygon;
   function retrieve_point       (collection : Geometry; index : Positive := 1)
                                  return Geometric_Point;
   function retrieve_line        (collection : Geometry; index : Positive := 1)
                                  return Geometric_Line;
   function retrieve_line_string (collection : Geometry; index : Positive := 1)
                                  return Geometric_Line_String;
   function retrieve_circle      (collection : Geometry)
                                  return Geometric_Circle;

   function retrieve_two_points_of_infinite_line (collection : Geometry)
                                                  return Geometric_Line;

   function number_of_polygon_holes (collection : Geometry; index : Positive)
                                     return Natural;

   -------------------
   --  Conversions  --
   -------------------

   function convert_infinite_line (line : Geometric_Line)
                                   return Slope_Intercept;

   function convert_infinite_line (line : Geometric_Line) return Standard_Form;

   function convert_to_infinite_line (std_form : Standard_Form)
                                      return Geometric_Line;

   function convert_to_infinite_line (intercept_form : Slope_Intercept)
                                      return Geometric_Line;

   ---------------------------
   --  Text Representation  --
   ---------------------------
   function mysql_text (collection : Geometry) return String;
   function Well_Known_Text (collection : Geometry) return String;


   CONVERSION_FAILED       : exception;
   OUT_OF_COLLECTION_RANGE : exception;
   LACKING_POINTS          : exception;
   ILLEGAL_POLY_HOLE       : exception;

private

   subtype Geometric_Point_Collection is Geometric_Point_set;

   type Homogeneous_Collection_Unit is
      record
         point    : Geometric_Point;
         shape_id : Positive;
      end record;

   type Heterogeneous_Collection_Unit is
      record
         point     : Geometric_Point;
         shape     : Geometric_Shape;
         shape_id  : Positive;
         component : Positive := 1;
      end record;

   Homogeneous_Dummy : constant Homogeneous_Collection_Unit :=
                       (Origin_Point, 1);

   heterogeneous_Dummy : constant Heterogeneous_Collection_Unit :=
                         (Origin_Point, point_shape, 1, 1);

   type Homogeneous_Collection is
       array (Positive range <>) of Homogeneous_Collection_Unit;

   type Heterogeneous_Collection is
       array (Positive range <>) of Heterogeneous_Collection_Unit;

   type Geometry (contents : Collection_Type; items : Positive) is tagged
      record
         case contents is
            when single_point =>
               point : Geometric_Point := Origin_Point;
            when single_circle =>
               circle : Geometric_Circle := (center_point => Origin_Point,
                                             radius       => 1.0);
            when single_line =>
               line : Geometric_Line := (others => Origin_Point);
            when single_infinite_line =>
               infinite_line : Geometric_Line := (others => Origin_Point);
            when single_line_string =>
               line_string : Geometric_Line_String (1 .. items) :=
                             (others => Origin_Point);
            when single_polygon =>
               polygon : Geometric_Polygon (1 .. items) :=
                         (others => Origin_Point);
            when multi_point =>
               set_points : Geometric_Point_Collection (1 .. items) :=
                            (others => Origin_Point);
            when multi_line_string =>
               set_line_strings : Homogeneous_Collection (1 .. items) :=
                                  (others => Homogeneous_Dummy);
            when multi_polygon =>
               --  Includes the "holes" of a single polygon.  Appending a
               --  hole will change the container from polygon to set_polygons
               set_polygons : Heterogeneous_Collection (1 .. items) :=
                              (others => heterogeneous_Dummy);
            when heterogeneous =>
               set_heterogeneous : Heterogeneous_Collection (1 .. items) :=
                                   (others => heterogeneous_Dummy);
         end case;
      end record;

   --  returns a trimmed floating point image
   function format_real (value : Geometric_Real) return String;

   --  Returns starting position in polygon_set for given index
   --  For 2-ring polygons, this is same as outer ring
   function outer_polygon_position (collection : Geometry; item : Positive)
                                    return Positive;
   function outer_polygon_hetero_position (collection : Geometry;
                                           item : Positive)
                                           return Positive;

   --  Returns starting position of inner ring of 2-ring polygons
   function inner_polygon_position (collection : Geometry; item : Positive;
                                   hole_item : Positive) return Positive;
   function inner_polygon_hetero_position (collection : Geometry;
                                           item : Positive;
                                           hole_item : Positive)
                                           return Positive;

   --  Given a starting position, returns the number of points in the polygon
   function polygon_ring_size (collection : Geometry; position : Positive)
                               return Positive;
   function polygon_hetero_ring_size (collection : Geometry;
                                      position : Positive)
                                      return Positive;

   --  raises exception if index is out of range
   procedure check_collection_index (collection : Geometry; index : Positive);

   --  Raises exception if index is not found, otherwise locates exactly
   --  where shapes starts in the array and how many points it contains
   procedure locate_heterogenous_item (collection : Geometry;
                                       index      : Positive;
                                       set_index  : out Positive;
                                       num_points : out Positive);

   --  Returns heterogenous section of a polygon including its holes
   function retrieve_full_polygon (collection : Geometry;
                                   index : Positive := 1)
                                   return Heterogeneous_Collection;

end Spatial_Data;