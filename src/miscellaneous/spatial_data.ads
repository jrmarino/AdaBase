--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with CommonText;

package Spatial_Data is

   package CT renames CommonText;

   type Collection_Type is (unset,
                            single_point,
                            single_line_string,
                            single_infinite_line,
                            single_circle,
                            single_polygon,
                            multi_point,
                            multi_line_string,
                            multi_polygon,
                            heterogeneous);

   type Geometric_Shape is (point_shape,
                            line_string_shape,
                            infinite_line_shape,
                            circle_shape,
                            polygon_shape,
                            mixture);

   --  The range limits are necessary to avoid storage error warnings
   subtype Geo_Points is Positive range 1 .. 2 ** 20;
   subtype Geo_Units  is Natural  range 0 .. 2 ** 20;

   --  Mutable variant records must be limited if tagged
   --  However, limited geometry cannot work.  We must be able to change the
   --  contents discriminate, so it meants this record cannot be tagged.
   type Geometry (contents : Collection_Type := unset;
                  points   : Geo_Points := Geo_Points'First;
                  units    : Geo_Units := Geo_Units'First) is private;

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
   Blank_Geometry : constant Geometry;

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
   procedure append_complex_geometry (collection : out Geometry;
                                      subcollection : Geometry);

   ---------------------------
   --  Retrieval functions  --
   ---------------------------
   function type_of_collection    (collection : Geometry)
                                   return Collection_Type;
   function size_of_collection    (collection : Geometry)
                                   return Positive;
   function collection_item_shape (collection : Geometry;
                                   index      : Positive := 1)
                                   return Geometric_Shape;
   function collection_item_type  (collection : Geometry;
                                   index      : Positive := 1)
                                   return Collection_Type;
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
   function retrieve_subcollection (collection : Geometry; index : Positive := 1)
                                    return Geometry;

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
   function Well_Known_Text (collection : Geometry;
                             top_first  : Boolean := True) return String;


   CONVERSION_FAILED       : exception;
   OUT_OF_COLLECTION_RANGE : exception;
   LACKING_POINTS          : exception;
   ILLEGAL_POLY_HOLE       : exception;
   ILLEGAL_SHAPE           : exception;

private

   subtype Geometric_Point_Collection is Geometric_Point_set;

   type Homogeneous_Collection_Unit is
      record
         point    : Geometric_Point;
         shape_id : Positive;
      end record;

   type Heterogeneous_Collection_Unit is
      record
         --  for primate shapes, group_id = shape_id and component = 1
         group_id   : Positive;
         group_type : Collection_Type;
         shape_id   : Positive;
         shape      : Geometric_Shape;
         component  : Positive;
         point      : Geometric_Point;
      end record;

   Homogeneous_Dummy : constant Homogeneous_Collection_Unit :=
                       (Origin_Point, 1);

   heterogeneous_Dummy : constant Heterogeneous_Collection_Unit :=
                         (1, single_point, 1, point_shape, 1, Origin_Point);

   type Homogeneous_Collection is
       array (Positive range <>) of Homogeneous_Collection_Unit;

   type Heterogeneous_Collection is
       array (Positive range <>) of Heterogeneous_Collection_Unit;

   type Geometry (contents : Collection_Type := unset;
                  points   : Geo_Points := Geo_Points'First;
                  units    : Geo_Units := Geo_Units'First) is
      record
         case contents is
            when unset => null;
            when single_point =>
               point : Geometric_Point := Origin_Point;
            when single_circle =>
               circle : Geometric_Circle := (center_point => Origin_Point,
                                             radius       => 1.0);
            when single_infinite_line =>
               infinite_line : Geometric_Line := (others => Origin_Point);
            when single_line_string =>
               line_string : Geometric_Line_String (1 .. points) :=
                             (others => Origin_Point);
            when single_polygon =>
               polygon : Geometric_Polygon (1 .. points) :=
                         (others => Origin_Point);
            when multi_point =>
               set_points : Geometric_Point_Collection (1 .. points) :=
                            (others => Origin_Point);
            when multi_line_string =>
               set_line_strings : Homogeneous_Collection (1 .. points) :=
                                  (others => Homogeneous_Dummy);
            when multi_polygon =>
               --  Includes the "holes" of a single polygon.  Appending a
               --  hole will change the container from polygon to set_polygons
               set_polygons : Heterogeneous_Collection (1 .. points) :=
                              (others => heterogeneous_Dummy);
            when heterogeneous =>
               set_heterogeneous : Heterogeneous_Collection (1 .. points) :=
                                   (others => heterogeneous_Dummy);
         end case;
      end record;

   Blank_Geometry : constant Geometry := (unset, 1, 0);

   --  returns a trimmed floating point image
   function format_real (value : Geometric_Real) return String;

   --  Returns starting position in polygon_set for given index
   --  For 2-ring polygons, this is same as outer ring
   function outer_polygon_position (collection : Geometry; item : Positive)
                                    return Positive;

   --  Returns starting position of inner ring of 2-ring polygons
   function inner_polygon_position (collection : Geometry; item : Positive;
                                    hole_item : Positive) return Positive;

   --  Given a starting position, returns the number of points in the polygon
   function polygon_ring_size (collection : Geometry; position : Positive)
                               return Positive;

   function group_size (collection : Geometry;
                        position   : Positive) return Natural;

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
