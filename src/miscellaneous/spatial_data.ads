--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with CommonText;

package Spatial_Data is

   package CT renames CommonText;

   type Collection_Type is (unset,
                            single_point,
                            single_line_string,
                            single_polygon,
                            multi_point,
                            multi_line_string,
                            multi_polygon,
                            heterogeneous);

   type Geometric_Shape is (point_shape,
                            line_string_shape,
                            polygon_shape,
                            mixture);

   --  The range limits are necessary to avoid storage error warnings
   subtype Geo_Points is Positive range 1 .. 2 ** 20;
   subtype Geo_Units  is Natural  range 0 .. 2 ** 20;

   --  Mutable variant records must be limited if tagged
   --  However, limited geometry cannot work.  We must be able to change the
   --  contents discriminate, so it meants this record cannot be tagged.
   type Geometry (contents : Collection_Type := unset;
                  units    : Geo_Units       := Geo_Units'First;
                  subunits : Geo_Units       := Geo_Units'First;
                  points   : Geo_Points      := Geo_Points'First) is private;

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
   subtype Geometric_Ring        is Geometric_Point_set;
   subtype Geometric_Line_String is Geometric_Point_set;
   subtype Geometric_Line        is Geometric_Line_String (1 .. 2);
   type Geometric_Polygon (rings  : Geo_Units := Geo_Units'First;
                           points : Geo_Points := Geo_Points'First) is private;

   Origin_Point   : constant Geometric_Point := (0.0, 0.0);
   Blank_Geometry : constant Geometry;


   --------------------------------
   --  Initialization functions  --
   --------------------------------
   function start_polygon               (outer_ring : Geometric_Ring)
                                         return Geometric_Polygon;
   procedure append_inner_ring          (polygon    : in out Geometric_Polygon;
                                         inner_ring : Geometric_Ring);

   function initialize_as_point         (point : Geometric_Point)
                                         return Geometry;
   function initialize_as_multi_point   (point : Geometric_Point)
                                         return Geometry;

   function initialize_as_line          (line_string : Geometric_Line_String)
                                         return Geometry;
   function initialize_as_multi_line    (line_string : Geometric_Line_String)
                                         return Geometry;

   function initialize_as_polygon       (polygon : Geometric_Polygon)
                                         return Geometry;
   function initialize_as_multi_polygon (polygon : Geometric_Polygon)
                                         return Geometry;

   function initialize_as_collection    (anything : Geometry) return Geometry;


   -----------------------------------
   --  Build collections functions  --
   -----------------------------------
   procedure augment_multi_point   (collection : in out Geometry;
                                    point      : Geometric_Point);

   procedure augment_multi_polygon (collection : in out Geometry;
                                    polygon    : Geometric_Polygon);

   procedure augment_multi_line    (collection : in out Geometry;
                                    line       : Geometric_Line_String);

   procedure augment_collection    (collection : in out Geometry;
                                    anything   : Geometry);


   ---------------------------
   --  Retrieval functions  --
   ---------------------------
   function type_of_collection     (collection : Geometry)
                                    return Collection_Type;
   function size_of_collection     (collection : Geometry)
                                    return Positive;
   function collection_item_shape  (collection : Geometry;
                                    index      : Positive := 1)
                                    return Geometric_Shape;
   function collection_item_type   (collection : Geometry;
                                    index      : Positive := 1)
                                    return Collection_Type;

   function retrieve_subcollection (collection : Geometry;
                                    index : Positive := 1)
                                    return Geometry;

   function retrieve_point         (collection : Geometry;
                                    index : Positive := 1)
                                    return Geometric_Point;
   function retrieve_line          (collection : Geometry;
                                    index : Positive := 1)
                                    return Geometric_Line_String;

   function retrieve_polygon       (collection : Geometry;
                                    index      : Positive := 1)
                                    return Geometric_Polygon;
   function number_of_rings        (polygon : Geometric_Polygon)
                                    return Natural;
   function retrieve_ring          (polygon : Geometric_Polygon;
                                    ring_index : Positive)
                                    return Geometric_Ring;

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

   type Ring_Structure is
      record
         Item_ID     : Positive;
         Ring_ID     : Positive;
         Ring_Size   : Positive;
         Point_Index : Positive;
      end record;

   type Ring_Structures is array (Positive range <>) of Ring_Structure;

   type Geometric_Polygon (rings  : Geo_Units := Geo_Units'First;
                           points : Geo_Points := Geo_Points'First) is
      record
         structures : Ring_Structures (1 .. rings);
         points_set : Geometric_Point_Collection (1 .. points) :=
                      (others => Origin_Point);
      end record;

   type Geometry (contents : Collection_Type := unset;
                  units    : Geo_Units := Geo_Units'First;
                  subunits : Geo_Units := Geo_Units'First;
                  points   : Geo_Points := Geo_Points'First) is
      record
         case contents is
            when unset => null;
            when others =>
               structures : Ring_Structures (1 .. subunits);
               points_set : Geometric_Point_Collection (1 .. points) :=
                            (others => Origin_Point);
         end case;
      end record;

   Blank_Geometry : constant Geometry := (unset, 0, 0, 1);

   --  returns a trimmed floating point image
   function format_real (value : Geometric_Real) return String;

--     --  Returns starting position in polygon_set for given index
--     --  For 2-ring polygons, this is same as outer ring
--     function outer_polygon_position (collection : Geometry; item : Positive)
--                                      return Positive;
--
--     --  Returns starting position of inner ring of 2-ring polygons
--     function inner_polygon_position (collection : Geometry; item : Positive;
--                                      hole_item : Positive) return Positive;
--
--     --  Given a starting position, returns the number of points in the polygon
--     function polygon_ring_size (collection : Geometry; position : Positive)
--                                 return Positive;
--
--     function group_size (collection : Geometry;
--                          position   : Positive) return Natural;
--
--     --  raises exception if index is out of range
--     procedure check_collection_index (collection : Geometry; index : Positive);

--     --  Raises exception if index is not found, otherwise locates exactly
--     --  where shapes starts in the array and how many points it contains
--     procedure locate_heterogenous_item (collection : Geometry;
--                                         index      : Positive;
--                                         set_index  : out Positive;
--                                         num_points : out Positive);
--
--     --  Returns heterogenous section of a polygon including its holes
--     function retrieve_full_polygon (collection : Geometry;
--                                     index : Positive := 1)
--                                     return Heterogeneous_Collection;

end Spatial_Data;
