--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body Spatial_Data is

   ---------------------------
   --  initialize_as_point  --
   ---------------------------
   function initialize_as_point (point : Geometric_Point) return Geometry is
   begin
      return (single_point, 1, 1, point);
   end initialize_as_point;


   ----------------------------
   --  initialize_as_circle  --
   ----------------------------
   function initialize_as_circle (circle : Geometric_Circle) return Geometry is
   begin
      return (single_circle, 1, 1, circle);
   end initialize_as_circle;


   --------------------------
   --  initialize_as_line  --
   --------------------------
   function initialize_as_line (line : Geometric_Line) return Geometry is
   begin
      return (single_line_string, line'Length, 1, line);
   end initialize_as_line;


   ---------------------------------
   --  initialize_as_line_string  --
   ---------------------------------
   function initialize_as_line_string (line_string : Geometric_Line_String)
                                       return Geometry is
   begin
      return (single_line_string, line_string'Length, 1, line_string);
   end initialize_as_line_string;


   -----------------------------------
   --  initialize_as_infinite_line  --
   -----------------------------------
   function initialize_as_infinite_line (two_points_on_line : Geometric_Line)
                                         return Geometry is
   begin
      return  (single_infinite_line,
               two_points_on_line'Length,
               1,
               two_points_on_line);
   end initialize_as_infinite_line;


   -----------------------------
   --  initialize_as_polygon  --
   -----------------------------
   function initialize_as_polygon (polygon : Geometric_Polygon)
                                   return Geometry is
   begin
      if polygon'Length < 4 then
         raise LACKING_POINTS
           with "polygons must have at least 4 points (found only" &
           polygon'Length'Img & ")";
      end if;
      return (single_polygon, polygon'Length, 1, polygon);
   end initialize_as_polygon;


   --------------------
   --  append_point  --
   --------------------
   procedure append_point (collection : out Geometry; point : Geometric_Point)
   is
      classification : Collection_Type := collection.contents;
      num_units   : Natural := collection.units + 1;
   begin
      case classification is
         when single_circle =>
            raise CONVERSION_FAILED
              with "circles cannot be part of a geometric collection";
         when single_infinite_line =>
            raise CONVERSION_FAILED
              with "Infinite lines cannot be part of a geometric collection";
         when unset =>
            collection := (single_point, 1, 1, point);
         when single_point =>
            collection := (multi_point,
                           num_units,
                           num_units,
                           (collection.point, point));
         when single_line_string =>
            declare
               point_count : Natural := collection.line_string'Length + 1;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.line_string'Range loop
                  HC (x).group_id   := 1;
                  HC (x).group_type := single_line_string;
                  HC (x).shape_id   := 1;
                  HC (x).shape      := line_string_shape;
                  HC (x).component  := 1;
                  HC (x).point      := collection.line_string (x);
               end loop;
               HC (HC'Last) := (num_units, single_point,
                                1, point_shape, 1, point);
               collection := (heterogeneous, point_count, num_units, HC);
            end;
         when single_polygon =>
            declare
               point_count : Natural := collection.polygon'Length + 1;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.polygon'Range loop
                  HC (x).group_id   := 1;
                  HC (x).group_type := single_polygon;
                  HC (x).shape_id   := 1;
                  HC (x).shape      := polygon_shape;
                  HC (x).component  := 1;
                  HC (x).point      := collection.polygon (x);
               end loop;
               HC (HC'Last) := (num_units, single_point,
                                1, point_shape, 1, point);
               collection := (heterogeneous, point_count, num_units, HC);
            end;
         when multi_point =>
            declare
               HC : Geometric_Point_Collection (1 .. num_units);
            begin
               HC (1 .. collection.set_points'Last) := collection.set_points;
               HC (HC'Last) := (point);
               collection := (multi_point, num_units, num_units, HC);
            end;
         when multi_line_string =>
            declare
               point_count : Natural := collection.set_line_strings'Length + 1;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.set_line_strings'Range loop
                  HC (x).group_id   := 1;
                  HC (x).group_type := multi_line_string;
                  HC (x).shape_id  := collection.set_line_strings (x).shape_id;
                  HC (x).shape     := line_string_shape;
                  HC (x).component := 1;
                  HC (x).point     := collection.set_line_strings (x).point;
               end loop;
               HC (HC'Last) := (num_units, single_point,
                                1, point_shape, 1, point);
               collection := (heterogeneous, point_count, num_units, HC);
            end;
         when multi_polygon =>
            declare
               point_count : Natural := collection.set_polygons'Length + 1;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               HC (collection.set_polygons'Range) := collection.set_polygons;
               HC (HC'Last) := (num_units, single_point,
                                1, point_shape, 1, point);
               collection := (heterogeneous, point_count, num_units, HC);
            end;
         when heterogeneous =>
            declare
               point_count : Natural :=
                 collection.set_heterogeneous'Length + 1;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               HC (1 .. collection.set_heterogeneous'Last) :=
                 collection.set_heterogeneous;
               HC (HC'Last) := (num_units, single_point,
                                1, point_shape, 1, point);
               collection := (heterogeneous, point_count, num_units, HC);
            end;
      end case;
   end append_point;


   -------------------
   --  append_line  --
   -------------------
   procedure append_line (collection : out Geometry; line : Geometric_Line)
   is
      LS : Geometric_Line_String (1 .. 2) := (line (1), line (2));
   begin
      append_line_string (collection, LS);
   end append_line;


   --------------------------
   --  append_line_string  --
   --------------------------
   procedure append_line_string (collection : out Geometry;
                                 line_string : Geometric_Line_String)
   is
      classification : Collection_Type := collection.contents;
      product     : Geometry := initialize_as_point ((0.0, 0.0));
      num_units   : Natural := collection.units + 1;
   begin
      case classification is
         when single_circle =>
            raise CONVERSION_FAILED
              with "circles cannot be part of a geometric collection";
         when single_infinite_line =>
            raise CONVERSION_FAILED
              with "Infinite lines cannot be part of a geometric collection";
         when unset =>
            product := (single_line_string, 1, 1, line_string);
         when single_point =>
            declare
               point_count : Natural := 1 + line_string'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               HC (1).group_id   := 1;
               HC (1).group_type := single_point;
               HC (1).shape_id   := 1;
               HC (1).shape      := point_shape;
               HC (1).component  := 1;
               HC (1).point      := collection.point;
               for x in line_string'Range loop
                  HC (x + 1).group_id   := num_units;
                  HC (x + 1).group_type := single_line_string;
                  HC (x + 1).shape_id   := x;
                  HC (x + 1).shape      := line_string_shape;
                  HC (x + 1).component  := 1;
                  HC (x + 1).point      := line_string (x);
               end loop;
               product := (heterogeneous, point_count, num_units, HC);
            end;
         when single_line_string =>
            declare
               point_count : Natural := collection.line_string'Length +
                                        line_string'Length;
               LL : Natural := collection.line_string'Length;
               HC : Homogeneous_Collection (1 .. point_count);
            begin
               for x in collection.line_string'Range loop
                  HC (x).shape_id := 1;
                  HC (x).point    := collection.line_string (x);
               end loop;
               for x in line_string'Range loop
                  HC (x + LL).shape_id := num_units;
                  HC (x + LL).point    := line_string (x);
               end loop;
               product := (multi_line_string, point_count, num_units, HC);
            end;
         when single_polygon =>
            declare
               point_count : Natural := collection.polygon'Length +
                                        line_string'Length;
               LL : Natural := collection.polygon'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.polygon'Range loop
                  HC (x).group_id   := 1;
                  HC (x).group_type := single_polygon;
                  HC (x).shape_id   := 1;
                  HC (x).shape      := polygon_shape;
                  HC (x).component  := 1;
                  HC (x).point      := collection.polygon (x);
               end loop;
               for x in line_string'Range loop
                  HC (x + LL).group_id   := num_units;
                  HC (x + LL).group_type := single_line_string;
                  HC (x + LL).shape_id   := x;
                  HC (x + LL).shape      := line_string_shape;
                  HC (x + LL).component  := 1;
                  HC (x + LL).point      := line_string (x);
               end loop;
               product := (heterogeneous, point_count, num_units, HC);
            end;
         when multi_point =>
            declare
               point_count : Natural := collection.set_points'Length +
                                        line_string'Length;
               LL : Natural := collection.set_points'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.set_points'Range loop
                  HC (x).group_id   := 1;
                  HC (x).group_type := multi_point;
                  HC (x).shape_id   := x;
                  HC (x).shape      := point_shape;
                  HC (x).component  := 1;
                  HC (x).point      := collection.set_points (x);
               end loop;
               for x in line_string'Range loop
                  HC (x + LL).group_id   := num_units;
                  HC (x + LL).group_type := single_line_string;
                  HC (x + LL).shape_id   := x;
                  HC (x + LL).shape      := line_string_shape;
                  HC (x + LL).component  := 1;
                  HC (x + LL).point      := line_string (x);
               end loop;
               product := (heterogeneous, point_count, num_units, HC);
            end;
         when multi_line_string =>
            declare
               point_count : Natural := collection.set_line_strings'Length +
                                        line_string'Length;
               LL : Natural := collection.set_line_strings'Length;
               HC : Homogeneous_Collection (1 .. point_count);
            begin
               HC (collection.set_points'Range) := collection.set_line_strings;
               for x in line_string'Range loop
                  HC (x + LL).shape_id := num_units;
                  HC (x + LL).point    := line_string (x);
               end loop;
               product := (multi_line_string, point_count, num_units, HC);
            end;
         when multi_polygon =>
            declare
               point_count : Natural := collection.set_polygons'Length +
                                        line_string'Length;
               LL : Natural := collection.set_polygons'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.set_polygons'Range loop
                  HC (x).group_id   := 1;
                  HC (x).group_type := multi_polygon;
                  HC (x).shape_id   := collection.set_polygons (x).shape_id;
                  HC (x).shape      := polygon_shape;
                  HC (x).component  := collection.set_polygons (x).component;
                  HC (x).point      := collection.set_polygons (x).point;
               end loop;
               for x in line_string'Range loop
                  HC (x + LL).group_id   := num_units;
                  HC (x + LL).group_type := single_line_string;
                  HC (x + LL).shape_id   := x;
                  HC (x + LL).shape      := line_string_shape;
                  HC (x + LL).component  := 1;
                  HC (x + LL).point      := line_string (x);
               end loop;
               product := (heterogeneous, point_count, num_units, HC);
            end;
         when heterogeneous =>
            declare
               point_count : Natural := collection.set_heterogeneous'Length +
                                        line_string'Length;
               LL : Natural := collection.set_heterogeneous'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               HC (collection.set_heterogeneous'Range) :=
                 collection.set_heterogeneous;
               for x in line_string'Range loop
                  HC (x + LL).group_id   := num_units;
                  HC (x + LL).group_type := single_line_string;
                  HC (x + LL).shape_id   := x;
                  HC (x + LL).shape      := line_string_shape;
                  HC (x + LL).component  := 1;
                  HC (x + LL).point      := line_string (x);
               end loop;
               product := (heterogeneous, point_count, num_units, HC);
            end;
      end case;
      collection := product;
   end append_line_string;


   ----------------------
   --  append_polygon  --
   ----------------------
   procedure append_polygon (collection : out Geometry;
                             polygon : Geometric_Polygon)
   is
      classification : Collection_Type := collection.contents;
      product     : Geometry := initialize_as_point ((0.0, 0.0));
      num_units   : Natural := collection.units + 1;
   begin
      if polygon'Length < 4 then
         raise LACKING_POINTS
           with "polygons must have at least 4 points (found only" &
           polygon'Length'Img & ")";
      end if;
      case classification is
         when single_circle =>
            raise CONVERSION_FAILED
              with "circles cannot be part of a geometric collection";
         when single_infinite_line =>
            raise CONVERSION_FAILED
              with "Infinite lines cannot be part of a geometric collection";
         when unset =>
            product := (single_polygon, polygon'Length, 1, polygon);
         when single_point =>
            declare
               point_count : Natural := 1 + polygon'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               HC (1).group_id   := 1;
               HC (1).group_type := single_point;
               HC (1).shape_id   := 1;
               HC (1).shape      := point_shape;
               HC (1).component  := 1;
               HC (1).point      := collection.point;
               for x in polygon'Range loop
                  HC (x + 1).group_id   := num_units;
                  HC (x + 1).group_type := single_polygon;
                  HC (x + 1).shape_id   := 1;
                  HC (x + 1).shape      := polygon_shape;
                  HC (x + 1).component  := 1;
                  HC (x + 1).point      := polygon (x);
               end loop;
               product := (heterogeneous, point_count, num_units, HC);
            end;
         when single_line_string =>
            declare
               point_count : Natural := collection.line_string'Length +
                                        polygon'Length;
               LL : Natural := collection.line_string'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.line_string'Range loop
                  HC (x).group_id   := 1;
                  HC (x).group_type := single_line_string;
                  HC (x).shape_id   := 1;
                  HC (x).shape      := line_string_shape;
                  HC (x).component  := 1;
                  HC (x).point      := collection.line_string (x);
               end loop;
               for x in polygon'Range loop
                  HC (x + LL).group_id   := num_units;
                  HC (x + LL).group_type := single_polygon;
                  HC (x + LL).shape_id   := 1;
                  HC (x + LL).shape      := polygon_shape;
                  HC (x + LL).component  := 1;
                  HC (x + LL).point      := polygon (x);
               end loop;
               product := (heterogeneous, point_count, num_units, HC);
            end;
         when single_polygon =>
            declare
               point_count : Natural := collection.polygon'Length +
                                        polygon'Length;
               LL : Natural := collection.polygon'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.polygon'Range loop
                  HC (x).group_id   := 1;
                  HC (x).group_type := single_polygon;
                  HC (x).shape_id   := 1;
                  HC (x).shape      := polygon_shape;
                  HC (x).component  := 1;
                  HC (x).point      := collection.polygon (x);
               end loop;
               for x in polygon'Range loop
                  HC (x + LL).group_id   := num_units;
                  HC (x + LL).group_type := single_polygon;
                  HC (x + LL).shape_id   := 1;
                  HC (x + LL).shape      := polygon_shape;
                  HC (x + LL).component  := 1;
                  HC (x + LL).point      := polygon (x);
               end loop;
               product := (multi_polygon, point_count, num_units, HC);
            end;
         when multi_point =>
            declare
               point_count : Natural := collection.set_points'Length +
                                        polygon'Length;
               LL : Natural := collection.set_points'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.set_points'Range loop
                  HC (x).group_id   := 1;
                  HC (x).group_type := multi_point;
                  HC (x).shape_id   := x;
                  HC (x).shape      := point_shape;
                  HC (x).component := 1;
                  HC (x).point      := collection.set_points (x);
               end loop;
               for x in polygon'Range loop
                  HC (x + LL).group_id   := num_units;
                  HC (x + LL).group_type := single_polygon;
                  HC (x + LL).shape_id   := 1;
                  HC (x + LL).shape      := polygon_shape;
                  HC (x + LL).component  := 1;
                  HC (x + LL).point      := polygon (x);
               end loop;
               product := (heterogeneous, point_count, num_units, HC);
            end;
         when multi_line_string =>
            declare
               point_count : Natural := collection.set_line_strings'Length +
                                        polygon'Length;
               LL : Natural := collection.set_line_strings'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.set_line_strings'Range loop
                  HC (x).group_id   := 1;
                  HC (x).group_type := multi_line_string;
                  HC (x).shape_id  := collection.set_line_strings (x).shape_id;
                  HC (x).shape     := line_string_shape;
                  HC (x).component := 1;
                  HC (x).point     := collection.set_line_strings (x).point;
               end loop;
               for x in polygon'Range loop
                  HC (x + LL).group_id   := num_units;
                  HC (x + LL).group_type := single_polygon;
                  HC (x + LL).shape_id   := 1;
                  HC (x + LL).shape      := polygon_shape;
                  HC (x + LL).component  := 1;
                  HC (x + LL).point      := polygon (x);
               end loop;
               product := (heterogeneous, point_count, num_units, HC);
            end;
         when multi_polygon =>
            declare
               point_count : Natural := collection.set_polygons'Length +
                                        polygon'Length;
               LL : Natural := collection.set_polygons'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               HC (collection.set_polygons'Range) := collection.set_polygons;
               for x in polygon'Range loop
                  HC (x + LL).group_id   := num_units;
                  HC (x + LL).group_type := single_polygon;
                  HC (x + LL).shape_id   := 1;
                  HC (x + LL).shape      := polygon_shape;
                  HC (x + LL).component  := 1;
                  HC (x + LL).point      := polygon (x);
               end loop;
               product := (multi_polygon, point_count, num_units, HC);
            end;
         when heterogeneous =>
            declare
               point_count : Natural := collection.set_heterogeneous'Length +
                                        polygon'Length;
               LL : Natural := collection.set_heterogeneous'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               HC (collection.set_heterogeneous'Range) :=
                 collection.set_heterogeneous;
               for x in polygon'Range loop
                  HC (x + LL).group_id   := num_units;
                  HC (x + LL).group_type := single_polygon;
                  HC (x + LL).shape_id   := 1;
                  HC (x + LL).shape      := polygon_shape;
                  HC (x + LL).component  := 1;
                  HC (x + LL).point      := polygon (x);
               end loop;
               product := (heterogeneous, point_count, num_units, HC);
            end;
      end case;
      collection := product;
   end append_polygon;


   ---------------------------
   --  append_polygon_hole  --
   ---------------------------
   procedure append_polygon_hole (collection : out Geometry;
                                  polygon : Geometric_Polygon)
   is
      classification : Collection_Type := collection.contents;
      product     : Geometry := initialize_as_point ((0.0, 0.0));
   begin
      if polygon'Length < 4 then
         raise LACKING_POINTS
           with "polygons must have at least 4 points (found only" &
           polygon'Length'Img & ")";
      end if;
      case classification is
         when single_polygon =>
            declare
               point_count : Natural := collection.polygon'Length +
                                        polygon'Length;
               LL : Natural := collection.polygon'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.polygon'Range loop
                  HC (x).group_id   := 1;
                  HC (x).group_type := single_polygon;
                  HC (x).shape_id   := 1;
                  HC (x).shape      := polygon_shape;
                  HC (x).component  := 1;
                  HC (x).point      := collection.polygon (x);
               end loop;
               for x in polygon'Range loop
                  HC (x + LL).group_id   := 1;
                  HC (x + LL).group_type := single_polygon;
                  HC (x + LL).shape_id   := 1;
                  HC (x + LL).shape      := polygon_shape;
                  HC (x + LL).component  := 2;
                  HC (x + LL).point      := polygon (x);
               end loop;
               product := (multi_polygon, point_count, 1, HC);
            end;
         when multi_polygon =>
            declare
               point_count : Natural := collection.set_polygons'Length +
                                        polygon'Length;
               LL : Natural := collection.set_polygons'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
               last_id : Positive := collection.set_polygons (LL).shape_id;
               last_sc : Positive := collection.set_polygons (LL).component;
               last_gr : Positive := collection.set_polygons (LL).group_id;
            begin
               HC (collection.set_polygons'Range) := collection.set_polygons;
               for x in polygon'Range loop
                  HC (x + LL).group_id   := last_gr;
                  HC (x + LL).group_type := multi_polygon;
                  HC (x + LL).shape_id   := last_id;
                  HC (x + LL).shape      := polygon_shape;
                  HC (x + LL).component  := last_sc + 1;
                  HC (x + LL).point      := polygon (x);
               end loop;
               product := (multi_polygon, point_count, last_id, HC);
            end;
         when heterogeneous =>
            declare
               point_count : Natural := collection.set_heterogeneous'Length +
                                        polygon'Length;
               LL : Natural := collection.set_heterogeneous'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
               last_id : Positive;
               last_sc : Positive;
               last_gr : Positive;
            begin
               if collection.set_heterogeneous (LL).shape /= polygon_shape then
                  raise ILLEGAL_POLY_HOLE
                    with "The last inserted shape was not a polygon";
               end if;
               last_id := collection.set_heterogeneous (LL).shape_id;
               last_sc := collection.set_heterogeneous (LL).component;
               last_gr := collection.set_heterogeneous (LL).group_id;
               HC (collection.set_heterogeneous'Range) :=
                 collection.set_heterogeneous;
               for x in polygon'Range loop
                  HC (x + LL).group_id   := last_gr;
                  HC (x + LL).group_type := heterogeneous;
                  HC (x + LL).shape_id   := last_id;
                  HC (x + LL).shape      := polygon_shape;
                  HC (x + LL).component  := last_sc + 1;
                  HC (x + LL).point      := polygon (x);
               end loop;
               product := (heterogeneous, point_count, last_id, HC);
            end;
         when others =>
            raise ILLEGAL_POLY_HOLE
              with "Cannot add polygon hole to " & classification'Img;
      end case;
      collection := product;
   end append_polygon_hole;


   -------------------------------
   --  append_complex_geometry  --
   -------------------------------
   procedure append_complex_geometry (collection : out Geometry;
                                      subcollection : Geometry)
   is
      classification : Collection_Type := collection.contents;
      product     : Geometry := initialize_as_point ((0.0, 0.0));
   begin
      case classification is
         when unset |
              single_point |
              single_line_string |
              single_infinite_line |
              single_circle |
              single_polygon =>
            raise ILLEGAL_SHAPE
              with "Shape is not complex: " & classification'Img;
         when multi_point => null;
         when multi_line_string => null;
         when multi_polygon => null;
         when heterogeneous => null;
      end case;
   end append_complex_geometry;


   --------------------------
   --  size_of_collection  --
   --------------------------
   function size_of_collection (collection : Geometry) return Positive is
   begin
      return collection.units;
   end size_of_collection;


   --------------------------
   --  type_of_collection  --
   --------------------------
   function type_of_collection (collection : Geometry) return Collection_Type
   is
   begin
      return collection.contents;
   end type_of_collection;


   ------------------------------
   --  retrieve_subcollection  --
   ------------------------------
   function retrieve_subcollection (collection : Geometry;
                                    index : Positive := 1) return Geometry
   is
      cset : Heterogeneous_Collection renames collection.set_heterogeneous;
      sub_index  : Positive;
      data_size  : Positive;
   begin
      case collection.contents is
         when unset |
              single_point |
              single_polygon |
              single_line_string |
              single_infinite_line |
              single_circle =>
            raise OUT_OF_COLLECTION_RANGE
              with "Applies only to multi- and mixed geometric collections";
         when multi_point =>
            declare
               pt : Geometric_Point := retrieve_point (collection, index);
            begin
               return initialize_as_point (pt);
            end;
         when multi_line_string =>
            declare
               LS : Geometric_Line_String :=
                    retrieve_line_string (collection, index);
            begin
               return initialize_as_line_string (LS);
            end;
         when multi_polygon =>
            declare
               PG : Geometric_Polygon := retrieve_polygon (collection, index);
               NH : Natural := number_of_polygon_holes (collection, index);
               GM : Geometry := initialize_as_polygon (PG);
            begin
               for hole in 1 .. NH loop
                  append_polygon_hole
                    (GM, retrieve_hole (collection, index, hole));
               end loop;
               return GM;
            end;
         when heterogeneous =>
            --  Index refers to group ID, return everything with this ID
            locate_heterogenous_item (collection => collection,
                                      index      => index,
                                      set_index  => sub_index,
                                      num_points => data_size);
            case cset (sub_index).group_type is
               when unset | single_circle | single_infinite_line =>
                  raise CONVERSION_FAILED
                       with "Illegal heterogenous type";
               when single_point =>
                  return initialize_as_point (cset (sub_index).point);
               when single_line_string =>
                  declare
                     LS : Geometric_Line_String (1 .. data_size);
                  begin
                     for pt in 1 .. data_size loop
                        LS (pt) := cset (sub_index + pt - 1).point;
                     end loop;
                     return initialize_as_line_string (LS);
                  end;
               when heterogeneous =>
                  declare
                     HC : Heterogeneous_Collection (1 .. data_size);
                     item_cnt : Natural;
                  begin
                     item_cnt := group_size (collection, sub_index);
                     HC (HC'Range) := collection.set_heterogeneous
                       (sub_index .. sub_index + data_size - 1);
                     return (contents => heterogeneous,
                             points   => data_size,
                             units    => item_cnt,
                             set_heterogeneous => HC);
                  end;
               when multi_polygon =>
                  declare
                     HC : Heterogeneous_Collection (1 .. data_size);
                     poly_cnt : Natural;
                  begin
                     poly_cnt := group_size (collection, sub_index);
                     HC (HC'Range) := collection.set_heterogeneous
                       (sub_index .. sub_index + data_size - 1);
                     return (contents => multi_polygon,
                             points   => data_size,
                             units    => poly_cnt,
                             set_polygons => HC);
                  end;
               when single_polygon =>
                  declare
                     PG : Geometric_Polygon (1 .. data_size);
                  begin
                     for pt in 1 .. data_size loop
                        PG (pt) := cset (sub_index + pt - 1).point;
                     end loop;
                     return initialize_as_polygon (PG);
                  end;
               when multi_point =>
                  declare
                     PC : Geometric_Point_Collection (1 .. data_size);
                  begin
                     for pt in 1 .. data_size loop
                        PC (pt) := cset (sub_index + pt - 1).point;
                     end loop;
                     return (contents   => multi_point,
                             points     => data_size,
                             units      => data_size,
                             set_points => PC);
                  end;
               when multi_line_string =>
                  declare
                     HC : Homogeneous_Collection (1 .. data_size);
                     line_cnt : Natural;
                  begin
                     line_cnt := group_size (collection, sub_index);
                     for pt in 1 .. data_size loop
                        HC (pt).point :=  cset (sub_index + pt - 1).point;
                        HC (pt).shape_id := cset (sub_index + pt - 1).shape_id;
                     end loop;
                     return (contents => multi_line_string,
                             points   => data_size,
                             units    => line_cnt,
                             set_line_strings => HC);
                  end;
            end case;
      end case;
   end retrieve_subcollection;


   -------------------------------
   --  number_of_polygon_holes  --
   -------------------------------
   function number_of_polygon_holes (collection : Geometry; index : Positive)
                                     return Natural
   is
      position : Positive;
      shape_id : Positive;
      result   : Natural := 0;
   begin
      if collection.contents /= multi_polygon then
         return 0;
      end if;

      position := outer_polygon_position (collection, index);
      shape_id := collection.set_polygons (position).shape_id;
      for x in Positive range
        position + 1 .. collection.set_polygons'Last
      loop
         exit when collection.set_polygons (x).shape_id /= shape_id;
         result := collection.set_polygons (x).component - 1;
      end loop;
      return result;
   end number_of_polygon_holes;


   ------------------
   --  group_size  --
   ------------------
   function group_size (collection : Geometry;
                        position   : Positive) return Natural
   is
      group_id : Positive;
      result   : Natural;
   begin
      group_id := collection.set_heterogeneous (position).group_id;
      for x in position + 1 .. collection.set_heterogeneous'Last loop
         exit when collection.set_heterogeneous (x).group_id /= group_id;
         result := collection.set_heterogeneous (x).shape_id;
      end loop;
      return result;
   end group_size;


   ------------------------------
   --  check_collection_index  --
   ------------------------------
   procedure check_collection_index (collection : Geometry; index : Positive)
   is
   begin
      if index > collection.units then
         raise OUT_OF_COLLECTION_RANGE
           with "Only" & collection.units'Img & " items in collection " &
           "(attempted index of" & index'Img & ")";
      end if;
   end check_collection_index;


   ------------------------------
   --  locate_homogenous_item  --
   ------------------------------
   procedure locate_heterogenous_item (collection : Geometry;
                                       index      : Positive;
                                       set_index  : out Positive;
                                       num_points : out Positive)
   is
      set_index_set : Boolean := False;
   begin
      if collection.contents /= heterogeneous then
         raise OUT_OF_COLLECTION_RANGE with collection.contents'Img &
           ": Only works with heterogeneous collections";
      end if;
      for x in collection.set_heterogeneous'Range loop
         if collection.set_heterogeneous (x).group_id = index then
            if not set_index_set then
               set_index := x;
               set_index_set := True;
               num_points := 1;
            else
               num_points := num_points + 1;
            end if;
         end if;
      end loop;
      if not set_index_set then
         raise CONVERSION_FAILED
           with "data corrupt: expected index" & index'Img &
           " not found, but" & collection.units'Img & " items are present";
      end if;
   end locate_heterogenous_item;


   -------------------------
   --  polygon_ring_size  --
   -------------------------
   function polygon_ring_size (collection : Geometry; position : Positive)
                               return Positive
   is
      current_id : Positive := collection.set_polygons (position).shape_id;
      current_sc : Positive := collection.set_polygons (position).component;
      arrow      : Positive := position;
      size       : Positive := 1;
   begin
      loop
         arrow := arrow + 1;
         exit when arrow > collection.set_polygons'Length;
         exit when collection.set_polygons (arrow).shape_id /= current_id;
         exit when collection.set_polygons (arrow).component /= current_sc;
         size := size + 1;
      end loop;
      return size;
   end polygon_ring_size;


   ------------------------------
   --  outer_polygon_position  --
   ------------------------------
   function outer_polygon_position (collection : Geometry; item : Positive)
                                    return Positive is
   begin
      for x in collection.set_polygons'Range loop
         if collection.set_polygons (x).shape_id = item then
            if collection.set_polygons (x).component = 1 then
               return x;
            end if;
         end if;
      end loop;
      raise CONVERSION_FAILED
        with "Item" & item'Img & " was not found";
   end outer_polygon_position;


   ------------------------------
   --  inner_polygon_position  --
   ------------------------------
   function inner_polygon_position (collection : Geometry; item : Positive;
                                    hole_item : Positive) return Positive is
   begin
      for x in collection.set_polygons'Range loop
         if collection.set_polygons (x).shape_id = item and then
           collection.set_polygons (x).component = hole_item
         then
            return x;
         end if;
      end loop;
      raise CONVERSION_FAILED
        with "Item" & item'Img & "/" & hole_item'Img & " was not found";
   end inner_polygon_position;


   -----------------------------
   --  collection_item_shape  --
   -----------------------------
   function collection_item_shape (collection : Geometry;
                                   index : Positive := 1)
                                   return Geometric_Shape is
   begin
      check_collection_index (collection, index);
      case collection.contents is
         when single_point         => return point_shape;
         when single_line_string   => return line_string_shape;
         when single_infinite_line => return infinite_line_shape;
         when single_circle        => return circle_shape;
         when single_polygon       => return polygon_shape;
         when multi_point          => return point_shape;
         when multi_line_string    => return line_string_shape;
         when multi_polygon        => return polygon_shape;
         when heterogeneous        => return mixture;
         when unset =>
            raise CONVERSION_FAILED
              with "Geometry is unset - it contains zero shapes";
      end case;
   end collection_item_shape;


   ----------------------------
   --  collection_item_type  --
   ----------------------------
   function collection_item_type (collection : Geometry;
                                  index      : Positive := 1)
                                  return Collection_Type is
   begin
      check_collection_index (collection, index);
      case collection.contents is
         when unset => raise CONVERSION_FAILED
              with "geometry is unset (typeless)";
         when single_point |
              single_circle |
              single_polygon |
              single_infinite_line |
              single_line_string =>
            return collection.contents;
         when multi_point => return single_point;
         when multi_line_string => return single_line_string;
         when multi_polygon => return single_polygon;
         when heterogeneous =>
            declare
               sub_index : Positive;
               data_size : Positive;
            begin
               locate_heterogenous_item (collection => collection,
                                         index      => index,
                                         set_index  => sub_index,
                                         num_points => data_size);
               return collection.set_heterogeneous (sub_index).group_type;
            end;
      end case;
   end collection_item_type;


   ----------------------
   --  retrieve_point  --
   ----------------------
   function retrieve_point (collection : Geometry; index : Positive := 1)
                             return Geometric_Point is
   begin
      check_collection_index (collection, index);
      case collection.contents is
         when single_point  => return collection.point;
         when multi_point   => return collection.set_points (index);
         when heterogeneous =>
            raise CONVERSION_FAILED
              with "Requested polygon from mixed collection. " &
              "(Extract using retrieve_subcollection instead)";
         when others =>
            raise CONVERSION_FAILED
              with "Requested point, but shape is " &
              collection_item_shape (collection, index)'Img;
      end case;
   end retrieve_point;


   ---------------------
   --  retrieve_line  --
   ---------------------
   function retrieve_line (collection : Geometry; index : Positive := 1)
                           return Geometric_Line is
   begin
      check_collection_index (collection, index);
      declare
         product : Geometric_Line_String :=
                   retrieve_line_string (collection, index);
      begin
         if product'Length > 2 then
            raise CONVERSION_FAILED
              with "Object consists of more than 1 line (it's a line string)";
         end if;
         return product;
      end;
   end retrieve_line;


   ----------------------------
   --  retrieve_line_string  --
   ----------------------------
   function retrieve_line_string (collection : Geometry; index : Positive := 1)
                                  return Geometric_Line_String is
   begin
      check_collection_index (collection, index);
      case collection.contents is
         when single_line_string => return collection.line_string;
         when multi_line_string  =>
            declare
               sub_index : Positive;
               data_size : Positive;
               set_index_set : Boolean := False;
            begin
               for x in collection.set_line_strings'Range loop
                  if collection.set_line_strings (x).shape_id = index then
                     if not set_index_set then
                        sub_index := x;
                        set_index_set := True;
                        data_size := 1;
                     else
                        data_size := data_size + 1;
                     end if;
                  end if;
               end loop;
               if not set_index_set then
                  raise CONVERSION_FAILED
                    with "data corrupt: expected index" & index'Img &
                    " not found, but" & collection.units'Img;
               end if;
               if data_size >= 2 then
                  declare
                     LNS : Geometric_Line_String (1 .. data_size);
                  begin
                     for x in Positive range 1 .. data_size loop
                        LNS (x) := collection.set_line_strings
                          (sub_index + x - 1).point;
                     end loop;
                     return LNS;
                  end;
               else
                  raise CONVERSION_FAILED
                    with "Data type at set_line_strings index" & index'Img &
                    "is not at least 2 points long";
               end if;
            end;
         when heterogeneous =>
            raise CONVERSION_FAILED
              with "Requested line_string from mixed collection. " &
              "(Extract using retrieve_subcollection instead)";
         when others =>
            raise CONVERSION_FAILED
              with "Requested line_string, but shape is " &
              collection_item_shape (collection, index)'Img;
      end case;
   end retrieve_line_string;


   ----------------------
   --  retrieve_circle  --
   ----------------------
   function retrieve_circle (collection : Geometry) return Geometric_Circle is
   begin
      case collection.contents is
         when single_circle  => return collection.circle;
         when others =>
            raise CONVERSION_FAILED
              with "Requested circle, but shape is " &
              collection_item_shape (collection, 1)'Img;
      end case;
   end retrieve_circle;


   ------------------------
   --  retrieve_polygon  --
   ------------------------
   function retrieve_polygon (collection : Geometry; index : Positive := 1)
                              return Geometric_Polygon
   is
      position  : Positive;
      data_size : Positive;
   begin
      check_collection_index (collection, index);
      case collection.contents is
         when single_polygon => return collection.polygon;
         when multi_polygon  =>
            position  := outer_polygon_position (collection, index);
            data_size := polygon_ring_size (collection, position);
            declare
               LNS : Geometric_Polygon (1 .. data_size);
            begin
               for x in Positive range 1 .. data_size loop
                  LNS (x) := collection.set_polygons (position + x - 1).point;
               end loop;
               return LNS;
            end;
         when heterogeneous =>
            raise CONVERSION_FAILED
              with "Requested polygon from mixed collection. " &
              "(Extract using retrieve_subcollection instead)";
         when others =>
            raise CONVERSION_FAILED
              with "Requested polygon, but shape is " &
              collection_item_shape (collection, index)'Img;
      end case;
   end retrieve_polygon;


   -----------------------------
   --  retrieve_full_polygon  --
   -----------------------------
   function retrieve_full_polygon (collection : Geometry;
                                   index : Positive := 1)
                                   return Heterogeneous_Collection
   is
      position : Positive;
      shape_id : Positive;
      endpoint : Positive := 1;
   begin
      check_collection_index (collection, index);
      case collection.contents is
         when single_polygon =>
            declare
               LL : Natural := collection.polygon'Length;
               HC : Heterogeneous_Collection (1 .. LL);
            begin
               for x in collection.polygon'Range loop
                  HC (x).shape     := polygon_shape;
                  HC (x).point     := collection.polygon (x);
                  HC (x).shape_id  := 1;
                  HC (x).component := 1;
               end loop;
               return HC;
            end;
         when multi_polygon  =>
            position := outer_polygon_position (collection, index);
            shape_id := collection.set_polygons (position).shape_id;
            for x in Positive range position .. collection.set_polygons'Last
            loop
               exit when collection.set_polygons (x).shape_id /= shape_id;
               endpoint := x;
            end loop;
            return collection.set_polygons (position .. endpoint);
         when heterogeneous =>
            raise CONVERSION_FAILED
              with "Requested full polygon from mixed collection. " &
              "(Extract using retrieve_subcollection instead)";
         when others =>
            raise CONVERSION_FAILED
              with "Requested polygon, but shape is " &
              collection_item_shape (collection, index)'Img;
      end case;
   end retrieve_full_polygon;


   ---------------------
   --  retrieve_hole  --
   ---------------------
   function retrieve_hole (collection : Geometry; index : Positive := 1;
                           hole_index : Positive) return Geometric_Polygon
   is
      position  : Positive;
      data_size : Positive;
   begin
      check_collection_index (collection, index);
      case collection.contents is
         when multi_polygon  =>
            position  := inner_polygon_position (collection,
                                                 index, hole_index);
            data_size := polygon_ring_size (collection, position);
            declare
               LNS : Geometric_Polygon (1 .. data_size);
            begin
               for x in Positive range 1 .. data_size loop
                  LNS (x) := collection.set_polygons (position + x - 1).point;
               end loop;
               return LNS;
            end;
         when heterogeneous =>
            raise CONVERSION_FAILED
              with "Requested polygon hole from mixed collection. " &
              "(Extract using retrieve_subcollection instead)";
         when others =>
            raise CONVERSION_FAILED with "Requested polygon hole#" &
              hole_index'Img & ", but shape is " &
              collection_item_shape (collection, index)'Img;
      end case;
   end retrieve_hole;


   --------------------------------------------
   --  retrieve_two_points_of_infinite_line  --
   --------------------------------------------
   function retrieve_two_points_of_infinite_line (collection : Geometry)
                                                  return Geometric_Line is
   begin
      case collection.contents is
         when single_infinite_line => return collection.infinite_line;
         when others =>
            raise CONVERSION_FAILED
              with "Requested infinite_line, but shape is " &
              collection_item_shape (collection, 1)'Img;
      end case;
   end retrieve_two_points_of_infinite_line;


   --------------------------------
   --  convert_infinite_line #1  --
   --------------------------------
   function convert_infinite_line (line : Geometric_Line)
                                   return Slope_Intercept
   is
      diff_x    : constant Geometric_Real := line (2).X - line (1).X;
      diff_y    : constant Geometric_Real := line (2).Y - line (1).Y;
      slope     : Geometric_Real;
      intercept : Geometric_Real;
   begin
      if diff_x = 0.0 then
         return (slope => 0.0, y_intercept => 0.0, vertical => True);
      end if;

      slope := diff_y / diff_x;
      intercept := line (1).Y - (slope * line (1).X);
      return (slope, intercept, False);
   end convert_infinite_line;


   --------------------------------
   --  convert_infinite_line #2  --
   --------------------------------
   function convert_infinite_line (line : Geometric_Line) return Standard_Form
   is
      --  If vertical slope ("run" = 0, "rise" /= 0) the result is
      --      A=1 B=0 C=x-coordinate
      --  For the non-vertical case
      --  A is equivalent to negative slope
      --  B is equivalent to 1.0
      --  C is equivalent to y-intercept
      SLINT : Slope_Intercept := convert_infinite_line (line);
   begin
      if SLINT.vertical then
         return (A => 1.0, B => 0.0, C => line (1).X);
      end if;
      return (A => -1.0 * SLINT.slope, B => 1.0, C => SLINT.y_intercept);
   end convert_infinite_line;


   -----------------------------------
   --  convert_to_infinite_line #1  --
   -----------------------------------
   function convert_to_infinite_line (std_form : Standard_Form)
                                      return Geometric_Line
   is
      XX : Geometric_Real;
      YY : Geometric_Real;
   begin
      if std_form.B = 0.0 then
         if std_form.A = 0.0 then
            raise CONVERSION_FAILED
              with "Illegal standard form: A and B are both zero";
         end if;
         --  Vertical line
         XX := std_form.C / std_form.A;
         return ((XX, 0.0), (XX, 1.0));
      end if;

      if std_form.A = 0.0 then
         --  Horizontal line
         YY := std_form.C / std_form.B;
         return ((0.0, YY), (1.0, YY));
      end if;

      --  Sloped (non-inclusively been +/- 0 and infinity)
      --  In other words, neither A nor B is zero; both axes are crossed
      XX := std_form.C / std_form.A;
      YY := std_form.C / std_form.B;
      return ((0.0, YY), (XX, 0.0));

   end convert_to_infinite_line;


   -----------------------------------
   --  convert_to_infinite_line #2  --
   -----------------------------------
   function convert_to_infinite_line (intercept_form : Slope_Intercept)
                                      return Geometric_Line
   is
      XX : Geometric_Real;
      YY : Geometric_Real;
   begin
      if intercept_form.vertical then
         raise CONVERSION_FAILED
           with "Cannot convert vertical lines using the intercept form";
      end if;
      YY := intercept_form.y_intercept;

      --  Handle horizontal case
      if intercept_form.slope = 0.0 then
         return ((0.0, YY), (1.0, YY));
      end if;

      --  Remaining cases cross both axes
      XX := -1.0 * intercept_form.y_intercept / intercept_form.slope;
      return ((0.0, YY), (XX, 0.0));
   end convert_to_infinite_line;


   -------------------
   --  format_real  --
   -------------------
   function format_real (value : Geometric_Real) return String
   is
      function trim_sides (S : String) return String;
      raw    : constant String := CT.trim (Geometric_Real'Image (value));
      last3  : constant String := raw (raw'Last - 2 .. raw'Last);
      posend : constant Natural := raw'Last - 4;
      shift  : constant Integer := Integer'Value (last3);
      canvas : String (1 .. 26) := (others => '0');
      dot    : Natural;

      function trim_sides (S : String) return String
      is
         left  : Natural := S'First;
         right : Natural := S'Last;
         keep  : Boolean;
      begin
         for x in S'Range loop
            keep := (S (x) /= '0' and then S (x) /= ' ');
            exit when keep;
            left := left + 1;
         end loop;
         for x in reverse S'Range loop
            keep := (S (x) /= '0' and then S (x) /= ' ');
            exit when keep;
            right := right - 1;
         end loop;
         if S (left) = '.' then
            left := left - 1;
         end if;
         if S (right) = '.' then
            right := right - 1;
         end if;
         if S (left .. left + 1) = "-." then
            return "-0" & S (left + 1 .. right);
         else
            return S (left .. right);
         end if;
      end trim_sides;
   begin
      if shift = 0 then
         canvas (1 .. posend) := raw (1 .. posend);
         return trim_sides (canvas (1 .. posend));
      elsif shift > 18 or else shift < -18 then
         return CT.trim (raw);
      elsif shift > 0 then
         canvas (1 .. posend) := raw (1 .. posend);
         dot := CT.pinpoint (canvas, ".");
         for bubble in Positive range dot + 1 .. dot + shift loop
            --  Left side is always the dot
            canvas (bubble - 1) := canvas (bubble);
            canvas (bubble) := '.';
         end loop;
         return trim_sides (canvas);
      else
         canvas (canvas'Last - posend + 1 .. canvas'Last) := raw (1 .. posend);
         dot := CT.pinpoint (canvas, ".");
         for bubble in reverse dot + shift .. dot - 1 loop
            --  Right side is always the dot
            canvas (bubble + 1) := canvas (bubble);
            canvas (bubble) := '.';
         end loop;
         return trim_sides (canvas);
      end if;
   end format_real;


   ------------------
   --  mysql_text  --
   ------------------
   function mysql_text (collection : Geometry) return String is
      function format_point (pt    : Geometric_Point;
                             first : Boolean := False) return String;
      function format_polygon (poly  : Heterogeneous_Collection;
                               first : Boolean := False) return String;
      function format_line_string (LNS   : Geometric_Line_String;
                                   first : Boolean := False) return String;

      sep    : constant String := ", ";
      pclose : constant String := ")";

      function format_point (pt    : Geometric_Point;
                             first : Boolean := False) return String
      is
         ptx  : constant String := format_real (pt.X);
         pty  : constant String := format_real (pt.Y);
         core : constant String := "Point(" & ptx & sep & pty & pclose;
      begin
         if first then
            return core;
         else
            return sep & core;
         end if;
      end format_point;

      function format_polygon (poly  : Heterogeneous_Collection;
                               first : Boolean := False) return String
      is
         lead   : constant String := "Polygon(";
         work   : CT.Text;
         inner1 : Boolean;
         lastsc : Natural := 0;
      begin
         if first then
            CT.SU.Append (work, lead);
         else
            CT.SU.Append (work, sep & lead);
         end if;
         for x in poly'Range loop
            inner1 := (poly (x).component /= lastsc);
            lastsc := poly (x).component;
            if inner1 then
               if x /= poly'First then
                  CT.SU.Append (work, pclose & sep);
               end if;
               CT.SU.Append (work, "Linestring(");
            end if;
            CT.SU.Append (work, format_point (poly (x).point, inner1));
            if x = poly'Last then
               CT.SU.Append (work, pclose);
            end if;
         end loop;
         CT.SU.Append (work, pclose);
         return CT.USS (work);
      end format_polygon;

      function format_line_string (LNS : Geometric_Line_String;
                                   first : Boolean := False) return String
      is
         lead : constant String := "LineString(";
         work : CT.Text := CT.SUS (lead);
         inn1 : Boolean;
      begin
         for x in LNS'Range loop
            inn1 := (x = LNS'First);
                     CT.SU.Append (work, format_point (LNS (x), inn1));
         end loop;
         if first then
            return CT.USS (work) & pclose;
         else
            return sep & CT.USS (work) & pclose;
         end if;
      end format_line_string;

      classification : Collection_Type := collection.contents;
   begin
      case classification is
         when unset        => return "";
         when single_point => return format_point (collection.point, True);
         when single_line_string =>
            return format_line_string (collection.line_string, True);
         when single_infinite_line =>
            --  Infinite lines are not supported by MySQL so this is not
            --  actually correct
            return format_line_string (collection.infinite_line, True);
         when single_circle =>
            --  Circles are unique to postgresql, so this is nonsense for
            --  MySQL.  It's better than exception though, I guess.
            return "Circle(" &
              format_point (collection.circle.center_point, True) & sep &
              format_real (collection.circle.radius) & pclose;
         when single_polygon =>
            return format_polygon (retrieve_full_polygon (collection, 1),
                                   True);
         when multi_point =>
            declare
               product : CT.Text := CT.SUS ("MultiPoint(");
               first   : Boolean;
            begin
               for x in collection.set_points'Range loop
                  first := (x = collection.set_points'First);
                  CT.SU.Append
                    (product, format_point
                       (collection.set_points (x), first));
               end loop;
               return CT.USS (product) & pclose;
            end;
         when multi_line_string =>
            declare
               product : CT.Text := CT.SUS ("MultiLineString(");
               first   : Boolean;
            begin
               for ls in 1 .. collection.units loop
                  first := (ls = 1);
                  CT.SU.Append
                    (product, format_line_string
                       (retrieve_line_string (collection, ls), first));
               end loop;
               return CT.USS (product) & pclose;
            end;
         when multi_polygon =>
            declare
               product : CT.Text := CT.SUS ("MultiPolygon(");
               first   : Boolean;
            begin
               if collection.units = 1 then
                  product := CT.blank;
               end if;
               for ls in 1 .. collection.units loop
                  first := (ls = 1);
                  CT.SU.Append
                    (product, format_polygon
                       (retrieve_full_polygon (collection, ls), first));
               end loop;
               if collection.units > 1 then
                  CT.SU.Append (product, pclose);
               end if;
               return CT.USS (product);
            end;
         when heterogeneous =>
            declare
               product : CT.Text := CT.SUS ("GeometryCollection(");
               first   : Boolean;
               flavor  : Geometric_Shape;
            begin
               for ls in 1 .. collection.units loop
                  first := (ls = 1);
                  flavor := collection_item_shape (collection, ls);
                  case flavor is
                     when point_shape =>
                        CT.SU.Append
                          (product, format_point
                             (retrieve_point (collection, ls), first));
                     when line_string_shape =>
                        CT.SU.Append
                          (product, format_line_string
                             (retrieve_line_string (collection, ls), first));
                     when polygon_shape =>
                        CT.SU.Append
                          (product, format_polygon
                             (retrieve_full_polygon (collection, ls), first));
                     when circle_shape        => null;
                     when infinite_line_shape => null;
                     when mixture =>
                        raise CONVERSION_FAILED with "TO BE IMPLEMENTED";
                  end case;
               end loop;
               return CT.USS (product) & pclose;
            end;
      end case;
   end mysql_text;


   -----------------------
   --  Well_Known_Text  --
   -----------------------
   function Well_Known_Text (collection : Geometry;
                             top_first  : Boolean := True) return String
   is
      function format_point (pt    : Geometric_Point;
                             first : Boolean := False;
                             label : Boolean := False) return String;
      function format_polygon (poly  : Heterogeneous_Collection;
                               first : Boolean := False;
                               label : Boolean := False) return String;
      function format_line_string (LNS   : Geometric_Line_String;
                                   first : Boolean := False;
                                   label : Boolean := False) return String;

      sep    : constant String := ",";
      popen  : constant String := "(";
      pclose : constant String := ")";

      function format_point (pt    : Geometric_Point;
                             first : Boolean := False;
                             label : Boolean := False) return String
      is
         ptx    : constant String := format_real (pt.X);
         pty    : constant String := format_real (pt.Y);
         lead   : constant String := "POINT";
         core   : constant String := ptx & " " & pty;
      begin
         if label then
            if first then
               return lead & popen & core & pclose;
            else
               return sep & lead & popen & core & pclose;
            end if;
         else
            if first then
               return core;
            else
               return sep & core;
            end if;
         end if;
      end format_point;

      function format_polygon (poly  : Heterogeneous_Collection;
                               first : Boolean := False;
                               label : Boolean := False) return String
      is
         lead   : constant String := "POLYGON";
         work   : CT.Text;
         inner1 : Boolean;
         lastsc : Natural := 0;
      begin
         if label then
            if first then
               CT.SU.Append (work, lead & popen);
            else
               CT.SU.Append (work, sep & lead & popen);
            end if;
         else
            if first then
               CT.SU.Append (work, popen);
            else
               CT.SU.Append (work, sep & popen);
            end if;
         end if;
         for x in poly'Range loop
            inner1 := (poly (x).component /= lastsc);
            lastsc := poly (x).component;
            if inner1 then
               if x /= poly'First then
                  CT.SU.Append (work, pclose & sep);
               end if;
               CT.SU.Append (work, popen);
            end if;
            CT.SU.Append (work, format_point (poly (x).point, inner1));
            if x = poly'Last then
               CT.SU.Append (work, pclose);
            end if;
         end loop;
         CT.SU.Append (work, pclose);
         return CT.USS (work);
      end format_polygon;

      function format_line_string (LNS : Geometric_Line_String;
                                   first : Boolean := False;
                                   label : Boolean := False) return String
      is
         lead   : constant String := "LINESTRING";
         work   : CT.Text := CT.blank;
         inner1 : Boolean;
      begin
         if label then
            if first then
               CT.SU.Append (work, lead & popen);
            else
               CT.SU.Append (work, sep & lead & popen);
            end if;
         else
            if first then
               CT.SU.Append (work, popen);
            else
               CT.SU.Append (work, sep & popen);
            end if;
         end if;

         for x in LNS'Range loop
            inner1 := (x = LNS'First);
                     CT.SU.Append (work, format_point (LNS (x), inner1));
         end loop;
         CT.SU.Append (work, pclose);
         return CT.USS (work);
      end format_line_string;

      classification : Collection_Type := collection.contents;
   begin
      case classification is
         when unset =>
            return "";
         when single_point =>
            return format_point (collection.point, top_first, True);
         when single_line_string =>
            return format_line_string (collection.line_string, top_first, True);
         when single_infinite_line =>
            --  Infinite lines are not supported by WKT so this is wrong
            return format_line_string (collection.infinite_line, top_first, True);
         when single_circle =>
            --  No circles in WKT, so using this output will result in error
            return "CIRCLE (" &
              format_point (collection.circle.center_point, top_first) & sep &
              format_real (collection.circle.radius) & pclose;
         when single_polygon =>
            return format_polygon (retrieve_full_polygon (collection, 1),
                                   top_first, True);
         when multi_point =>
            declare
               product : CT.Text := CT.SUS ("MULTIPOINT(");
               first   : Boolean;
            begin
               for x in collection.set_points'Range loop
                  first := (x = collection.set_points'First);
                  CT.SU.Append
                    (product, format_point
                       (collection.set_points (x), first));
               end loop;
               return CT.USS (product) & pclose;
            end;
         when multi_line_string =>
            declare
               product : CT.Text := CT.SUS ("MULTILINESTRING(");
               first   : Boolean;
            begin
               for ls in 1 .. collection.units loop
                  first := (ls = 1);
                  CT.SU.Append
                    (product, format_line_string
                       (retrieve_line_string (collection, ls), first));
               end loop;
               return CT.USS (product) & pclose;
            end;
         when multi_polygon =>
            declare
               product : CT.Text := CT.SUS ("MULTIPOLYGON(");
               first   : Boolean;
            begin
               if collection.units = 1 then
                  product := CT.SUS ("POLYGON");
               end if;
               for ls in 1 .. collection.units loop
                  first := (ls = 1);
                  CT.SU.Append
                    (product, format_polygon
                       (retrieve_full_polygon (collection, ls), first));
               end loop;
               if collection.units > 1 then
                  CT.SU.Append (product, pclose);
               end if;
               return CT.USS (product);
            end;
         when heterogeneous =>
            declare
               product : CT.Text := CT.SUS ("GEOMETRYCOLLECTION(");
               first   : Boolean := True;
               GM      : Geometry;
            begin
               for ls in 1 .. collection.units loop
                  GM := retrieve_subcollection (collection, ls);
                  CT.SU.Append (product, Well_Known_Text (GM, first));
                  first := False;
               end loop;
               return CT.USS (product) & pclose;
            end;
      end case;

   end Well_Known_Text;


end Spatial_Data;
