--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body Spatial_Data is


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


   --------------------
   --  append_point  --
   --------------------
   procedure append_point (collection : out Geometry; point : Geometric_Point)
   is
      classification : Collection_Type := collection.contents;
      product     : Geometry := initialize_as_point ((0.0, 0.0));
      num_items   : Natural := collection.items + 1;
      point_count : Natural;
   begin
      case classification is
         when single_circle =>
            raise CONVERSION_FAILED
              with "circles cannot be part of a geometric collection";
         when single_point =>
            product := (multi_point, num_items, (collection.point, point));
         when single_line =>
            product := (heterogeneous, num_items,
                        ((collection.line (1), line_shape, 1),
                         (collection.line (2), line_shape, 1),
                         (point, point_shape, 2)));
         when single_infinite_line =>
            product := (heterogeneous, num_items,
                        ((collection.infinite_line (1), infinite_line_shape, 1),
                         (collection.infinite_line (2), infinite_line_shape, 1),
                         (point, point_shape, 2)));
         when single_line_string =>
            point_count := collection.line_string'Length + 1;
            declare
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.line_string'Range loop
                  HC (x).point    := collection.line (x);
                  HC (x).shape    := line_string_shape;
                  HC (x).shape_id := 1;
               end loop;
               HC (HC'Last) := (point, point_shape, 2);
               product := (heterogeneous, num_items, HC);
            end;
         when single_polygon =>
            point_count := collection.polygon'Length + 1;
            declare
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.polygon'Range loop
                  HC (x).point    := collection.line (x);
                  HC (x).shape    := polygon_shape;
                  HC (x).shape_id := 1;
               end loop;
               HC (HC'Last) := (point, point_shape, 2);
               product := (heterogeneous, num_items, HC);
            end;
         when multi_point =>
            declare
               HC : Geometric_Point_Collection (1 .. num_items);
            begin
               HC (1 .. collection.set_points'Last) := collection.set_points;
               HC (HC'Last) := (point);
               product := (multi_point, num_items, HC);
            end;
         when multi_line_string =>
            point_count := collection.set_line_strings'Length + 1;
            declare
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.set_line_strings'Range loop
                  HC (x).shape_id := collection.set_line_strings (x).shape_id;
                  HC (x).point    := collection.set_line_strings (x).point;
                  HC (x).shape    := line_string_shape;
               end loop;
               HC (HC'Last) := (point, point_shape, num_items);
               product := (heterogeneous, num_items, HC);
            end;
         when multi_polygon =>
            point_count := collection.set_polygons'Length + 1;
            declare
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.set_polygons'Range loop
                  HC (x).shape_id := collection.set_polygons (x).shape_id;
                  HC (x).point    := collection.set_polygons (x).point;
                  HC (x).shape    := polygon_shape;
               end loop;
               HC (HC'Last) := (point, point_shape, num_items);
               product := (heterogeneous, num_items, HC);
            end;
         when heterogeneous =>
            point_count := collection.set_polygons'Length + 1;
            declare
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               HC (1 .. collection.set_heterogeneous'Last) :=
                 collection.set_heterogeneous;
               HC (HC'Last) := (point, point_shape, num_items);
               product := (heterogeneous, num_items, HC);
            end;
      end case;
      collection := product;
   end append_point;


   -------------------
   --  append_line  --
   -------------------
   procedure append_line (collection : out Geometry; line : Geometric_Line)
   is
      LS : Geometric_Line_String (1 .. 2) := (line (1), line (2));
   begin
      collection.append_line_string (LS);
   end append_line;


   --------------------------
   --  append_line_string  --
   --------------------------
   procedure append_line_string (collection : out Geometry;
                                 line_string : Geometric_Line_String)
   is
      classification : Collection_Type := collection.contents;
      product     : Geometry := initialize_as_point ((0.0, 0.0));
      num_items   : Natural := collection.items + 1;
      point_count : Natural;
   begin
      case classification is
         when single_circle =>
            raise CONVERSION_FAILED
              with "circles cannot be part of a geometric collection";
         when single_point =>
            point_count := 1 + line_string'Length;
            declare
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               HC (1).shape_id := 1;
               HC (1).shape    := point_shape;
               HC (1).point    := collection.point;
               for x in line_string'Range loop
                  HC (x + 1).shape_id := num_items;
                  HC (x + 1).shape    := line_string_shape;
                  HC (x + 1).point    := line_string (x);
               end loop;
               product := (heterogeneous, num_items, HC);
            end;
         when single_line =>
            point_count := collection.line'Length + line_string'Length;
            declare
               LL : Natural := collection.line'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.line'Range loop
                  HC (x).shape_id := 1;
                  HC (x).shape    := line_shape;
                  HC (x).point    := collection.line (x);
               end loop;
               for x in line_string'Range loop
                  HC (x + LL).shape_id := num_items;
                  HC (x + LL).shape    := line_string_shape;
                  HC (x + LL).point    := line_string (x);
               end loop;
               product := (heterogeneous, num_items, HC);
            end;
         when single_infinite_line =>
            point_count := collection.infinite_line'Length +
                           line_string'Length;
            declare
               LL : Natural := collection.infinite_line'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.infinite_line'Range loop
                  HC (x).shape_id := 1;
                  HC (x).shape    := infinite_line_shape;
                  HC (x).point    := collection.infinite_line (x);
               end loop;
               for x in line_string'Range loop
                  HC (x + LL).shape_id := num_items;
                  HC (x + LL).shape    := line_string_shape;
                  HC (x + LL).point    := line_string (x);
               end loop;
               product := (heterogeneous, num_items, HC);
            end;
         when single_line_string =>
            point_count := collection.line_string'Length + line_string'Length;
            declare
               LL : Natural := collection.line_string'Length;
               HC : Homogeneous_Collection (1 .. point_count);
            begin
               for x in collection.line_string'Range loop
                  HC (x).shape_id := 1;
                  HC (x).point    := collection.line_string (x);
               end loop;
               for x in line_string'Range loop
                  HC (x + LL).shape_id := num_items;
                  HC (x + LL).point    := line_string (x);
               end loop;
               product := (multi_line_string, num_items, HC);
            end;
         when single_polygon =>
            point_count := collection.polygon'Length + line_string'Length;
            declare
               LL : Natural := collection.polygon'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.polygon'Range loop
                  HC (x).shape_id := 1;
                  HC (x).shape    := polygon_shape;
                  HC (x).point    := collection.polygon (x);
               end loop;
               for x in line_string'Range loop
                  HC (x + LL).shape_id := num_items;
                  HC (x + LL).shape    := line_string_shape;
                  HC (x + LL).point    := line_string (x);
               end loop;
               product := (heterogeneous, num_items, HC);
            end;
         when multi_point =>
            point_count := collection.set_points'Length + line_string'Length;
            declare
               LL : Natural := collection.set_points'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.set_points'Range loop
                  HC (x).shape_id := x;
                  HC (x).shape    := point_shape;
                  HC (x).point    := collection.set_points (x);
               end loop;
               for x in line_string'Range loop
                  HC (x + LL).shape_id := num_items;
                  HC (x + LL).shape    := line_string_shape;
                  HC (x + LL).point    := line_string (x);
               end loop;
               product := (heterogeneous, num_items, HC);
            end;
         when multi_line_string =>
            point_count := collection.set_line_strings'Length +
                           line_string'Length;
            declare
               LL : Natural := collection.set_line_strings'Length;
               HC : Homogeneous_Collection (1 .. point_count);
            begin
               HC (collection.set_points'Range) := collection.set_line_strings;
               for x in line_string'Range loop
                  HC (x + LL).shape_id := num_items;
                  HC (x + LL).point    := line_string (x);
               end loop;
               product := (multi_line_string, num_items, HC);
            end;
         when multi_polygon =>
            point_count := collection.set_polygons'Length + line_string'Length;
            declare
               LL : Natural := collection.set_polygons'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.set_polygons'Range loop
                  HC (x).shape    := polygon_shape;
                  HC (x).shape_id := collection.set_polygons (x).shape_id;
                  HC (x).point    := collection.set_polygons (x).point;
               end loop;
               for x in line_string'Range loop
                  HC (x + LL).shape_id := num_items;
                  HC (x + LL).shape    := line_string_shape;
                  HC (x + LL).point    := line_string (x);
               end loop;
               product := (heterogeneous, num_items, HC);
            end;
         when heterogeneous =>
            point_count := collection.set_heterogeneous'Length +
                           line_string'Length;
            declare
               LL : Natural := collection.set_heterogeneous'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               HC (collection.set_heterogeneous'Range) :=
                 collection.set_heterogeneous;
               for x in line_string'Range loop
                  HC (x + LL).shape_id := num_items;
                  HC (x + LL).shape    := line_string_shape;
                  HC (x + LL).point    := line_string (x);
               end loop;
               product := (heterogeneous, num_items, HC);
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
      num_items   : Natural := collection.items + 1;
      point_count : Natural;
   begin
      case classification is
         when single_circle =>
            raise CONVERSION_FAILED
              with "circles cannot be part of a geometric collection";
         when single_point =>
            point_count := 1 + polygon'Length;
            declare
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               HC (1).shape_id := 1;
               HC (1).shape    := point_shape;
               HC (1).point    := collection.point;
               for x in polygon'Range loop
                  HC (x + 1).shape_id := num_items;
                  HC (x + 1).shape    := polygon_shape;
                  HC (x + 1).point    := polygon (x);
               end loop;
               product := (heterogeneous, num_items, HC);
            end;
         when single_line =>
            point_count := collection.line'Length + polygon'Length;
            declare
               LL : Natural := collection.line'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.line'Range loop
                  HC (x).shape_id := 1;
                  HC (x).shape    := line_shape;
                  HC (x).point    := collection.line (x);
               end loop;
               for x in polygon'Range loop
                  HC (x + LL).shape_id := num_items;
                  HC (x + LL).shape    := polygon_shape;
                  HC (x + LL).point    := polygon (x);
               end loop;
               product := (heterogeneous, num_items, HC);
            end;
         when single_infinite_line =>
            point_count := collection.infinite_line'Length + polygon'Length;
            declare
               LL : Natural := collection.infinite_line'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.infinite_line'Range loop
                  HC (x).shape_id := 1;
                  HC (x).shape    := infinite_line_shape;
                  HC (x).point    := collection.infinite_line (x);
               end loop;
               for x in polygon'Range loop
                  HC (x + LL).shape_id := num_items;
                  HC (x + LL).shape    := polygon_shape;
                  HC (x + LL).point    := polygon (x);
               end loop;
               product := (heterogeneous, num_items, HC);
            end;
         when single_line_string =>
            point_count := collection.polygon'Length + polygon'Length;
            declare
               LL : Natural := collection.polygon'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.polygon'Range loop
                  HC (x).shape_id := 1;
                  HC (x).shape    := line_string_shape;
                  HC (x).point    := collection.polygon (x);
               end loop;
               for x in polygon'Range loop
                  HC (x + LL).shape    := polygon_shape;
                  HC (x + LL).shape_id := num_items;
                  HC (x + LL).point    := polygon (x);
               end loop;
               product := (heterogeneous, num_items, HC);
            end;
         when single_polygon =>
            point_count := collection.polygon'Length + polygon'Length;
            declare
               LL : Natural := collection.polygon'Length;
               HC : Homogeneous_Collection (1 .. point_count);
            begin
               for x in collection.polygon'Range loop
                  HC (x).shape_id := 1;
                  HC (x).point    := collection.polygon (x);
               end loop;
               for x in polygon'Range loop
                  HC (x + LL).shape_id := num_items;
                  HC (x + LL).point    := polygon (x);
               end loop;
               product := (multi_polygon, num_items, HC);
            end;
         when multi_point =>
            point_count := collection.set_points'Length + polygon'Length;
            declare
               LL : Natural := collection.set_points'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.set_points'Range loop
                  HC (x).shape_id := x;
                  HC (x).shape    := point_shape;
                  HC (x).point    := collection.set_points (x);
               end loop;
               for x in polygon'Range loop
                  HC (x + LL).shape_id := num_items;
                  HC (x + LL).shape    := polygon_shape;
                  HC (x + LL).point    := polygon (x);
               end loop;
               product := (heterogeneous, num_items, HC);
            end;
         when multi_line_string =>
            point_count := collection.set_line_strings'Length +
                           polygon'Length;
            declare
               LL : Natural := collection.set_line_strings'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               for x in collection.set_line_strings'Range loop
                  HC (x).shape    := line_string_shape;
                  HC (x).shape_id := collection.set_line_strings (x).shape_id;
                  HC (x).point    := collection.set_line_strings (x).point;
               end loop;
               for x in polygon'Range loop
                  HC (x + LL).shape_id := num_items;
                  HC (x + LL).point    := polygon (x);
               end loop;
               product := (heterogeneous, num_items, HC);
            end;
         when multi_polygon =>
            point_count := collection.set_polygons'Length + polygon'Length;
            declare
               LL : Natural := collection.set_polygons'Length;
               HC : Homogeneous_Collection (1 .. point_count);
            begin
               HC (collection.set_polygons'Range) := collection.set_polygons;
               for x in polygon'Range loop
                  HC (x + LL).shape_id := num_items;
                  HC (x + LL).point    := polygon (x);
               end loop;
               product := (multi_polygon, num_items, HC);
            end;
         when heterogeneous =>
            point_count := collection.set_heterogeneous'Length +
                           polygon'Length;
            declare
               LL : Natural := collection.set_heterogeneous'Length;
               HC : Heterogeneous_Collection (1 .. point_count);
            begin
               HC (collection.set_heterogeneous'Range) :=
                 collection.set_heterogeneous;
               for x in polygon'Range loop
                  HC (x + LL).shape_id := num_items;
                  HC (x + LL).shape    := polygon_shape;
                  HC (x + LL).point    := polygon (x);
               end loop;
               product := (heterogeneous, num_items, HC);
            end;
      end case;
      collection := product;
   end append_polygon;


   --------------------------
   --  size_of_collection  --
   --------------------------
   function size_of_collection (collection : Geometry) return Positive is
   begin
      return collection.items;
   end size_of_collection;

end Spatial_Data;
