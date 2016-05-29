--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with CommonText;

package body Spatial_Data is

   package CT renames CommonText;

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
      if polygon'Length < 3 then
         raise LACKING_POINTS
           with "polygons must have at least 3 points (found only" &
           polygon'Length'Img & ")";
      end if;
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
      if polygon'Length < 3 then
         raise LACKING_POINTS
           with "polygons must have at least 3 points (found only" &
           polygon'Length'Img & ")";
      end if;
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


   ------------------------------
   --  check_collection_index  --
   ------------------------------
   procedure check_collection_index (collection : Geometry; index : Positive)
   is
   begin
      if index > collection.items then
         raise OUT_OF_COLLECTION_RANGE
           with "Only" & collection.items'Img & " items in collection " &
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
      for x in collection.set_heterogeneous'Range loop
         if collection.set_heterogeneous (x).shape_id = index then
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
           " not found, but" & collection.items'Img & " items are present";
      end if;
   end locate_heterogenous_item;


   -----------------------------
   --  collection_item_shape  --
   -----------------------------
   function collection_item_shape (collection : Geometry;
                                   index : Positive := 1)
                                   return Geometric_Shape
   is
   begin
      collection.check_collection_index (index);
      case collection.contents is
         when single_point         => return point_shape;
         when single_line          => return line_shape;
         when single_line_string   => return line_string_shape;
         when single_infinite_line => return infinite_line_shape;
         when single_circle        => return circle_shape;
         when single_polygon       => return polygon_shape;
         when multi_point          => return point_shape;
         when multi_line_string    => return line_string_shape;
         when multi_polygon        => return polygon_shape;
         when heterogeneous        =>
            for x in collection.set_heterogeneous'Range loop
               if collection.set_heterogeneous (x).shape_id = index then
                  return collection.set_heterogeneous (x).shape;
               end if;
            end loop;
            raise CONVERSION_FAILED
              with "Impossible! Collection size=" & collection.items'Img &
              " index=" & index'Img & " but heterogeneous shape not found";
      end case;
   end collection_item_shape;


   ----------------------
   --  retrieve_point  --
   ----------------------
   function retrieve_point (collection : Geometry; index : Positive := 1)
                             return Geometric_Point is
   begin
      collection.check_collection_index (index);
      case collection.contents is
         when single_point  => return collection.point;
         when multi_point   => return collection.set_points (index);
         when heterogeneous =>
            declare
               sub_index : Positive;
               data_size : Positive;
            begin
               collection.locate_heterogenous_item (index      => index,
                                                    set_index  => sub_index,
                                                    num_points => data_size);
               if collection.set_heterogeneous (sub_index).shape =
                 point_shape and then data_size > 1
               then
                  return collection.set_heterogeneous (sub_index).point;
               else
                  raise CONVERSION_FAILED
                    with "Data type at heterogenous index" & index'Img &
                    "is not a point or is not 1-component group";
               end if;
            end;
         when others =>
            raise CONVERSION_FAILED
              with "Requested point, but shape is " &
              collection.collection_item_shape (index)'Img;
      end case;
   end retrieve_point;


   ---------------------
   --  retrieve_line  --
   ---------------------
   function retrieve_line (collection : Geometry; index : Positive := 1)
                           return Geometric_Line is
   begin
      collection.check_collection_index (index);
      case collection.contents is
         when single_line => return collection.line;
         when heterogeneous =>
            declare
               sub_index : Positive;
               data_size : Positive;
               LN        : Geometric_Line;
            begin
               collection.locate_heterogenous_item (index      => index,
                                                    set_index  => sub_index,
                                                    num_points => data_size);
               if collection.set_heterogeneous (sub_index).shape = line_shape
                 and then data_size = 2
               then
                  LN (1) := collection.set_heterogeneous (sub_index).point;
                  LN (2) := collection.set_heterogeneous (sub_index + 1).point;
                  return LN;
               else
                  raise CONVERSION_FAILED
                    with "Data type at heterogeneous index" & index'Img &
                    "is not a line or is not 2-component group";
               end if;
            end;
         when others =>
            raise CONVERSION_FAILED
              with "Requested line, but shape is " &
              collection.collection_item_shape (index)'Img;
      end case;
   end retrieve_line;


   ----------------------------
   --  retrieve_line_string  --
   ----------------------------
   function retrieve_line_string (collection : Geometry; index : Positive := 1)
                                  return Geometric_Line_String is
   begin
      collection.check_collection_index (index);
      case collection.contents is
         when single_line_string => return collection.line_string;
         when heterogeneous =>
            declare
               sub_index : Positive;
               data_size : Positive;
            begin
               collection.locate_heterogenous_item (index      => index,
                                                    set_index  => sub_index,
                                                    num_points => data_size);
               if collection.set_heterogeneous (sub_index).shape =
                 line_string_shape and then data_size >= 2
               then
                  declare
                     LNS : Geometric_Line_String (1 .. data_size);
                  begin
                     for x in Positive range 1 .. data_size loop
                        LNS (x) := collection.set_heterogeneous
                          (sub_index + x - 1).point;
                     end loop;
                     return LNS;
                  end;
               else
                  raise CONVERSION_FAILED
                    with "Data type at heterogeneous index" & index'Img &
                    "is not a line_string or is not at least 2 points long";
               end if;
            end;
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
                    " not found, but" & collection.items'Img;
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
         when others =>
            raise CONVERSION_FAILED
              with "Requested line_string, but shape is " &
              collection.collection_item_shape (index)'Img;
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
              collection.collection_item_shape (1)'Img;
      end case;
   end retrieve_circle;


   ------------------------
   --  retrieve_polygon  --
   ------------------------
   function retrieve_polygon (collection : Geometry; index : Positive := 1)
                              return Geometric_Polygon is
   begin
      collection.check_collection_index (index);
      case collection.contents is
         when single_polygon => return collection.polygon;
         when heterogeneous =>
            declare
               sub_index : Positive;
               data_size : Positive;
            begin
               collection.locate_heterogenous_item (index      => index,
                                                    set_index  => sub_index,
                                                    num_points => data_size);
               if collection.set_heterogeneous (sub_index).shape =
                 polygon_shape and then data_size > 2
               then
                  declare
                     LNS : Geometric_Polygon (1 .. data_size);
                  begin
                     for x in Positive range 1 .. data_size loop
                        LNS (x) := collection.set_heterogeneous
                          (sub_index + x - 1).point;
                     end loop;
                     return LNS;
                  end;
               else
                  raise CONVERSION_FAILED
                    with "Data type at heterogeneous index" & index'Img &
                    "is not a polygon or does not have at least 3 points";
               end if;
            end;
         when multi_polygon  =>
            declare
               sub_index : Positive;
               data_size : Positive;
               set_index_set : Boolean := False;
            begin
               for x in collection.set_polygons'Range loop
                  if collection.set_polygons (x).shape_id = index then
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
                    " not found, but" & collection.items'Img;
               end if;
               if data_size > 2 then
                  declare
                     LNS : Geometric_Polygon (1 .. data_size);
                  begin
                     for x in Positive range 1 .. data_size loop
                        LNS (x) := collection.set_polygons
                          (sub_index + x - 1).point;
                     end loop;
                     return LNS;
                  end;
               else
                  raise CONVERSION_FAILED
                    with "Data type at set_line_strings index" & index'Img &
                    "is not at least 3 points long";
               end if;
            end;
         when others =>
            raise CONVERSION_FAILED
              with "Requested polygon, but shape is " &
              collection.collection_item_shape (index)'Img;
      end case;
   end retrieve_polygon;


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
              collection.collection_item_shape (1)'Img;
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
   function format_real (value : Geometric_Real) return String is
      raw : constant String := value'Img;
   begin
      return CT.trim (raw);
   end format_real;


   -----------------------
   --  convert_to_text  --
   -----------------------
   function mysql_text (collection : Geometry) return String is
      function format_point (pt    : Geometric_Point;
                             first : Boolean := False) return String;
      function format_polygon (LNS   : Geometric_Polygon;
                               first : Boolean := False) return String;
      function format_line_string (LNS   : Geometric_Line_String;
                                   first : Boolean := False) return String;

      function format_point (pt    : Geometric_Point;
                             first : Boolean := False) return String
      is
         ptx  : constant String := format_real (pt.X);
         pty  : constant String := format_real (pt.Y);
         sep  : constant String := ", ";
         core : constant String := "Point(" & ptx & sep & pty & ")";
      begin
         if first then
            return core;
         else
            return sep & core;
         end if;
      end format_point;

      function format_polygon (LNS   : Geometric_Polygon;
                               first : Boolean := False) return String
      is
         lead : constant String := "Polygon(LineString(";
         sep  : constant String := ", ";
         work : CT.Text := CT.SUS (lead);
         inn1 : Boolean;
      begin
         for x in LNS'Range loop
            inn1 := (x = LNS'First);
                     CT.SU.Append (work, format_point (LNS (x), inn1));
         end loop;
         if first then
            return CT.USS (work) & "))";
         else
            return sep & CT.USS (work) & "))";
         end if;
      end format_polygon;

      function format_line_string (LNS : Geometric_Line_String;
                                   first : Boolean := False) return String
      is
         lead : constant String := "LineString(";
         sep  : constant String := ", ";
         work : CT.Text := CT.SUS (lead);
         inn1 : Boolean;
      begin
         for x in LNS'Range loop
            inn1 := (x = LNS'First);
                     CT.SU.Append (work, format_point (LNS (x), inn1));
         end loop;
         if first then
            return CT.USS (work) & ")";
         else
            return sep & CT.USS (work) & ")";
         end if;
      end format_line_string;

      classification : Collection_Type := collection.contents;
   begin
      case classification is
         when single_point => return format_point (collection.point, True);
         when single_line  =>
            return format_line_string (collection.line, True);
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
              format_point (collection.circle.center_point, True) & ", " &
              format_real (collection.circle.radius) & ")";
         when single_polygon =>
            return format_polygon (collection.polygon, True);
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
               return CT.USS (product) & ")";
            end;
         when multi_line_string =>
            declare
               product : CT.Text := CT.SUS ("MultiLineString(");
               first   : Boolean;
            begin
               for ls in 1 .. collection.items loop
                  first := (ls = 1);
                  CT.SU.Append
                    (product, format_line_string
                       (collection.retrieve_line_string (ls), first));
               end loop;
               return CT.USS (product) & ")";
            end;
         when multi_polygon =>
            declare
               product : CT.Text := CT.SUS ("MultiPolygon(");
               first   : Boolean;
            begin
               for ls in 1 .. collection.items loop
                  first := (ls = 1);
                  CT.SU.Append
                    (product, format_polygon
                       (collection.retrieve_polygon (ls), first));
               end loop;
               return CT.USS (product) & ")";
            end;
         when heterogeneous =>
            declare
               product : CT.Text := CT.SUS ("GeometryCollection(");
               first   : Boolean;
               flavor  : Geometric_Shape;
            begin
               for ls in 1 .. collection.items loop
                  first := (ls = 1);
                  flavor := collection.collection_item_shape;
                  case flavor is
                     when point_shape =>
                        CT.SU.Append
                          (product, format_point
                             (collection.retrieve_point (ls), first));
                     when line_string_shape =>
                        CT.SU.Append
                          (product, format_line_string
                             (collection.retrieve_line_string (ls), first));
                     when polygon_shape =>
                        CT.SU.Append
                          (product, format_polygon
                             (collection.retrieve_polygon (ls), first));
                     when circle_shape        => null;
                     when line_shape          => null;
                     when infinite_line_shape => null;
                  end case;
               end loop;
               return CT.USS (product) & ")";
            end;
      end case;
   end mysql_text;

   -----------------------
   --  Well_Known_Text  --
   -----------------------
   function Well_Known_Text (collection : Geometry) return String
   is
      function format_point (pt    : Geometric_Point;
                             first : Boolean := False;
                             label : Boolean := False) return String;
      function format_polygon (LNS   : Geometric_Polygon;
                               first : Boolean := False;
                               label : Boolean := False) return String;
      function format_line_string (LNS   : Geometric_Line_String;
                                   first : Boolean := False;
                                   label : Boolean := False) return String;

      function format_point (pt    : Geometric_Point;
                             first : Boolean := False;
                             label : Boolean := False) return String
      is
         ptx    : constant String := format_real (pt.X);
         pty    : constant String := format_real (pt.Y);
         lead   : constant String := "POINT ";
         sep    : constant String := ", ";
         popen  : constant String := "(";
         pclose : constant String := ")";
         core   : constant String := ptx & sep & pty;
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

      --  IMPLEMENT inner polygons
      function format_polygon (LNS   : Geometric_Polygon;
                               first : Boolean := False;
                               label : Boolean := False) return String
      is
         lead   : constant String := "POLYGON ";
         sep    : constant String := ", ";
         popen  : constant String := "((";
         pclose : constant String := "))";
         work   : CT.Text;
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
      end format_polygon;

      function format_line_string (LNS : Geometric_Line_String;
                                   first : Boolean := False;
                                   label : Boolean := False) return String
      is
         lead   : constant String := "LINESTRING ";
         sep    : constant String := ", ";
         popen  : constant String := "(";
         pclose : constant String := ")";
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
         when single_point =>
            return format_point (collection.point, True, True);
         when single_line  =>
            return format_line_string (collection.line, True, True);
         when single_line_string =>
            return format_line_string (collection.line_string, True, True);
         when single_infinite_line =>
            --  Infinite lines are not supported by WKT so this is wrong
            return format_line_string (collection.infinite_line, True, True);
         when single_circle =>
            --  No circles in WKT, so using this output will result in error
            return "CIRCLE (" &
              format_point (collection.circle.center_point, True) & ", " &
              format_real (collection.circle.radius) & ")";
         when single_polygon =>
            return format_polygon (collection.polygon, True, True);
         when multi_point =>
            declare
               product : CT.Text := CT.SUS ("MULTIPOINT (");
               first   : Boolean;
            begin
               for x in collection.set_points'Range loop
                  first := (x = collection.set_points'First);
                  CT.SU.Append
                    (product, format_point
                       (collection.set_points (x), first));
               end loop;
               return CT.USS (product) & ")";
            end;
         when multi_line_string =>
            declare
               product : CT.Text := CT.SUS ("MULTILINESTRING (");
               first   : Boolean;
            begin
               for ls in 1 .. collection.items loop
                  first := (ls = 1);
                  CT.SU.Append
                    (product, format_line_string
                       (collection.retrieve_line_string (ls), first));
               end loop;
               return CT.USS (product) & ")";
            end;
         when multi_polygon =>
            declare
               product : CT.Text := CT.SUS ("MULTIPOLYGON (");
               first   : Boolean;
            begin
               for ls in 1 .. collection.items loop
                  first := (ls = 1);
                  CT.SU.Append
                    (product, format_polygon
                       (collection.retrieve_polygon (ls), first));
               end loop;
               return CT.USS (product) & ")";
            end;
         when heterogeneous =>
            declare
               product : CT.Text := CT.SUS ("GEOMETRYCOLLECTION (");
               first   : Boolean;
               flavor  : Geometric_Shape;
            begin
               for ls in 1 .. collection.items loop
                  first := (ls = 1);
                  flavor := collection.collection_item_shape;
                  case flavor is
                     when point_shape =>
                        CT.SU.Append
                          (product, format_point
                             (collection.retrieve_point (ls), first, True));
                     when line_string_shape =>
                        CT.SU.Append
                          (product, format_line_string
                             (collection.retrieve_line_string (ls), first,
                              True));
                     when polygon_shape =>
                        CT.SU.Append
                          (product, format_polygon
                             (collection.retrieve_polygon (ls), first, True));
                     when circle_shape        => null;
                     when line_shape          => null;
                     when infinite_line_shape => null;
                  end case;
               end loop;
               return CT.USS (product) & ")";
            end;
      end case;

   end Well_Known_Text;


end Spatial_Data;
