--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Characters.Latin_1;

package body Spatial_Data is

   package LAT renames Ada.Characters.Latin_1;

   ---------------------------
   --  initialize_as_point  --
   ---------------------------
   function initialize_as_point (point : Geometric_Point) return Geometry
   is
      metadata : Ring_Structure := (Item_Type   => single_point,
                                    Item_ID     => 1,
                                    Ring_ID     => 1,
                                    Ring_Size   => 1,
                                    Ring_Count  => 1,
                                    Point_Index => 1,
                                    Level_Flags => 0,
                                    Group_ID    => 1);
   begin
      return (contents   => single_point,
              units      => 1,
              subunits   => 1,
              points     => 1,
              structures => (1 => metadata),
              points_set => (1 => point));
   end initialize_as_point;


   ---------------------------------
   --  initialize_as_multi_point  --
   ---------------------------------
   function initialize_as_multi_point (point : Geometric_Point) return Geometry
   is
      metadata : Ring_Structure := (Item_Type   => multi_point,
                                    Item_ID     => 1,
                                    Ring_ID     => 1,
                                    Ring_Size   => 1,
                                    Ring_Count  => 1,
                                    Point_Index => 1,
                                    Level_Flags => 0,
                                    Group_ID    => 1);
   begin
      return (contents   => multi_point,
              units      => 1,
              subunits   => 1,
              points     => 1,
              structures => (1 => metadata),
              points_set => (1 => point));
   end initialize_as_multi_point;


   --------------------------
   --  initialize_as_line  --
   --------------------------
   function initialize_as_line (line_string : Geometric_Line_String)
                                return Geometry
   is
      metadata : Ring_Structure := (Item_Type   => single_line_string,
                                    Item_ID     => 1,
                                    Ring_ID     => 1,
                                    Ring_Size   => line_string'Length,
                                    Ring_Count  => 1,
                                    Point_Index => 1,
                                    Level_Flags => 0,
                                    Group_ID    => 1);
   begin
      return (contents   => single_line_string,
              units      => 1,
              subunits   => 1,
              points     => line_string'Length,
              structures => (1 => metadata),
              points_set => line_string);
   end initialize_as_line;


   --------------------------------
   --  initialize_as_multi_line  --
   --------------------------------
   function initialize_as_multi_line (line_string : Geometric_Line_String)
                                      return Geometry
   is
      metadata : Ring_Structure := (Item_Type   => multi_line_string,
                                    Item_ID     => 1,
                                    Ring_ID     => 1,
                                    Ring_Size   => line_string'Length,
                                    Ring_Count  => 1,
                                    Point_Index => 1,
                                    Level_Flags => 0,
                                    Group_ID    => 1);
   begin
      return (contents   => multi_line_string,
              units      => 1,
              subunits   => 1,
              points     => line_string'Length,
              structures => (1 => metadata),
              points_set => line_string);
   end initialize_as_multi_line;


   ---------------------
   --  start_polygon  --
   ---------------------
   function start_polygon (outer_ring : Geometric_Ring)
                           return Geometric_Polygon
   is
      num_points : constant Natural := outer_ring'Length;
      PG : Geometric_Polygon (rings  => 1, points => num_points);
      metadata : Ring_Structure := (Item_Type   => single_polygon,
                                    Item_ID     => 1,
                                    Ring_ID     => 1,
                                    Ring_Size   => num_points,
                                    Ring_Count  => 1,
                                    Point_Index => 1,
                                    Level_Flags => 0,
                                    Group_ID    => 1);
   begin
      if num_points < 4 then
         raise LACKING_POINTS
           with "polygon rings must have at least 4 points (found only" &
           num_points'Img & ")";
      end if;
      PG.structures := (1 => metadata);
      PG.points_set := outer_ring;
      return PG;
   end start_polygon;


   -------------------------
   --  append_inner_ring  --
   -------------------------
   procedure append_inner_ring (polygon    : in out Geometric_Polygon;
                                inner_ring : Geometric_Ring)
   is
      num_points   : constant Natural := inner_ring'Length;
      last_ring    : constant Positive := polygon.rings + 1;
      total_points : constant Natural := polygon.points + num_points;
      PG : Geometric_Polygon (rings  => last_ring, points => total_points);
      metadata : Ring_Structure := (Item_Type   => single_polygon,
                                    Item_ID     => 1,
                                    Ring_ID     => last_ring,
                                    Ring_Size   => num_points,
                                    Ring_Count  => last_ring,
                                    Point_Index => polygon.points + 1,
                                    Level_Flags => 0,
                                    Group_ID    => 1);
   begin
      if num_points < 4 then
         raise LACKING_POINTS
           with "polygon rings must have at least 4 points (found only" &
           num_points'Img & ")";
      end if;
      for ring in 1 .. polygon.rings loop
         PG.structures (ring) := polygon.structures (ring);
         PG.structures (ring).Ring_Count := last_ring;
      end loop;
      PG.structures (last_ring) := metadata;

      for pt in 1 .. polygon.points loop
         PG.points_set (pt) := polygon.points_set (pt);
      end loop;
      for pt in 1 .. num_points loop
         PG.points_set (polygon.points + pt) := inner_ring (pt);
      end loop;
      polygon := PG;
   end append_inner_ring;


   -----------------------
   --  number_of_rings  --
   -----------------------
   function number_of_rings (polygon : Geometric_Polygon) return Natural is
   begin
      return Natural (polygon.rings);
   end number_of_rings;


   ---------------------
   --  retrieve_ring  --
   ---------------------
   function retrieve_ring (polygon : Geometric_Polygon; ring_index : Positive)
                           return Geometric_Ring is
   begin
      if ring_index > polygon.rings then
         raise OUT_OF_COLLECTION_RANGE
           with "Requested ring" & ring_index'Img &
           ", but there are only" & polygon.rings'Img & " available";
      end if;
      declare
         num_points : Positive := polygon.structures (ring_index).Ring_Size;
         start_here : Positive := polygon.structures (ring_index).Point_Index;
         finish     : Positive := start_here + num_points - 1;
         GR : Geometric_Ring (1 .. num_points);
      begin
         GR := polygon.points_set (start_here .. finish);
         return GR;
      end;
   end retrieve_ring;


   -----------------------------
   --  initialize_as_polygon  --
   -----------------------------
   function initialize_as_polygon (polygon : Geometric_Polygon)
                                   return Geometry
   is
      GM : Geometry (contents   => single_polygon,
                     units      => 1,
                     subunits   => polygon.rings,
                     points     => polygon.points);
   begin
      for ring in 1 .. polygon.rings loop
         GM.structures (ring) := polygon.structures (ring);
      end loop;

      for pt in 1 .. polygon.points loop
         GM.points_set (pt) := polygon.points_set (pt);
      end loop;

      return GM;
   end initialize_as_polygon;


   -----------------------------------
   --  initialize_as_multi_polygon  --
   -----------------------------------
   function initialize_as_multi_polygon (polygon : Geometric_Polygon)
                                         return Geometry
   is
      GM : Geometry (contents   => multi_polygon,
                     units      => 1,
                     subunits   => polygon.rings,
                     points     => polygon.points);
   begin
      for ring in 1 .. polygon.rings loop
         GM.structures (ring) := polygon.structures (ring);
         GM.structures (ring).Item_Type := multi_polygon;
      end loop;

      for pt in 1 .. polygon.points loop
         GM.points_set (pt) := polygon.points_set (pt);
      end loop;

      return GM;
   end initialize_as_multi_polygon;


   --------------------------------
   --  initialize_as_collection  --
   --------------------------------
   function initialize_as_collection (anything : Geometry) return Geometry
   is
      classification : Collection_Type := anything.contents;
      GM : Geometry (contents   => heterogeneous,
                     units      => anything.units,
                     subunits   => anything.subunits,
                     points     => anything.points);
   begin
      GM.structures := anything.structures;
      GM.points_set := anything.points_set;
      for ring in 1 .. anything.subunits loop
         --  Shift any existing flags over one place before setting level
         GM.structures (ring).Level_Flags := 1 +
           (anything.structures (ring).Level_Flags * 2);
      end loop;
      return GM;
   end initialize_as_collection;


   --------------------------
   --  size_of_collection  --
   --------------------------
   function size_of_collection (collection : Geometry) return Positive is
   begin
      if collection.contents = heterogeneous then
         --  For colletions, return the number of groups, not units
         return collection.structures (collection.structures'Last).Group_ID;
      else
         return collection.units;
      end if;
   end size_of_collection;


   --------------------------
   --  type_of_collection  --
   --------------------------
   function type_of_collection (collection : Geometry) return Collection_Type
   is
   begin
      return collection.contents;
   end type_of_collection;


   ---------------------------
   --  augment_multi_point  --
   ---------------------------
   procedure augment_multi_point (collection : in out Geometry;
                                  point      : Geometric_Point)
   is
   begin
      case collection.contents is
         when multi_point =>
            declare
               last_point : Geo_Points := collection.points + 1;
               last_unit  : Geo_Units  := collection.units + 1;
               GM : Geometry (contents   => multi_point,
                              units      => last_unit,
                              subunits   => last_unit,
                              points     => last_point);
            begin
               for ring in 1 .. collection.subunits loop
                  GM.structures (ring) := collection.structures (ring);
                  GM.structures (ring).Ring_Count := last_unit;
               end loop;
               GM.points_set (1 .. collection.points) := collection.points_set;
               GM.structures (last_unit) := (Item_Type   => multi_point,
                                             Item_ID     => last_unit,
                                             Ring_ID     => 1,
                                             Ring_Size   => 1,
                                             Ring_Count  => last_unit,
                                             Point_Index => last_point,
                                             Level_Flags => 0,
                                             Group_ID    => 1);
               GM.points_set (last_point) := point;
               collection := GM;
            end;
         when others =>
            raise ILLEGAL_SHAPE
              with "The collection must already be a multi_point type";
      end case;
   end augment_multi_point;


   --------------------------
   --  augment_multi_line  --
   --------------------------
   procedure augment_multi_line (collection : in out Geometry;
                                 line       : Geometric_Line_String)
   is
   begin
      case collection.contents is
         when multi_line_string =>
            declare
               LL          : Natural    := line'Length;
               first_point : Geo_Points := collection.points + 1;
               last_point  : Geo_Points := collection.points + LL;
               last_unit   : Geo_Units  := collection.units + 1;
               marker      : Positive   := line'First;
               GM : Geometry (contents   => multi_line_string,
                              units      => last_unit,
                              subunits   => last_unit,
                              points     => last_point);
            begin
               for ring in 1 .. collection.subunits loop
                  GM.structures (ring) := collection.structures (ring);
                  GM.structures (ring).Ring_Count := last_unit;
               end loop;
               GM.points_set (1 .. collection.points) := collection.points_set;
               GM.structures (last_unit) := (Item_Type   => multi_line_string,
                                             Item_ID     => last_unit,
                                             Ring_ID     => 1,
                                             Ring_Size   => LL,
                                             Ring_Count  => last_unit,
                                             Point_Index => first_point,
                                             Level_Flags => 0,
                                             Group_ID    => 1);
               for pt in first_point .. last_point loop
                  GM.points_set (pt) := line (marker);
                  marker := marker + 1;
               end loop;
               collection := GM;
            end;
         when others =>
            raise ILLEGAL_SHAPE
              with "The collection must already be a multi_line_string type";
      end case;
   end augment_multi_line;


   -----------------------------
   --  augment_multi_polygon  --
   -----------------------------
   procedure augment_multi_polygon (collection : in out Geometry;
                                    polygon    : Geometric_Polygon)
   is
   begin
      case collection.contents is
         when multi_polygon =>
            declare
               num_points    : Geo_Points := polygon.points;
               first_point   : Geo_Points := collection.points + 1;
               last_point    : Geo_Points := collection.points + num_points;
               last_unit     : Geo_Units  := collection.units + 1;
               first_subunit : Geo_Units  := collection.subunits + 1;
               last_subunit  : Geo_Units  := collection.subunits +
                                            polygon.rings;
               marker        : Positive   := polygon.structures'First;
               ptmr          : Geo_Points := first_point;
               ppsm          : Geo_Points := polygon.points_set'First;
               GM : Geometry (contents   => multi_polygon,
                              units      => last_unit,
                              subunits   => last_subunit,
                              points     => last_point);
            begin
               for ring in 1 .. collection.subunits loop
                  GM.structures (ring) := collection.structures (ring);
                  GM.structures (ring).Ring_Count := last_subunit;
               end loop;
               GM.points_set (1 .. collection.points) := collection.points_set;

               for ring in first_subunit .. last_subunit loop
                  GM.structures (ring) :=
                    (Item_Type   => multi_polygon,
                     Item_ID     => last_unit,
                     Ring_ID     => polygon.structures (marker).Ring_ID,
                     Ring_Size   => polygon.structures (marker).Ring_Size,
                     Ring_Count  => last_subunit,
                     Point_Index => ptmr,
                     Level_Flags => 0,
                     Group_ID    => 1);
                  ptmr := ptmr + polygon.structures (marker).Ring_Size;
                  marker := marker + 1;
               end loop;

               for pt in first_point .. last_point loop
                  GM.points_set (pt) := polygon.points_set (ppsm);
                  ppsm := ppsm + 1;
               end loop;
               collection := GM;
            end;
         when others =>
            raise ILLEGAL_SHAPE
              with "The collection must already be a multi_polygon type";
      end case;
   end augment_multi_polygon;


   --------------------------
   --  augment_collection  --
   --------------------------
   procedure augment_collection (collection : in out Geometry;
                                 anything   : Geometry) is
   begin
      case collection.contents is
         when heterogeneous =>
            declare
               num_points    : Geo_Points := anything.points;
               first_point   : Geo_Points := collection.points + 1;
               last_point    : Geo_Points := collection.points + num_points;
               last_unit     : Geo_Units  := collection.units + 1;
               first_subunit : Geo_Units  := collection.subunits + 1;
               last_subunit  : Geo_Units  := collection.subunits +
                                             anything.subunits;
               marker        : Positive   := anything.structures'First;
               ptmr          : Geo_Points := first_point;
               ppsm          : Geo_Points := anything.points_set'First;
               multiplier    : constant collection_flags :=
                               highest_level (collection) * 2;
               last_id       : Positive   :=
                 collection.structures (collection.subunits).Item_ID;
               next_group    : Positive   :=
                 collection.structures (collection.subunits).Group_ID + 1;
               GM : Geometry (contents   => heterogeneous,
                              units      => last_unit,
                              subunits   => last_subunit,
                              points     => last_point);
            begin

               GM.structures (1 .. collection.subunits) :=
                 collection.structures;
               GM.points_set (1 .. collection.points) := collection.points_set;

               for ring in first_subunit .. last_subunit loop
                  GM.structures (ring) :=
                    (Item_Type   => anything.structures (marker).Item_Type,
                     Item_ID     => anything.structures (marker).Item_ID +
                                    last_id,
                     Ring_ID     => anything.structures (marker).Ring_ID,
                     Ring_Size   => anything.structures (marker).Ring_Size,
                     Ring_Count  => anything.structures (marker).Ring_Count,
                     Point_Index => ptmr,
                     Level_Flags => (anything.structures (marker).Level_Flags *
                                    multiplier) + 1,
                     Group_ID    => next_group);
                  ptmr := ptmr + anything.structures (marker).Ring_Size;
                  marker := marker + 1;
               end loop;

               for pt in first_point .. last_point loop
                  GM.points_set (pt) := anything.points_set (ppsm);
                  ppsm := ppsm + 1;
               end loop;
               collection := GM;
            end;
      when others =>
            raise ILLEGAL_SHAPE
              with "The collection must already be a hetegeneous type";
      end case;
   end augment_collection;


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


   ----------------------
   --  retrieve_point  --
   ----------------------
   function retrieve_point (collection : Geometry; index : Positive := 1)
                             return Geometric_Point is
   begin
      check_collection_index (collection, index);
      case collection.contents is
         when single_point | multi_point =>
            return collection.points_set (index);
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
                                  return Geometric_Line_String is
   begin
      check_collection_index (collection, index);
      case collection.contents is
         when single_line_string | multi_line_string  =>
            declare
               CS : Ring_Structure renames collection.structures (index);
               data_size   : Positive   := CS.Ring_Size;
               first_point : Geo_Points := CS.Point_Index;
               last_point  : Geo_Points := first_point + data_size - 1;
               LNS         : Geometric_Line_String (1 .. data_size);
            begin
               LNS := collection.points_set (first_point .. last_point);
               return LNS;
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
   end retrieve_line;


   ------------------------
   --  retrieve_polygon  --
   ------------------------
   function retrieve_polygon (collection : Geometry; index : Positive := 1)
                              return Geometric_Polygon
   is
      found     : Boolean := False;
      F_subunit : Geo_Units;
      L_subunit : Geo_Units;
      product   : Geometric_Polygon;
   begin
      check_collection_index (collection, index);
      case collection.contents is
         when single_polygon | multi_polygon  =>
            for subunit in 1 .. collection.subunits loop
               if collection.structures (subunit).Item_ID = index then
                  if not found then
                     F_subunit := subunit;
                  end if;
                  L_subunit := subunit;
                  found := True;
               end if;
            end loop;
            if not found then
               raise OUT_OF_COLLECTION_RANGE
                 with "Failed to locate polygon" & index'Img;
            end if;
            declare
               CS : Ring_Structure renames collection.structures (F_subunit);
               data_size   : Positive   := CS.Ring_Size;
               first_point : Geo_Points := CS.Point_Index;
               last_point  : Geo_Points := first_point + data_size - 1;
               outer_ring  : Geometric_Ring (1 .. data_size);
            begin
               outer_ring := collection.points_set (first_point .. last_point);
               product := start_polygon (outer_ring);
            end;
            for subunit in F_subunit + 1 .. L_subunit loop
               declare
                  CS : Ring_Structure renames collection.structures (subunit);
                  data_size   : Positive   := CS.Ring_Size;
                  first_point : Geo_Points := CS.Point_Index;
                  last_point  : Geo_Points := first_point + data_size - 1;
                  hole        : Geometric_Ring (1 .. data_size);
               begin
                  hole := collection.points_set (first_point .. last_point);
                  append_inner_ring (product, hole);
               end;
            end loop;
            return product;
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


   ---------------------
   --  single_canvas  --
   ---------------------
   function single_canvas (gm_type  : Collection_Type;
                           items    : Item_ID_type;
                           subunits : Geo_Units;
                           points   : Geo_Points) return Geometry
   is
      p_set : Geometric_Point_Collection (1 .. points) :=
                    (others => Origin_Point);
      s_set : Ring_Structures (1 .. subunits) :=
              (others => (Item_Type   => single_point,
                          Item_ID     => 1,
                          Ring_ID     => 1,
                          Ring_Size   => 1,
                          Ring_Count  => 1,
                          Point_Index => 1,
                          Level_Flags => 0,
                          Group_ID    => 1));
   begin
      case gm_type is
         when unset =>
            return (unset, 1, 1, 1);
         when single_point =>
            return (single_point, items, 1, 1, s_set, p_set);
         when single_line_string =>
            return (single_line_string, items, 1, points, s_set, p_set);
         when single_polygon =>
            return (single_polygon, items, subunits, points, s_set, p_set);
         when multi_point =>
            return (multi_point, items, subunits, points, s_set, p_set);
         when multi_line_string =>
            return (multi_line_string, items, subunits, points, s_set, p_set);
         when multi_polygon =>
            return (multi_polygon, items, subunits, points, s_set, p_set);
         when heterogeneous =>
            return (contents => heterogeneous,
                    units    => items,
                    subunits => subunits,
                    points   => points,
                    structures => s_set,
                    points_set => p_set);
      end case;
   end single_canvas;


   ------------------------------
   --  retrieve_subcollection  --
   ------------------------------
   function retrieve_subcollection (collection : Geometry;
                                    index : Positive := 1) return Geometry
   is
      function cut (flags : collection_flags) return collection_flags;
      found      : Boolean := False;
      num_points : Natural := 0;
      num_sunits : Geo_Units := 0;
      num_items  : Natural := 0;
      prev_unit  : Natural := 0;
      first_unit : Natural := 0;
      prev_flags : collection_flags;
      F_subunit  : Geo_Units;
      L_subunit  : Geo_Units;
      coltype    : Collection_Type;
      function cut (flags : collection_flags) return collection_flags is
      begin
         return flags / 2;
      end cut;
   begin
      case collection.contents is
         when unset |
              single_point |
              single_polygon |
              single_line_string =>
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
               LS : Geometric_Line_String := retrieve_line (collection, index);
            begin
               return initialize_as_line (LS);
            end;
         when multi_polygon =>
            declare
               PG : Geometric_Polygon := retrieve_polygon (collection, index);
            begin
               return initialize_as_polygon (PG);
            end;
         when heterogeneous =>
            for subunit in 1 .. collection.subunits loop
               declare
                  CSU : Ring_Structure renames collection.structures (subunit);
                  lvl : collection_flags := cut (cut (CSU.Level_Flags));
               begin
                  if CSU.Group_ID = index then
                     if not found then
                        found      := True;
                        F_subunit  := subunit;
                        coltype    := CSU.Item_Type;
                        prev_unit  := CSU.Item_ID;
                        first_unit := CSU.Item_ID;
                        prev_flags := lvl;
                        num_items  := 1;
                        if cut (CSU.Level_Flags) > 0 then
                           coltype := heterogeneous;
                        end if;
                     end if;
                     L_subunit  := subunit;
                     num_sunits := num_sunits + 1;
                     num_points := num_points + CSU.Ring_Size;
                     if coltype = heterogeneous then
                        if lvl = 0 then
                           --  If lvl = 0 then we're in a geometry
                           --  collection that does not contain other
                           --  collections.  Thus the active group ID points
                           --  to a single* or multi* type, and all Item_IDs
                           --  are counted as retrievable items.
                           --  If we find a ring count > 1 then we have a
                           --  multi* type that was added to a collection
                           --  so keep these together (group ID gets mangled)
                           if CSU.Item_ID /= prev_unit then
                              num_items := num_items + 1;
                           end if;
                        else
                           --  Within this collection is another geometry
                           --  collection.  Items with the same baseflags are
                           --  considered a single unit.  Only count changes
                           --  to and from level 0.  Item IDs always change
                           --  at those borders; no need to check
                           if prev_flags = 0 then
                              num_items := num_items + 1;
                           end if;
                        end if;
                     else
                        --  single* types only have one unit, 1 group
                        --  multi* types have 1+ units, but only 1 group
                        num_items := CSU.Item_ID - first_unit + 1;
                     end if;
                     prev_flags := lvl;
                     prev_unit  := CSU.Item_ID;
                  end if;
               end;
            end loop;
            if not found then
               raise OUT_OF_COLLECTION_RANGE
                 with "Failed to locate subcollection" & index'Img;
            end if;
            case coltype is
               when unset =>
                  raise CONVERSION_FAILED
                    with "Illegal heterogenous type (unset)";
               when others =>
                  declare
                     RS : Ring_Structures renames collection.structures;
                     CS : Ring_Structure renames RS (F_subunit);
                     FP : Geo_Points := CS.Point_Index;
                     LP : Geo_Points := FP + num_points - 1;
                     GM : Geometry := single_canvas (coltype,
                                                     num_items,
                                                     num_sunits,
                                                     num_points);
                     marker : Geo_Units := 1;
                     diff   : Natural := CS.Item_ID - 1;
                     ptdiff : Natural := CS.Point_Index - 1;
                     group  : Positive := 1;
                     lvl    : collection_flags;
                     rseek  : Natural := 0;
                     rtrack : Natural := 0;
                  begin
                     prev_unit  := CS.Item_ID;
                     prev_flags := cut (cut (CS.Level_Flags));
                     GM.points_set (1 .. num_points) :=
                       collection.points_set (FP .. LP);
                     for S in F_subunit .. L_subunit loop
                        if coltype = heterogeneous then
                           lvl := cut (cut (RS (S).Level_Flags));
                           if lvl = 0 then
                              rtrack := rtrack + 1;
                              if rtrack > rseek then
                                 if RS (S).Item_ID  /= prev_unit then
                                    group := group + 1;
                                 end if;
                                 rseek  := RS (S).Ring_Count;
                                 rtrack := 1;
                              end if;
                           else
                              if prev_flags = 0 then
                                 group := group + 1;
                              end if;
                           end if;
                           prev_unit  := RS (S).Item_ID;
                           prev_flags := lvl;
                        end if;
                        GM.structures (marker) :=
                          (Item_Type   => RS (S).Item_Type,
                           Item_ID     => RS (S).Item_ID - diff,
                           Ring_ID     => RS (S).Ring_ID,
                           Ring_Size   => RS (S).Ring_Size,
                           Ring_Count  => RS (S).Ring_Count,
                           Point_Index => RS (S).Point_Index - ptdiff,
                           Level_Flags => cut (RS (S).Level_Flags),
                           Group_ID    => group);
                        marker := marker + 1;
                     end loop;
                     return GM;
                  end;
            end case;
      end case;
   end retrieve_subcollection;


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
              single_polygon |
              single_line_string =>
            return collection.contents;
         when multi_point => return single_point;
         when multi_line_string => return single_line_string;
         when multi_polygon => return single_polygon;
         when heterogeneous =>
            for subunit in 1 .. collection.subunits loop
               if collection.structures (subunit).Item_ID = index then
                  return collection.structures (subunit).Item_Type;
               end if;
            end loop;
            raise OUT_OF_COLLECTION_RANGE
              with "collection_item_type out of range: " & index'Img;
      end case;
   end collection_item_type;


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
      raw    : constant String := CT.trim (Geometric_Real'Image (abs (value)));
      last3  : constant String := raw (raw'Last - 2 .. raw'Last);
      posend : constant Natural := raw'Last - 4;
      shift  : constant Integer := Integer'Value (last3);
      is_neg : constant Boolean := value < 0.0;
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
         if is_neg then
            return "-" & S (left .. right);
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


   ---------------------
   --  highest_level  --
   ---------------------
   function highest_level (collection : Geometry) return collection_flags
   is
      res : collection_flags := 0;
   begin
      for csu in 1 .. collection.subunits loop
         if collection.structures (csu).Level_Flags > res then
            res := collection.structures (csu).Level_Flags;
         end if;
      end loop;
      return res;
   end highest_level;


   ------------
   --  dump  --
   ------------
   function dump (collection : Geometry) return String
   is
      function bin (level : collection_flags) return String;

      res : CT.Text;
      most : collection_flags := highest_level (collection);

      function bin (level : collection_flags) return String
      is
         mask : collection_flags;
         res  : String (1 .. 24) := (others => '0');
      begin
         if most = 0 then
            return "0";
         end if;
         for bit in 0 .. 23 loop
            mask := 2 ** bit;
            if mask > most then
               return res (1 .. bit);
            end if;
            if (level and mask) > 0 then
               res (bit + 1) := '1';
            end if;
         end loop;
         return res;
      end bin;
   begin

      CT.SU.Append (res,
           "contents : " & collection.contents'Img & LAT.LF &
           "units    : " & CT.int2str (collection.units) & LAT.LF &
           "subunits : " & CT.int2str (collection.subunits) & LAT.LF &
           "points   : " & CT.int2str (collection.points) & LAT.LF);
      for R in 1 .. collection.subunits loop
         CT.SU.Append (res, LAT.LF & "Ring #" & CT.int2str (R) & LAT.LF);
         declare
            CS : Ring_Structure renames collection.structures (R);
         begin
            CT.SU.Append (res,
                 "  Type     : " & CS.Item_Type'Img & LAT.LF &
                 "  Item_ID  : " & CT.int2str (CS.Item_ID) & LAT.LF &
                 "  Ring_ID  : " & CT.int2str (CS.Ring_ID) & LAT.LF &
                 "  Set Size : " & CT.int2str (CS.Ring_Count) & LAT.LF &
                 "  Size     : " & CT.int2str (CS.Ring_Size) & LAT.LF &
                 "  Pt Index : " & CT.int2str (CS.Point_Index) & LAT.LF &
                 "  Level    : " & bin (CS.Level_Flags) & LAT.LF &
                 "  Group ID : " & CT.int2str (CS.Group_ID) & LAT.LF);
         end;
      end loop;
      CT.SU.Append (res, LAT.LF & "Serialized Points" & LAT.LF);
      for PI in 1 .. collection.points loop
         declare
            coord : Geometric_Point renames collection.points_set (PI);
            line : String := CT.zeropad (PI, 2) & ": " &
              format_real (coord.X) & ", " & format_real (coord.Y);
         begin
            CT.SU.Append (res, line & LAT.LF);
         end;
      end loop;
      return CT.USS (res);
   end dump;


   ------------------
   --  mysql_text  --
   ------------------
   function mysql_text (collection : Geometry; top_first : Boolean := True)
                        return String is
      function initialize_title (title : String) return CT.Text;
      function format_point (pt    : Geometric_Point;
                             first : Boolean := False) return String;
      function format_polygon (poly  : Geometric_Polygon;
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

      function format_polygon (poly  : Geometric_Polygon;
                               first : Boolean := False) return String
      is
         lead   : constant String := "Polygon(";
         work   : CT.Text;
         lastsc : Natural := 0;
         inner1 : Boolean;
         nrings : Natural := number_of_rings (poly);
      begin
         if first then
            CT.SU.Append (work, lead);
         else
            CT.SU.Append (work, sep & lead);
         end if;
         for ring in 1 .. nrings loop
            if ring > 1 then
               CT.SU.Append (work, sep);
            end if;
            CT.SU.Append (work, "Linestring(");
            declare
               GR : Geometric_Ring := retrieve_ring (poly, ring);
            begin
               for pt in GR'Range loop
                  inner1 := (pt = GR'First);
                  CT.SU.Append (work, format_point (GR (pt), inner1));
               end loop;
            end;
            CT.SU.Append (work, pclose);
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

      function initialize_title (title : String) return CT.Text is
      begin
         if top_first then
            return CT.SUS (title);
         else
            return CT.SUS (sep & title);
         end if;
      end initialize_title;

      classification : Collection_Type := collection.contents;
   begin
      case classification is
         when unset        => return "";
         when single_point =>
            return format_point (retrieve_point (collection), top_first);
         when single_line_string =>
            return format_line_string (retrieve_line (collection), top_first);
         when single_polygon =>
            return format_polygon (retrieve_polygon (collection), top_first);
         when multi_point =>
            declare
               lead    : constant String := "MultiPoint(";
               first   : Boolean := True;
               product : CT.Text;
            begin
               if top_first then
                  CT.SU.Append (product, lead);
               else
                  CT.SU.Append (product, sep & lead);
               end if;
               for x in collection.points_set'Range loop
                  CT.SU.Append
                    (product, format_point
                       (collection.points_set (x), first));
                  first := False;
               end loop;
               return CT.USS (product) & pclose;
            end;
         when multi_line_string =>
            declare
               product : CT.Text := initialize_title ("MultiLineString(");
               first   : Boolean := True;
            begin
               for ls in 1 .. collection.units loop
                  CT.SU.Append
                    (product, format_line_string
                       (retrieve_line (collection, ls), first));
                  first := False;
               end loop;
               return CT.USS (product) & pclose;
            end;
         when multi_polygon =>
            declare
               lead    : constant String := "MultiPolygon(";
               first   : Boolean := True;
               product : CT.Text;
            begin
               if top_first then
                  if collection.units > 1 then
                     CT.SU.Append (product, lead);
                  end if;
               else
                  if collection.units > 1 then
                     CT.SU.Append (product, sep & lead);
                  else
                     CT.SU.Append (product, sep);
                  end if;
               end if;
               for ls in 1 .. collection.units loop
                  CT.SU.Append
                    (product, format_polygon
                       (retrieve_polygon (collection, ls), first));
                  first := False;
               end loop;
               if collection.units > 1 then
                  CT.SU.Append (product, pclose);
               end if;
               return CT.USS (product);
            end;
         when heterogeneous =>
            declare
               product : CT.Text := initialize_title ("GeometryCollection(");
               first   : Boolean := True;
               GM      : Geometry;
            begin
               for ls in 1 .. size_of_collection (collection) loop
                  GM := retrieve_subcollection (collection, ls);
                  CT.SU.Append (product, mysql_text (GM, first));
                  first := False;
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
      function initialize_title (title : String) return CT.Text;
      function format_point (pt    : Geometric_Point;
                             first : Boolean := False;
                             label : Boolean := False) return String;
      function format_polygon (poly  : Geometric_Polygon;
                               first : Boolean := False;
                               label : Boolean := False) return String;
      function format_line_string (LNS   : Geometric_Line_String;
                                   first : Boolean := False;
                                   label : Boolean := False) return String;

      sep    : constant String := ",";
      popen  : constant String := "(";
      pclose : constant String := ")";

      function initialize_title (title : String) return CT.Text is
      begin
         if top_first then
            return CT.SUS (title);
         else
            return CT.SUS (sep & title);
         end if;
      end initialize_title;

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

      function format_polygon (poly  : Geometric_Polygon;
                               first : Boolean := False;
                               label : Boolean := False) return String
      is
         lead   : constant String := "POLYGON";
         work   : CT.Text;
         inner1 : Boolean;
         lastsc : Natural := 0;
         nrings : Natural := number_of_rings (poly);
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
         for ring in 1 .. nrings loop
            if ring > 1 then
               CT.SU.Append (work, sep);
            end if;
            CT.SU.Append (work, popen);
            declare
               GR : Geometric_Ring := retrieve_ring (poly, ring);
            begin
               for pt in GR'Range loop
                  inner1 := (pt = GR'First);
                  CT.SU.Append (work, format_point (GR (pt), inner1));
               end loop;
            end;
            CT.SU.Append (work, pclose);
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
            return format_point (retrieve_point (collection), top_first, True);
         when single_line_string =>
            return format_line_string (retrieve_line (collection),
                                       top_first, True);
         when single_polygon =>
            return format_polygon (retrieve_polygon (collection, 1),
                                   top_first, True);
         when multi_point =>
            declare
               product : CT.Text := initialize_title ("MULTIPOINT(");
               first   : Boolean := True;
            begin
               for x in collection.points_set'Range loop
                  CT.SU.Append
                    (product, format_point
                       (collection.points_set (x), first));
                  first := False;
               end loop;
               return CT.USS (product) & pclose;
            end;
         when multi_line_string =>
            declare
               product : CT.Text := initialize_title ("MULTILINESTRING(");
               first   : Boolean := True;
            begin
               for ls in 1 .. collection.units loop
                  CT.SU.Append
                    (product, format_line_string
                       (retrieve_line (collection, ls), first));
                  first := False;
               end loop;
               return CT.USS (product) & pclose;
            end;
         when multi_polygon =>
            declare
               product : CT.Text := initialize_title ("MULTIPOLYGON(");
               first   : Boolean := True;
            begin
               for ls in 1 .. collection.units loop
                  CT.SU.Append
                    (product, format_polygon
                       (retrieve_polygon (collection, ls), first));
                  first := False;
               end loop;
               CT.SU.Append (product, pclose);
               return CT.USS (product);
            end;
         when heterogeneous =>
            declare
               product : CT.Text := initialize_title ("GEOMETRYCOLLECTION(");
               first   : Boolean := True;
               GM      : Geometry;
            begin
               for ls in 1 .. size_of_collection (collection) loop
                  GM := retrieve_subcollection (collection, ls);
                  CT.SU.Append (product, Well_Known_Text (GM, first));
                  first := False;
               end loop;
               return CT.USS (product) & pclose;
            end;
      end case;

   end Well_Known_Text;


end Spatial_Data;
