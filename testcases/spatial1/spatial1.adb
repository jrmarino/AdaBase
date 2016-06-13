with Ada.Text_IO;
with Spatial_Data; Use Spatial_Data;

procedure Spatial1 is

   package TIO renames Ada.Text_IO;

   procedure print (shape : Geometry; label : String);
   procedure print (shape : Geometry; label : String) is
   begin
      TIO.Put_Line ("===== " & label & " =====");
      TIO.Put_Line ("MySQL: " & mysql_text (shape));
      TIO.Put_Line ("  WKT: " & Well_Known_Text (shape)); -- (shape));
      TIO.Put_Line ("");
   end print;

   magic_point   : constant Geometric_Point := (18.2, -3.4);
   magic_linestr : constant Geometric_Line_String := ((0.5, 2.0), (11.0, 4.4), (12.0, 8.1));
   magic_polygon : constant Geometric_Polygon := start_polygon (((0.5, 2.0), (11.0, 4.4), (12.0, 8.1), (0.005, 2.0)));

   my_point   : Geometry := initialize_as_point (magic_point);
   my_linestr : Geometry := initialize_as_line (magic_linestr);
   my_polygon : Geometry := initialize_as_polygon (magic_polygon);

   pt1 : Geometric_Point := (9.2, 4.773);
   pt2 : Geometric_Point := (-7.01, -4.9234);
   pt3 : Geometric_Point := (4.5, 6.0);
   altpoly : Geometric_Ring := ((1.0, 2.0), (3.2, 4.5), (8.8, 7.7), (1.0, 2.0));
   polyhole : Geometric_Polygon;

begin

   --  critical checklist
   --  =========================================================
   --  single point                                           59
   --  single line_string                                     60
   --  single polygon                                         61
   --  single polygon with 1 hole                          71-74
   --  single polygon with 2 holes                         66-70
   --  point + point = multipoint                          72-77
   --  point + line_string = collection                   94-100
   --  point + point(s) = collection                     102-108
   --  point + polygon = collection                      110-115
   --  point + polygon with 2 holex = 2 item collection  117-122
   --  line_string + line_string = multiline               87-92
   --  line_string + point + point = collection          124-130
   --  line_string + polygon = collection                132-137
   --  line_string + polygon w/holes = 2 item collection 139-144
   --  polygon + point = 2 item collection                   146-151
   --  polygon + line_string = 2 item collection             153-158
   --  polygon + hole + point + point = 3 item collection    160-167
   --  polygon + hole + polygon + point = 3 item collection  169-176
   --  polygon + polygon + hole = 2 item multipolygon        178-182
   --  polygon + polygon + hole + line_string = 3 item col.  178-185
   --  polygon + hole + polygon + hole = 2 item multipolygon 187-196
   --  polygon + hole + point = 2 item collection            198-204
   --  polygon + hole + line_string = 2 item collection      206-212

   print (my_point,   "SINGLE POINT");
   print (my_linestr, "SINGLE LINE STRING (THREE POINT)");
   print (my_polygon, "SINGLE POLYGON (3 SIDES)");

   polyhole := magic_polygon;
   append_inner_ring (polyhole, altpoly);

   declare
      shape : Geometry := initialize_as_polygon (polyhole);
   begin
      print (shape, "STILL SINGLE POLYGON #1");
   end;

   append_inner_ring (polyhole, ((13.5, 15.35), (98.1, 11.7), (-13.75, 0.0004), (13.5, 15.35)));
   declare
      shape : Geometry := initialize_as_polygon (polyhole);
   begin
      print (shape, "STILL SINGLE POLYGON #2");
   end;

   declare
      MP : Geometry := initialize_as_multi_point (magic_point);
   begin
      augment_multi_point (MP, pt1);
      augment_multi_point (MP, pt2);
      print (MP, "MULTIPOINT COLLECTION");
   end;

   declare
      MLS : Geometry := initialize_as_multi_line (magic_linestr);
   begin
      augment_multi_line (MLS, ((pt3, pt1, pt2)));
      print (MLS, "MULTILINESTRING COLLECTION");
   end;

   declare
      shape : Geometry :=
        initialize_as_collection (initialize_as_point (pt1));
   begin
      augment_collection (shape, my_linestr);
      print (shape, "MIXED COLLECTION #1 (PT + LINE)");
   end;

   declare
      shape : Geometry := initialize_as_collection (my_point);
   begin
      augment_collection (shape, initialize_as_point (pt1));
      augment_collection (shape, initialize_as_point (pt2));
      print (shape, "MIXED COLLECTION #2 (ALL POINTS)");
   end;

   declare
      shape : Geometry := initialize_as_collection (my_point);
   begin
      augment_collection (shape, my_polygon);
      print (shape, "MIXED COLLECTION #3 (PT + POLY)");
   end;

   declare
      shape : Geometry := initialize_as_collection (my_point);
   begin
      augment_collection (shape, initialize_as_polygon (polyhole));
      print (shape, "MIXED COLLECTION #4 (PT + CMPLX POLY)");
   end;

   declare
      shape : Geometry := initialize_as_collection (my_linestr);
   begin
      augment_collection (shape, initialize_as_point (pt1));
      augment_collection (shape, initialize_as_point (pt2));
      print (shape, "MIXED COLLECTION #5 (LINE + 2 PT)");
   end;

   declare
      shape : Geometry := initialize_as_collection (my_linestr);
   begin
      augment_collection (shape, my_polygon);
      print (shape, "MIXED COLLECTION #6 (LINE + POLY)");
   end;

   declare
      shape : Geometry := initialize_as_collection (my_linestr);
   begin
      augment_collection (shape, initialize_as_polygon (polyhole));
      print (shape, "MIXED COLLECTION #6 ENHANCED (LINE + CMPLX POLY)");
   end;

   declare
      shape : Geometry := initialize_as_collection (my_polygon);
   begin
      augment_collection (shape, initialize_as_point (pt2));
      print (shape, "MIXED COLLECTION #7 (POLY + PT)");
   end;

   declare
      shape : Geometry := initialize_as_collection (my_polygon);
   begin
      augment_collection (shape, my_linestr);
      print (shape, "MIXED COLLECTION #7 (POLY + LINE)");
   end;

   declare
      shape : Geometry :=
        initialize_as_collection (initialize_as_polygon (polyhole));
   begin
      augment_collection (shape, initialize_as_point (pt1));
      augment_collection (shape, initialize_as_point (pt2));
      print (shape, "MIXED COLLECTION #8 (CMPLX POLY + 2 PT)");
   end;

   declare
      shape : Geometry :=
        initialize_as_collection (initialize_as_polygon (polyhole));
   begin
      augment_collection (shape, my_polygon);
      augment_collection (shape, initialize_as_point (pt2));
      print (shape, "MIXED COLLECTION #9 (CMPLX POLY + POLY + PT)");
   end;

   declare
      shape : Geometry := initialize_as_collection (my_polygon);
   begin
      augment_collection (shape, initialize_as_polygon (polyhole));
      print (shape, "MIXED COLLECTION #10 (POLY + CMPLX POLY)");
      augment_collection (shape, my_linestr);
      print (shape, "MIXED COLLECTION #11 (POLY + CMPLX POLY + LINE)");
   end;

   declare
      shape : Geometry :=
        initialize_as_collection (initialize_as_polygon (polyhole));
      new_poly : Geometric_Polygon;
   begin
      new_poly := start_polygon (((5.0, 6.0), (1.4, 2.2), (18.1, 24.0), (5.0, 6.0)));
      append_inner_ring (new_poly, ((5.0, 6.0), (1.4, 2.2), (18.1, 24.0), (5.0, 6.0)));
      augment_collection (shape, initialize_as_polygon (new_poly));
      print (shape, "MIXED COLLECTION #12 (2 CMPLX POLY)");
   end;

   declare
      shape : Geometry :=
        initialize_as_collection (initialize_as_polygon (polyhole));
   begin
      augment_collection (shape, initialize_as_point (pt3));
      print (shape, "MIXED COLLECTION #13 (CMPLX POLY + PT)");
   end;

   declare
      shape : Geometry :=
        initialize_as_collection (initialize_as_polygon (polyhole));
   begin
      augment_collection (shape, my_linestr);
      print (shape, "MIXED COLLECTION #13 (CMPLX POLY + Line)");
   end;

   -----------------------------------
   --  Additional collection tests  --
   -----------------------------------

   --  Have a collection inside a collection

   declare
      shape  : Geometry := initialize_as_collection (my_point);
      shape2 : geometry :=
        initialize_as_collection (initialize_as_point (pt3));
   begin
      augment_collection (shape2, my_linestr);
      augment_collection (shape, shape2);
--      print (shape, "COLLECTION THAT CONTAINS ANOTHER COLLECTION");
   end;

end Spatial1;
