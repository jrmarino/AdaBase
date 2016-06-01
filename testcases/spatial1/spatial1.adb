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
   magic_polygon : constant Geometric_Polygon := ((0.5, 2.0), (11.0, 4.4), (12.0, 8.1), (0.005, 2.0));

   my_point   : Geometry := initialize_as_point (magic_point);
   my_line    : Geometry := initialize_as_line (((4.0, 2.23), (0.25, 5.1)));
   my_linestr : Geometry := initialize_as_line_string (magic_linestr);
   my_polygon : Geometry := initialize_as_polygon (magic_polygon);

   my_circle  : Geometry := initialize_as_circle (((2.0, 1.0), 4.5));
   my_infline : Geometry := initialize_as_infinite_line (((0.0, 0.0), (2.0, 2.0)));

begin   

   --  critical checklist
   --  =========================================================
   --  single point                                           61
   --  single line (converts to line string)                  62
   --  single infinite line (!)                               66
   --  single line_string                                     63
   --  single polygon                                         64
   --  single polygon with 1 hole                          91-94
   --  single polygon with 2 holes                         95-98
   --  single circle (!)                                      67
   --  point + point = multipoint                          76-78
   --  point + line_string = collection                  106-108
   --  point + polygon = collection                      110-112
   --  point + polygon with 1 hole = 2 item collection   114-115
   --  point + polygon hole = EXCEPTION                  117-126
   --  line_string + line_string = multiline               80-81
   --  line_string + point + point = collection          128-130
   --  line_string + polygon = collection                132-135
   --  line_string + polygon + hole = 2 item collection  136-137
   --  polygon + point = 2 item collection               139-142
   --  polygon + line_string = 2 item collection         143-146
   --  polygon + hole + point + point = 3 item collection    147-151
   --  polygon + hole + polygon = 2 item multipolygon        153-156
   --  polygon + hole + polygon + point = 3 item collection  158-159
   --  polygon + polygon + hole = 2 item multipolygon        161-165
   --  polygon + polygon + hole + line_string = 3 item col.  166-167
   --  polygon + hole + polygon + hole = 2 item multipolygon 169-174
   --  polygon + hole + point = 2 item collection            175-179
   --  polygon + hole + line_string = 2 item collection

   print (my_point,   "SINGLE POINT");
   print (my_line,    "SINGLE LINE");
   print (my_linestr, "SINGLE LINE STRING (THREE POINT)");
   print (my_polygon, "SINGLE POLYGON (3 SIDES)");
   print (my_circle,  "SINGLE CIRCLE (not legal for MySQL or WKT)");
   print (my_infline, "INFINITE LINE (converted to regular line on MySQL or WKT)");
   
   declare
      pt1 : Geometric_Point := (9.2, 4.773);
      pt2 : Geometric_Point := (-7.01, -4.9234);
      pt3 : Geometric_Point := (4.5, 6.0);
      altpoly : Geometric_Polygon := ((1.0, 2.0), (3.2, 4.5), (8.8, 7.7), (1.0, 2.0));
      polyhole1 : Geometry;
      polyhole2 : Geometry;
   begin
      append_point (my_point, pt1);
      append_point (my_point, pt2);
      print (my_point, "MULTIPOINT COLLECTION");

      append_line_string (my_linestr, ((pt3, pt1, pt2)));
      print (my_linestr, "MULTILINESTRING COLLECTION");      

      append_point (my_linestr, pt1);
      append_point (my_linestr, pt2);
      print (my_linestr, "MIXED COLLECTION #1");

      append_point (my_line, pt1);
      append_point (my_line, pt2);
      print (my_line, "MIXED COLLECTION #2");

      append_polygon_hole  (my_polygon, (altpoly));
      polyhole1 := my_polygon;
      print (polyhole1, "STILL SINGLE POLYGON #1");

      polyhole2 := polyhole1;
      append_polygon_hole (polyhole2, ((13.5, 15.35), (98.1, 11.7), (-13.75, 0.0004), (13.5, 15.35)));
      print (polyhole2, "STILL SINGLE POLYGON #2");

      append_polygon (my_polygon, ((5.0, 6.0), (1.4, 2.2), (18.1, 24.0), (5.0, 6.0)));
      print (my_polygon, "POLYGON COLLECTION #1");

      append_point (my_polygon, pt1);
      append_point (my_polygon, pt2);
      print (my_polygon, "MIXED COLLECTION #3");

      my_point := initialize_as_point (magic_point);
      append_line_string (my_point, magic_linestr);
      print (my_point, "MIXED COLLECTION #4");

      my_point := initialize_as_point (magic_point);
      append_polygon (my_point, magic_polygon);
      print (my_point, "MIXED COLLECTION #5");

      append_polygon_hole (my_point, altpoly);
      print (my_point, "MIXED COLLECTION #5 ENHANCED");

      my_point := initialize_as_point (magic_point);
      begin
         append_polygon_hole (my_point, altpoly);
      print (my_point, "MIXED COLLECTION #5 NEVER SEE THIS");
      exception
         when others =>
            TIO.Put_Line ("You can't add a polygon hole before adding " & 
                          "a polygon");
            TIO.Put_Line ("");
      end;

      my_linestr := initialize_as_line_string (magic_linestr);
      append_point (my_linestr, pt1);
      print (my_linestr, "MIXED COLLECTION #6");

      my_linestr := initialize_as_line_string (magic_linestr);
      append_polygon (my_linestr, magic_polygon);
      print (my_linestr, "MIXED COLLECTION #7");

      append_polygon_hole (my_linestr, altpoly);
      print (my_linestr, "MIXED COLLECTION #7 ENHANCED");

      my_polygon := initialize_as_polygon (magic_polygon);
      append_point (my_polygon, pt2);
      print (my_polygon, "MIXED COLLECTION #8");

      my_polygon := initialize_as_polygon (magic_polygon);
      append_line_string (my_polygon, magic_linestr);
      print (my_polygon, "MIXED COLLECTION #9");

      my_polygon := initialize_as_polygon (magic_polygon);
      append_polygon_hole (my_polygon, altpoly);
      append_point (my_polygon, pt1);
      append_point (my_polygon, pt2);
      print (my_polygon, "MIXED COLLECTION #10");

      my_polygon := initialize_as_polygon (magic_polygon);
      append_polygon_hole (my_polygon, altpoly);
      append_polygon (my_polygon, ((5.0, 6.0), (1.4, 2.2), (18.1, 24.0), (5.0, 6.0)));
      print (my_polygon, "POLYGON COLLECTION #2");

      append_point (my_polygon, pt2);
      print (my_polygon, "MIXED COLLECTION #11");

      my_polygon := initialize_as_polygon (magic_polygon);
      append_polygon (my_polygon, ((5.0, 6.0), (1.4, 2.2), (18.1, 24.0), (5.0, 6.0)));
      append_polygon_hole (my_polygon, altpoly);
      print (my_polygon, "POLYGON COLLECTION #3");

      append_line_string (my_polygon, magic_linestr);
      print (my_polygon, "MIXED COLLECTION #12");

      my_polygon := initialize_as_polygon (magic_polygon);
      append_polygon_hole (my_polygon, altpoly);
      append_polygon (my_polygon, ((5.0, 6.0), (1.4, 2.2), (18.1, 24.0), (5.0, 6.0)));
      append_polygon_hole (my_polygon, ((13.5, 15.35), (98.1, 11.7), (-13.75, 0.0004), (13.5, 15.35)));
      print (my_polygon, "POLYGON COLLECTION #4");
      
      my_polygon := initialize_as_polygon (magic_polygon);
      append_polygon_hole (my_polygon, altpoly);
      append_point (my_polygon, pt3);
      print (my_polygon, "MIXED COLLECTION #13");

      my_polygon := initialize_as_polygon (magic_polygon);
      append_polygon_hole (my_polygon, altpoly);
      append_line_string (my_polygon, magic_linestr);
      print (my_polygon, "MIXED COLLECTION #14");
   end;
   
end Spatial1;
