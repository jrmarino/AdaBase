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

   my_point   : Geometry := initialize_as_point ((18.2, -3.4));
   my_line    : Geometry := initialize_as_line (((4.0, 2.23), (0.25, 5.1)));
   my_linestr : Geometry := initialize_as_line_string 
                      (((0.5, 2.0), (11.0, 4.4), (12.0, 8.1)));
   my_polygon : Geometry := initialize_as_polygon
                      (((0.5, 2.0), (11.0, 4.4), (12.0, 8.1), (0.005, 2.0)));

   my_circle  : Geometry := initialize_as_circle (((2.0, 1.0), 4.5));
   my_infline : Geometry := initialize_as_infinite_line (((0.0, 0.0), (2.0, 2.0)));

begin   
   print (my_point,   "SINGLE POINT");
   print (my_line,    "SINGLE LINE");
   print (my_linestr, "SINGLE LINE STRING (THREE POINT)");
   print (my_polygon, "SINGLE POLYGON (3 SIDES)");
   print (my_circle,  "SINGLE CIRCLE (not legal for MySQL or WKT)");
   print (my_infline, "INFINITE LINE (converted to regular line on MySQL or WKT)");

   --  convert my point to to a multi-point collection
   
   declare
      pt1 : Geometric_point := (9.2, 4.773);
      pt2 : Geometric_point := (-7.01, -4.9234);
      pt3 : Geometric_point := (4.5, 6.0);
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

      append_polygon_hole  (my_polygon, ((1.0, 2.0), (3.2, 4.5), (8.8, 7.7), (1.0, 2.0)));
      append_point (my_polygon, pt1);
      append_point (my_polygon, pt2);
      print (my_polygon, "MIXED COLLECTION #3");
   end;
   
end Spatial1;
