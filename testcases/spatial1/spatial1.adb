with Ada.Text_IO;
with Spatial_Data; Use Spatial_Data;

procedure Spatial1 is

   package TIO renames Ada.Text_IO;
   
   procedure print (shape : Geometry; label : String);
   procedure print (shape : Geometry; label : String) is
   begin
      TIO.Put_Line ("===== " & label & " =====");
      TIO.Put_Line ("MySQL: " & mysql_text (shape));
      TIO.Put_Line ("  WKT: " & shape.Well_Known_Text); -- (shape));
      TIO.Put_Line ("");
   end print;

   my_point   : Geometry := initialize_as_point ((18.2, -3.4));
   my_line    : Geometry := initialize_as_line (((4.0, 2.23), (0.25, 5.1)));
   my_linestr : Geometry := initialize_as_line_string 
                      (((0.5, 2.0), (11.0, 4.4), (12.0, 8.1)));
   my_polygon : Geometry := initialize_as_polygon
                      (((0.5, 2.0), (11.0, 4.4), (12.0, 8.1), (0.005, 2.0)));

begin   
   print (my_point,   "SINGLE POINT");
   print (my_line,    "SINGLE LINE");
   print (my_linestr, "SINGLE LINE STRING (THREE POINT)");
   print (my_polygon, "SINGLE POLYGON (3 SIDES)");

end Spatial1;
