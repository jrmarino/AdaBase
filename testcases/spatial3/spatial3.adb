with AdaBase;
with Connect;
with CommonText;
with Ada.Text_IO;
with AdaBase.Results.Sets;
with Spatial_Data;

procedure Spatial3 is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ARS renames AdaBase.Results.Sets;
   package CT  renames CommonText;
   package SD  renames Spatial_Data;

   procedure print_wkt   (GM : SD.Geometry; cn, wkt : String);
   procedure print_point (point : SD.Geometric_Point; label : String);

   procedure print_wkt (GM : SD.Geometry; cn, wkt : String) is
   begin
      TIO.Put_Line ("");
      TIO.Put_Line ("Column Name : " & cn);
      TIO.Put_Line ("Geo subtype : " & SD.type_of_collection (GM)'Img);
      TIO.Put_Line ("WKT value   : " & wkt);
   end print_wkt;

   procedure print_point (point : SD.Geometric_Point; label : String) is
   begin
      TIO.Put_Line ("X=" & point.X'Img & " (" & label & ")");
      TIO.Put_Line ("Y=" & point.Y'Img);
   end print_point;

begin

   CON.connect_database;

   declare
      sql  : constant String := "SELECT * FROM spatial_plus";
      stmt : CON.Stmt_Type := CON.DR.query (sql);
      row  : ARS.Datarow := stmt.fetch_next;

      PT   : constant String := "sp_point";
      LN   : constant String := "sp_linestring";
      PG   : constant String := "sp_polygon";
      MP   : constant String := "sp_multi_point";
      ML   : constant String := "sp_multi_line_string";
      MPG  : constant String := "sp_multi_polygon";
      GC   : constant String := "sp_geo_collection";
   begin

      TIO.Put_Line ("Demonstrate direct geometry retrieval and manipulation");

      --  Point
      print_wkt (row.column (PT).as_geometry, PT, row.column (PT).as_string);
      print_point (SD.retrieve_point (row.column (PT).as_geometry), PT);

      --  Line
      print_wkt (row.column (LN).as_geometry, LN, row.column (LN).as_string);
      declare
         LNS : SD.Geometric_Line_String :=
            SD.retrieve_line (row.column (LN).as_geometry);
      begin
         for component in LNS'Range loop
            print_point (LNS (component), LN & component'Img);
         end loop;
      end;

      --  Polygon
      print_wkt (row.column (PG).as_geometry, PG, row.column (PG).as_string);
      declare
         PG1 : SD.Geometric_Polygon :=
            SD.retrieve_polygon (row.column (PG).as_geometry);
         ring_count : Natural := SD.number_of_rings (PG1);
      begin
         for Ring_ID in 1 .. ring_count loop
            declare
               RG : SD.Geometric_Ring := SD.retrieve_ring (PG1, Ring_ID);
               SZ : Natural := RG'Length;
            begin
               TIO.Put_Line ("Ring#" & Ring_ID'Img);
               for component in 1 .. SZ loop
                  print_point (RG (component), "point" & component'Img);
               end loop;
            end;
         end loop;
      end;

      --  Multi-Point
      declare
         GM  : SD.Geometry := row.column (MP).as_geometry;
         SZ  : Natural := SD.size_of_collection (GM);
      begin
         print_wkt (GM, MP, row.column (MP).as_string);
         for component in 1 .. SZ loop
            print_point (SD.retrieve_point (GM, component),
                         "Multipoint#" & component'Img);
         end loop;
      end;

      --  Multi-Line
      declare
         GM  : SD.Geometry := row.column (ML).as_geometry;
         SZ  : Natural := SD.size_of_collection (GM);
      begin
         print_wkt (GM, ML, row.column (ML).as_string);
         for component in 1 .. SZ loop
            declare
               --  extract line string type
               SLS : SD.Geometric_Line_String :=
                     SD.retrieve_line (GM, component);
               --  convert to a simple geometry type
               NGM : SD.Geometry := SD.initialize_as_line (SLS);
            begin
               TIO.Put_Line ("line#" & component'Img & ": " &
                  SD.Well_Known_Text (NGM));
            end;
         end loop;
      end;

      --  Multi-Polygon
      declare
         GM  : SD.Geometry := row.column (MPG).as_geometry;
         SZ  : Natural := SD.size_of_collection (GM);
      begin
         print_wkt (GM, MPG, row.column (MPG).as_string);
         for component in 1 .. SZ loop
            declare
               --  extract single polygon
               SPG : SD.Geometric_Polygon :=
                     SD.retrieve_polygon (GM, component);
               --  convert to a simple geometry type
               NGM : SD.Geometry := SD.initialize_as_polygon (SPG);
               num_rings : Natural := SD.number_of_rings (SPG);
            begin
               TIO.Put_Line ("polygon#" & component'Img & ": " &
                  SD.Well_Known_Text (NGM));
               for ring in 2 .. num_rings loop
                  declare
                     IR : SD.Geometric_Ring := SD.retrieve_ring (SPG, ring);
                     newpoly : SD.Geometric_Polygon := SD.start_polygon (IR);
                  begin
                     TIO.Put_Line ("Inner ring" & Integer (ring - 1)'Img &
                        " of polygon" & component'Img & " : " &
                        SD.Well_Known_Text
                           (SD.initialize_as_polygon (newpoly)));
                  end;
               end loop;
            end;
         end loop;
      end;

      --  Geometry Collection
      declare
         GM  : SD.Geometry := row.column (GC).as_geometry;
         SZ  : Natural := SD.size_of_collection (GM);
      begin
         TIO.Put_Line ("");
         TIO.Put_Line ("Column Name : " & GC);
         TIO.Put_Line ("Geo subtype : " & SD.type_of_collection (GM)'Img);
         TIO.Put_Line ("Number of elements in collection :" & SZ'Img);
         for component in 1 .. SZ loop
            declare
               NGM : SD.Geometry := SD.retrieve_subcollection (GM, component);
               SZC : Natural := SD.size_of_collection (NGM);
            begin
               TIO.Put_Line ("");
               TIO.Put_Line ("Element" & component'Img & " type : " &
                  SD.collection_item_type (GM, component)'Img);
               TIO.Put_Line ("Element" & component'Img & " size : " &
                  CT.int2str (SZC));
               TIO.Put_Line ("Element" & component'Img & " wkt  : " &
                  SD.Well_Known_Text (NGM));
            end;
         end loop;
      end;
   end;
   CON.DR.disconnect;

end Spatial3;
