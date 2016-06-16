with AdaBase;
with Connect;
with CommonText;
with Ada.Text_IO;
with AdaBase.Results.Sets;
with Spatial_Data;

procedure Spatial4 is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ARS renames AdaBase.Results.Sets;
   package CT  renames CommonText;
   package SD  renames Spatial_Data;

begin

   CON.connect_database;

   declare
      use type SD.Geometric_Real;
      my_point   : SD.Geometry := SD.initialize_as_point ((3.2, 4.775));
      my_linestr : SD.Geometry := SD.initialize_as_line
                   (((-0.034, 14.993), (5.0, 6.0), (-3.0, 19.0), (0.0, -7.1000009)));
      wrk_poly   : SD.Geometric_Polygon := SD.start_polygon
                   (((35.0, 10.0), (45.0, 45.0), (15.0, 40.0), (10.0, 20.0), (35.0, 10.0)));
      my_polygon : SD.Geometry;
      my_mpoly   : SD.Geometry;
      my_mpoint  : SD.Geometry := SD.initialize_as_multi_point ((10.0, 10.0));
      my_mline   : SD.Geometry := SD.initialize_as_multi_line
                   (((5.0, 5.0), (0.0, 2.0), (-7.0, 13.0), (99.0, -1.0), (50.0, 50.0)));
      my_mixture : SD.Geometry := SD.initialize_as_collection (my_linestr);
   begin
      SD.append_inner_ring (wrk_poly, ((20.0, 30.0), (35.0, 35.0), (30.0, 20.0), (20.0, 30.0)));
      my_polygon := SD.initialize_as_polygon (wrk_poly);
      SD.augment_multi_point (my_mpoint, (100.0, 200.0));
      SD.augment_multi_point (my_mpoint, (-52.0, 250.0));
      SD.augment_multi_line  (my_mline, ((20.0, 10.0), (87.0, 88.0)));
      my_mpoly := SD.initialize_as_multi_polygon (wrk_poly);
      SD.augment_collection (my_mixture, my_polygon);
      SD.augment_collection (my_mixture, my_mpoint);
      SD.augment_collection (my_mixture, my_point);
      SD.augment_collection (my_mixture, my_mline);
      declare
         template : String := "INSERT INTO spatial_plus " &
            "(id, sp_point, sp_linestring, sp_polygon, sp_multi_point," &
            " sp_multi_line_string, sp_multi_polygon, sp_geo_collection)" &
            " VALUES (10, ST_GeomFromText (:pt, 4326)," &
            " ST_GeomFromText (:line, 4326)," &
            " ST_GeomFromText (:poly, 4326)," &
            " ST_GeomFromText(:mpoint, 4326)," &
            " ST_GeomFromText(:mline, 4326)," &
            " ST_GeomFromText(:mpoly, 4326)," &
            " ST_GeomFromText(:collset, 4326))";
         stmt : CON.Stmt_Type := CON.DR.prepare (template);
      begin
         stmt.assign ("pt",      SD.Well_Known_Text (my_point));
         stmt.assign ("line",    SD.Well_Known_Text (my_linestr));
         stmt.assign ("poly",    SD.Well_Known_Text (my_polygon));
         stmt.assign ("mpoint",  SD.Well_Known_Text (my_mpoint));
         stmt.assign ("mline",   SD.Well_Known_Text (my_mline));
         stmt.assign ("mpoly",   SD.Well_Known_Text (my_mpoly));
         stmt.assign ("collset", SD.Well_Known_Text (my_mixture));
         if not stmt.execute then
            TIO.Put_Line (stmt.last_driver_message);
            CON.DR.rollback;
            return;
         end if;
         declare
            row : ARS.Datarow;
            s2  : CON.Stmt_Type := CON.DR.query
                  ("SELECT * FROM spatial_plus WHERE id=10");
         begin
            loop
               row := s2.fetch_next;
               exit when row.data_exhausted;
               for x in Natural range 1 .. row.count loop
                  TIO.Put (s2.column_name (x) & " : ");
                  TIO.Put_Line (row.column (x).as_string);
               end loop;
            end loop;
         end;
         CON.DR.rollback;
      end;
   end;
   CON.DR.disconnect;

end Spatial4;
