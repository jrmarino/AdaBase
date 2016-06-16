---
title: Geometry support
---

<div class="leftside">
<p>
AdaBase provides natural support for Spatial Data using <i>OpenGIS</i>
concepts.  Support is specifically provided for MySQL (Spatial Data extensions)
and PostgreSQL (PostGIS extension).
</p>
<p>
No specially care has to be taken to retrieve geometry data.  If AdaBase
detects a geometry type, it will convert the internal data to the "Well Known Binary"
(WKB) format and store that internally.  If a string conversion is requested, AdaBase
converts the WKB into "Well Known Text" format which is suitable to be used in other
queries.  More importantly, primitive geometric shapes such as points, line strings,
and polygons can be extracted and the point coordinates, which are 16-digit real
numbers, are directly available if necessary.  Homogenuous geometry collections that
contain an indefinite number of similar elements such as multi-point, multi-line-string,
and multi-polygon are also supported.  Finally "geometry collections" which may contain
a heterogenous mixture of any geometries are supported.  In the case of PostGIS, a
collection may contain other collections through many recursive levels -- a collection
may contain up to 23 other collections.  On MySQL, this is not supported, and the
geometry collection type may only contain 6 types (three single types and the analogous
multiple types).
</p>

<h3>AdaBase.Results.Sets.Datarow function<br/>
AdaBase.Statement.Base.[STMT].fetch_next ()</h3>
<p>
TBD
</p>
<pre class="code">
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
</pre>
<p class="caption">Example code: testcases/spatial3/spatial3.adb</p>
<br/>
<pre class="output">
Demonstrate direct geometry retrieval and manipulation

Column Name : sp_point
Geo subtype : SINGLE_POINT
WKT value   : POINT(4.5 -2.323)
X= 4.50000000000000000E+00 (sp_point)
Y=-2.32300000000000000E+00

Column Name : sp_linestring
Geo subtype : SINGLE_LINE_STRING
WKT value   : LINESTRING(-0.2 14.7,21.33 20,0 0)
X=-2.00000000000000000E-01 (sp_linestring 1)
Y= 1.47000000000000000E+01
X= 2.13300000000000000E+01 (sp_linestring 2)
Y= 2.00000000000000000E+01
X= 0.00000000000000000E+00 (sp_linestring 3)
Y= 0.00000000000000000E+00

Column Name : sp_polygon
Geo subtype : SINGLE_POLYGON
WKT value   : POLYGON((35 10,45 45,15 40,10 20,35 10),(20 30,35 35,30 20,20 30))
Ring# 1
X= 3.50000000000000000E+01 (point 1)
Y= 1.00000000000000000E+01
X= 4.50000000000000000E+01 (point 2)
Y= 4.50000000000000000E+01
X= 1.50000000000000000E+01 (point 3)
Y= 4.00000000000000000E+01
X= 1.00000000000000000E+01 (point 4)
Y= 2.00000000000000000E+01
X= 3.50000000000000000E+01 (point 5)
Y= 1.00000000000000000E+01
Ring# 2
X= 2.00000000000000000E+01 (point 1)
Y= 3.00000000000000000E+01
X= 3.50000000000000000E+01 (point 2)
Y= 3.50000000000000000E+01
X= 3.00000000000000000E+01 (point 3)
Y= 2.00000000000000000E+01
X= 2.00000000000000000E+01 (point 4)
Y= 3.00000000000000000E+01

Column Name : sp_multi_point
Geo subtype : MULTI_POINT
WKT value   : MULTIPOINT(-0.7 0.7,1.2 -1.2,2.2 2.995,-9.99 -9.00001)
X=-7.00000000000000000E-01 (Multipoint# 1)
Y= 7.00000000000000000E-01
X= 1.20000000000000000E+00 (Multipoint# 2)
Y=-1.20000000000000000E+00
X= 2.20000000000000000E+00 (Multipoint# 3)
Y= 2.99500000000000000E+00
X=-9.99000000000000000E+00 (Multipoint# 4)
Y=-9.00001000000000000E+00

Column Name : sp_multi_line_string
Geo subtype : MULTI_LINE_STRING
WKT value   : MULTILINESTRING((10 10,20 20,10 40),(40 40,30 30,40 20,30 10))
line# 1: LINESTRING(10 10,20 20,10 40)
line# 2: LINESTRING(40 40,30 30,40 20,30 10)

Column Name : sp_multi_polygon
Geo subtype : MULTI_POLYGON
WKT value   : MULTIPOLYGON(((40 40,20 45,45 30,40 40)),((20 35,10 30,10 10,30 5,45 20,20 35),(30 20,20 15,20 25,30 20)))
polygon# 1: POLYGON((40 40,20 45,45 30,40 40))
polygon# 2: POLYGON((20 35,10 30,10 10,30 5,45 20,20 35),(30 20,20 15,20 25,30 20))
Inner ring 1 of polygon 2 : POLYGON((30 20,20 15,20 25,30 20))

Column Name : sp_geo_collection
Geo subtype : HETEROGENEOUS
Number of elements in collection : 7

Element 1 type : SINGLE_POINT
Element 1 size : 1
Element 1 wkt  : POINT(4 6)

Element 2 type : SINGLE_LINE_STRING
Element 2 size : 1
Element 2 wkt  : LINESTRING(4 6,7 10)

Element 3 type : SINGLE_POLYGON
Element 3 size : 1
Element 3 wkt  : POLYGON((30 10,40 40,20 40,10 20,30 10))

Element 4 type : MULTI_POLYGON
Element 4 size : 2
Element 4 wkt  : MULTIPOLYGON(((30 20,45 40,10 40,30 20)),((15 5,40 10,10 20,5 10,15 5)))

Element 5 type : MULTI_POLYGON
Element 5 size : 2
Element 5 wkt  : MULTILINESTRING((10 10,20 20,10 40),(40 40,30 30,40 20,30 10))

Element 6 type : MULTI_LINE_STRING
Element 6 size : 4
Element 6 wkt  : MULTIPOINT(11.2 42.4,45.1 30,20.25 20,30.9 10.117)

Element 7 type : MULTI_LINE_STRING
Element 7 size : 1
Element 7 wkt  : POLYGON((30 10,40 40,20 40,10 20,30 10))
</pre>

<p class="caption">Output using the MySQL or PostgreSQL driver</p>
<br/>
<p>[DB] is "MySQL.MySQL_Driver" or "PostgreSQL.PostgreSQL_Driver"</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.query }}</li>
    <li>{{ page.stmt_successful }}</li>
    <li>{{ page.stmt_column_name }}</li>
    <li>{{ page.res_column }}</li>
    <li>{{ page.res_std_field }}</li>
    <li>{{ page.connect }}</li>
    <li>{{ page.disconnect }}</li>
  </ul>
</div>
