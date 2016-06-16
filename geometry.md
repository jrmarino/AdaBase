---
title: Geometry support
---

<div class="leftside">
<h3>Spatial and Geographic Object Support</h3>
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
<pre class="code">
package Spatial_Data is

   type Collection_Type is (unset,
                            single_point,
                            single_line_string,
                            single_polygon,
                            multi_point,
                            multi_line_string,
                            multi_polygon,
                            heterogeneous);

   subtype Geo_Points is Positive range 1 .. 2 ** 20;
   subtype Geo_Units  is Natural  range 0 .. 2 ** 12;

   type Geometry is private;

   type Geometric_Real is digits 18;

   type Geometric_Point is
      record
         X : Geometric_Real;
         Y : Geometric_Real;
      end record;

   type Geometric_Point_set is array (Positive range <>) of Geometric_Point;
   subtype Geometric_Ring        is Geometric_Point_set;
   subtype Geometric_Line_String is Geometric_Point_set;
   type Geometric_Polygon (rings  : Geo_Units := Geo_Units'First;
                           points : Geo_Points := Geo_Points'First) is private;

   Origin_Point   : constant Geometric_Point := (0.0, 0.0);


   --------------------------------
   --  Initialization functions  --
   --------------------------------
   function start_polygon               (outer_ring : Geometric_Ring)
                                         return Geometric_Polygon;
   procedure append_inner_ring          (polygon    : in out Geometric_Polygon;
                                         inner_ring : Geometric_Ring);

   function initialize_as_point         (point : Geometric_Point)
                                         return Geometry;
   function initialize_as_multi_point   (point : Geometric_Point)
                                         return Geometry;

   function initialize_as_line          (line_string : Geometric_Line_String)
                                         return Geometry;
   function initialize_as_multi_line    (line_string : Geometric_Line_String)
                                         return Geometry;

   function initialize_as_polygon       (polygon : Geometric_Polygon)
                                         return Geometry;
   function initialize_as_multi_polygon (polygon : Geometric_Polygon)
                                         return Geometry;

   function initialize_as_collection    (anything : Geometry) return Geometry;


   -----------------------------------
   --  Build collections functions  --
   -----------------------------------
   procedure augment_multi_point   (collection : in out Geometry;
                                    point      : Geometric_Point);

   procedure augment_multi_line    (collection : in out Geometry;
                                    line       : Geometric_Line_String);

   procedure augment_multi_polygon (collection : in out Geometry;
                                    polygon    : Geometric_Polygon);

   procedure augment_collection    (collection : in out Geometry;
                                    anything   : Geometry);


   ---------------------------
   --  Retrieval functions  --
   ---------------------------
   function type_of_collection     (collection : Geometry)
                                    return Collection_Type;
   function size_of_collection     (collection : Geometry)
                                    return Positive;
   function collection_item_type   (collection : Geometry;
                                    index      : Positive := 1)
                                    return Collection_Type;

   function retrieve_subcollection (collection : Geometry;
                                    index : Positive := 1)
                                    return Geometry;

   function retrieve_point         (collection : Geometry;
                                    index : Positive := 1)
                                    return Geometric_Point;
   function retrieve_line          (collection : Geometry;
                                    index : Positive := 1)
                                    return Geometric_Line_String;

   function retrieve_polygon       (collection : Geometry;
                                    index      : Positive := 1)
                                    return Geometric_Polygon;
   function number_of_rings        (polygon : Geometric_Polygon)
                                    return Natural;
   function retrieve_ring          (polygon : Geometric_Polygon;
                                    ring_index : Positive)
                                    return Geometric_Ring;

   ---------------------------
   --  Text Representation  --
   ---------------------------
   function mysql_text      (collection : Geometry;
                             top_first  : Boolean := True) return String;
   function Well_Known_Text (collection : Geometry;
                             top_first  : Boolean := True) return String;

end Spatial_Data;
</pre>

<h3>To retrieve and manipulate geometry data</h3>
<p>
After fetching a row, use the standard field conversion "as_geometry" to
return a private <b>geometry</b> type.  To retrieve the Well-Known-Text
representation of the geometry, either use the standard field conversion
"as_string", or pass the geometry to the <i>Well_Known_Text</i> function,
both of which return standard strings.
</p>
<p>
You may already know what type of geometry is in the field, but it is possible
the database table is constructed to allow any shape.  To determine the
<b>Collection_Type</b> of the geometry, pass it to the <i>type_of_collection</i>
function.  To determine how many components are contained in the geometry, use
the <i>size_of_collection</i> function.  The single geometries (point, line string,
polygon) always return 1 for this function.  If the <b>Collection_Type</b> is
heterogeneous, it may be desirable to know what kind of geometry each collection
element is.  For this information, use the <i>collection_item_type</i> function.
</p>
<p>
Every geometry can be broken down to it's most basic element, the 2-dimensional point.
While points consist of X, Y coordinates of <b>Geometric_Real</b> 18-digit type,
conversions will round the value to 16 significant digits.  To retrieve a point from
the <b>single_point</b> or <b>multi_point</b> geometries, use the <i>retrieve_point</i>
function.
</p>
<p>
Similarly, a line string is just an array of points.  To obtain a line string (type
<b>Geometric_Line_String</b>, use the <i>retrieve_line</i> function on
<b>single_line_string</b> and <b>multi_line_string</b> geometry types.
</p>
<p>
The polygons are more complex.  Structurally, they are similar to line strings, but the
first and last point must be identical (so the shape is closed) and their structural
elements are known as rings.  The first one is the outer ring.  A polygon may have zero
to an indefinitely number of "inner" rings also known as holes.  There are certain rules
related to holes (e.g. they can't touch, must be enclosed, etc.), so a polygon is basically
an array of an array of points.  One extracts polygons as the type <b>Geometric_Polygon</b>
using the <i>retrieve_polygon</i> by passing that function a geometry of type
<b>single_polygon</b> or <b>multi_polygon</b>.  The polygon is a private type, so to
determine how many rings it has (1 or more), use the <i>number_of_rings</i> function.
To obtain a ring of type <b>Geometric_Ring</b>, which is similar to a line string, use
the <i>retrieve_ring</i> function.
</p>
<p>
The geometry collections are geometries of type <b>heterogeneous</b>.  After determining
how many elements are contained within, use the <i>retrieve_subcollection</i> function
to peel off the collection element as a new geometry.  For example, if the third element
of a collection is a polygon, then use the retrieval function to extract the polygon
geometry which can be further broken down with <i>retrieve_polygon</i> function.  On
PostGIS, the PostgreSQL GIS extensions, the extracted geometry may well be another
heterogenous collection which has to have its own subcollections extracted as well.
</p>
<h3>How to create geometries</h3>
<p>
For various reasons, such as desiring Well-Known-Text as a product for the purposes
of supporting an insert or update query, it may be desirable to create geometries which
is generally accomplished by building it up.
</p>
<h3>How to create a single-point geometry</h3>
<p>
This is easy.  Just pass a variable of type <b>Geometric_Point</b> to the
<i>initialize_as_point</i>function, and a geometry containing that point will be
returned.
</p>
<h3>How to create a multi-point geometry</h3>
<p>
This is done exactly the same way as a single point, but instead the
<i>initialize_as_multi_point</i> function is used.  This geometry is only required
to hold a single point, so the difference between a single-point and a multi-point
geometry is in classification only when a single point is involved.
</p>
<h3>How to create a single-line-string geometry</h3>
<p>
First create a <b>Geometric_Line_String</b> variable (an array of points) or
just pass an array of points directly to the <i>initialize_as_line</i> function,
and the proper geometry is returned.
</p>
<h3>How to create a multiple-line-string geometry</h3>
<p>
The process is identical to the single-line-string except that the
<i>initialize_as_multi_line</i> function is used instead.  This geometry is
only required to hold a single string.
</p>
<h3>How to create polygon geometry</h3>
<p>
A polygon can only be constructed one ring at a time, and the geometry has to
constructed in two steps.  The first step is to create the private
<b>Geometric_Polygon</b> type by passing a <b>Geometric_Ring</b> (an array of
points) to the <i>start_polygon</i> function.  If the polygon has holes, these
need to be defined using the append_inner_ring procedure using the existing
polygon and another ring.  When the polygon is fully constructed, a geometry
can be created using the <i>initialize_as_polygon</i> or
<i>initialize_as_multi_polygon</i> functions.
</p>
<h3>How to add points to multi-point geometry</h3>
<p>
The <i>augment_multi_point</i> procedure is used to append additional points
to existing multi-point geometries.
</p>
<h3>How to add lines to multi-line-string geometry</h3>
<p>
The <i>augment_multi_line</i> procedure is used to append additional line
strings to existing multi-line-string geometries.
</p>
<h3>How to add lines to multi-polygon geometry</h3>
<p>
The <i>augment_multi_polygon</i> procedure is used to append additional
polygons to existing multi-polygon geometries.
</p>
<h3>How to create heterogenous geometries</h3>
<p>
To construct geometry collections, you must first obtain a geometry through
one of the previously mentioned methods, and then initialize a new
collection with it using the <i>initialize_as_collection</i>.  Additional
geometries are added by using the <i>augment_collection</i> procedure as
often as necessary.
</p>
<br/>
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

Element 5 type : MULTI_LINE_STRING
Element 5 size : 2
Element 5 wkt  : MULTILINESTRING((10 10,20 20,10 40),(40 40,30 30,40 20,30 10))

Element 6 type : MULTI_POINT
Element 6 size : 4
Element 6 wkt  : MULTIPOINT(11.2 42.4,45.1 30,20.25 20,30.9 10.117)

Element 7 type : SINGLE_POLYGON
Element 7 size : 1
Element 7 wkt  : POLYGON((30 10,40 40,20 40,10 20,30 10))
</pre>
<p class="caption">Output using the MySQL or PostgreSQL driver</p>
<br/>
<pre class="code">
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
</pre>
<p class="caption">Example code: testcases/spatial4/spatial4.adb</p>
<br/>
<pre class="output">
id : 10
sp_point : POINT(3.2 4.775)
sp_linestring : LINESTRING(-0.034 14.993,5 6,-3 19,0 -7.1000009)
sp_polygon : POLYGON((35 10,45 45,15 40,10 20,35 10),(20 30,35 35,30 20,20 30))
sp_multi_point : MULTIPOINT(10 10,100 200,-52 250)
sp_multi_line_string : MULTILINESTRING((5 5,0 2,-7 13,99 -1,50 50),(20 10,87 88))
sp_multi_polygon : MULTIPOLYGON(((35 10,45 45,15 40,10 20,35 10),(20 30,35 35,30 20,20 30)))
sp_geo_collection : GEOMETRYCOLLECTION(LINESTRING(-0.034 14.993,5 6,-3 19,0 -7.1000009),POLYGON((35 10,45 45,15 40,10 20,35 10),(20 30,35 35,30 20,20 30)),MULTIPOINT(10 10,100 200,-52 250),POINT(3.2 4.775),MULTILINESTRING((5 5,0 2,-7 13,99 -1,50 50),(20 10,87 88)))
sp_geometry : 
sp_outer_ring : 
sp_coll2 : 
sp_geometry2 : 
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
