---
title: Fetch next row of data
---

<div class="leftside">
<pre class="code">
package AdaBase.Results.Sets is
   type Datarow is tagged private;
   Empty_Datarow : constant Datarow;
end AdaBase.Results.Sets;
</pre>
<h3>AdaBase.Results.Sets.Datarow function<br/>
AdaBase.Statement.Base.[STMT].fetch_next ()</h3>
<p>
This function attempts to fetch the next row of data from a query's result
set.  If there is not another row of data available, the Empty_Datarow
constant is returned, and the row's data_exhausted method returns False.
If more data are available, the function returns a populated Datarow 
while the client library advances the cursor.
</p>
<pre class="code">
with AdaBase;
with Connect;
with CommonText;
with Ada.Text_IO;
with AdaBase.Results.Sets;

procedure DS_Fetch is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ARS renames AdaBase.Results.Sets;
   package CT  renames CommonText;

begin

   CON.connect_database;

   declare
      sql  : constant String := "SELECT * FROM fruits WHERE color = 'orange'";
      stmt : CON.Stmt_Type := CON.DR.query (sql);
      row  : ARS.Datarow;
   begin
      TIO.Put_Line (" Query successful: " & stmt.successful'Img);
      TIO.Put_Line ("   Data Discarded: " & stmt.data_discarded'Img);
      TIO.Put_Line ("Number of columns:" & stmt.column_count'Img);
      TIO.Put_Line ("   Number of rows:" & stmt.rows_returned'Img);

      TIO.Put_Line ("");
      for c in Natural range 1 .. stmt.column_count loop
         TIO.Put_Line ("Column" & c'Img & ":");
         TIO.Put_Line ("   TABLE: " & stmt.column_table (c));
         TIO.Put_Line ("    NAME: " & stmt.column_name (c));
         TIO.Put_Line ("    TYPE: " & stmt.column_native_type (c)'Img);
      end loop;

      TIO.Put_Line ("");
      loop
         row := stmt.fetch_next;
         exit when row.data_exhausted;
         TIO.Put (CT.zeropad (Natural (row.column (1).as_byte2), 2) & " ");
         declare
            fruit : String := row.column ("fruit").as_string;
            frlen : Natural := fruit'Length;
            rest  : String (1 .. 12 - frlen) := (others => ' ');
         begin
            TIO.Put (rest & fruit);
         end;
         TIO.Put (" (" & row.column ("color").as_string & ") calories =");
         TIO.Put_Line (row.column (4).as_byte2'Img);
      end loop;
   end;

   CON.DR.disconnect;

end DS_Fetch;
</pre>
<p class="caption">Example code: testcases/ds_fetch/ds_fetch.adb</p>
<br/>
<pre class="output">
 Query successful: TRUE
   Data Discarded: FALSE
Number of columns: 4
   Number of rows: 7

Column 1:
   TABLE: fruits
    NAME: id
    TYPE: FT_BYTE4
Column 2:
   TABLE: fruits
    NAME: fruit
    TYPE: FT_TEXTUAL
Column 3:
   TABLE: fruits
    NAME: color
    TYPE: FT_TEXTUAL
Column 4:
   TABLE: fruits
    NAME: calories
    TYPE: FT_NBYTE2

02      apricot (orange) calories = 30
07   clementine (orange) calories = 24
13        mango (orange) calories = 40
15   cantaloupe (orange) calories = 25
16    nectarine (orange) calories = 25
18       orange (orange) calories = 65
27    tangerine (orange) calories = 26
</pre>
<p class="caption">Output using MySQL Driver</p>
<br/>
<pre class="output">
 Query successful: TRUE
   Data Discarded: FALSE
Number of columns: 4
   Number of rows: 0

Column 1:
   TABLE: fruits
    NAME: id
    TYPE: FT_BYTE8
Column 2:
   TABLE: fruits
    NAME: fruit
    TYPE: FT_TEXTUAL
Column 3:
   TABLE: fruits
    NAME: color
    TYPE: FT_TEXTUAL
Column 4:
   TABLE: fruits
    NAME: calories
    TYPE: FT_BYTE8

02      apricot (orange) calories = 30
07   clementine (orange) calories = 24
13        mango (orange) calories = 40
15   cantaloupe (orange) calories = 25
16    nectarine (orange) calories = 25
18       orange (orange) calories = 65
27    tangerine (orange) calories = 26
</pre>
<p class="caption">Output using SQLite Driver</p>
<br/>
<pre class="output">
 Query successful: TRUE
   Data Discarded: FALSE
Number of columns: 4
   Number of rows: 7

Column 1:
   TABLE: fruits
    NAME: id
    TYPE: FT_BYTE4
Column 2:
   TABLE: fruits
    NAME: fruit
    TYPE: FT_TEXTUAL
Column 3:
   TABLE: fruits
    NAME: color
    TYPE: FT_TEXTUAL
Column 4:
   TABLE: fruits
    NAME: calories
    TYPE: FT_BYTE4

02      apricot (orange) calories = 30
07   clementine (orange) calories = 24
13        mango (orange) calories = 40
15   cantaloupe (orange) calories = 25
16    nectarine (orange) calories = 25
18       orange (orange) calories = 65
27    tangerine (orange) calories = 26
</pre>
<p class="caption">Output using PostgreSQL Driver</p>
<br/>
<p>{{ page.supported_stmts }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.query }}</li>
    <li>{{ page.stmt_successful }}</li>
    <li>{{ page.stmt_data_discarded }}</li>
    <li>{{ page.stmt_rows_returned }}</li>
    <li>{{ page.stmt_column_count }}</li>
    <li>{{ page.stmt_column_table }}</li>
    <li>{{ page.stmt_column_name }}</li>
    <li>{{ page.stmt_column_native_type }}</li>
    <li>{{ page.stmt_iterate }}</li>
    <li>{{ page.fetch_bound }}</li>
    <li>{{ page.res_data_exhausted }}</li>
    <li>{{ page.res_column }}</li>
    <li>{{ page.res_std_field }}</li>
    <li>{{ page.connect }}</li>
    <li>{{ page.disconnect }}</li>
  </ul>
</div>
