---
title: Fetch next row of data
---

<div class="leftside">
<pre class="code">
package AdaBase.Results.Sets is
   type DataRow        is tagged limited private;
   type DataRow_Access is access all DataRow;
end AdaBase.Results.Sets;
</pre>
<h3>DataRow_Access function<br/>
AdaBase.Statement.Base.[STMT].fetch_next ()</h3>
<p>
This function attempts to fetch the next row of data that was retrieved
prior as a result of a query.  If there is not another row of data
available, a null value for DataRow_Access is returned.  If more data
are available, access to the next row is returned and internally the database
client library advances the cursor.
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

   row : ARS.DataRow_Access;
   sql : constant String := "SELECT * FROM fruits WHERE color = 'orange'";

begin

   declare
   begin
      CON.connect_database;
   exception
      when others =>
         TIO.Put_Line ("database connect failed.");
         return;
   end;

   CON.STMT := CON.DR.query (sql);

   TIO.Put_Line (" Query successful: " & CON.STMT.successful'Img);
   TIO.Put_Line ("   Data Discarded: " & CON.STMT.data_discarded'Img);
   TIO.Put_Line ("Number of columns:" & CON.STMT.column_count'Img);
   TIO.Put_Line ("   Number of rows:" & CON.STMT.rows_returned'Img);

   TIO.Put_Line ("");
   for c in Natural range 1 .. CON.STMT.column_count loop
      TIO.Put_Line ("Column" & c'Img & ":");
      TIO.Put_Line ("   TABLE: " & CON.STMT.column_table (c));
      TIO.Put_Line ("    NAME: " & CON.STMT.column_name (c));
      TIO.Put_Line ("    TYPE: " & CON.STMT.column_native_type (c)'Img);
   end loop;

   TIO.Put_Line ("");
   loop
      row := CON.STMT.fetch_next;
      exit when ARS.complete (row);
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
<p>{{ page.supported_stmts }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.complete }}</li>
    <li>{{ page.query }}</li>
    <li>{{ page.stmt_successful }}</li>
    <li>{{ page.stmt_data_discarded }}</li>
    <li>{{ page.stmt_rows_returned }}</li>
    <li>{{ page.stmt_column_count }}</li>
    <li>{{ page.stmt_column_table }}</li>
    <li>{{ page.stmt_column_name }}</li>
    <li>{{ page.stmt_column_native_type }}</li>
    <li>{{ page.res_column }}</li>
    <li>{{ page.res_std_field }}</li>
    <li>{{ page.connect }}</li>
    <li>{{ page.disconnect }}</li>
  </ul>
</div>
