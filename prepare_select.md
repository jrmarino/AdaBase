---
title: Prepare_select (Abstract SQL)
---

<div class="leftside">
<pre class="code">
package AdaBase is
   type Null_Priority is (native, nulls_first, nulls_last);
end AdaBase;
</pre>
<h3>AdaBase.Statement.Base.[STMT] function<br/>
AdaBase.Driver.Base.[DB].prepare_select  (
                          distinct    : Boolean := False;
                          tables      : String;
                          columns     : String;
                          conditions  : String := "";
                          groupby     : String := "";
                          having      : String := "";
                          order       : String := "";
                          null_sort   : Null_Priority := native;
                          limit       : Trax_ID := 0;
                          offset      : Trax_ID := 0)</h3>

<p>This function assembles a driver-specific (SQL dialect-specific)
SELECT query based on which arguments are provided.  Generally the
<i>limit</i>, <i>offset</i> and <i>null_sort</i> parameters are the ones
that vary the most between dialects, with the latter not being supported
by all SQL drivers.
The only required parameters are <i>tables</i> and <i>columns</i>.
</p>
<p>Afterwards, the assembled SQL string is prepared and returns
the statement object that hasn't yet been executed.  The user
has the option to bind values and variables to the templated SQL prior
to the query execution.  This is allowed when the SQL has one or more
named (:<i>name</i>) or question mark (?) parameters in the string,
and both types can be present as the named parameters are internally
converted the question marks in the same order.  The named parameters
can only be used in one location though.
</p>
<p>
Once the statement has been prepared, it can be executed repeatedly after
updating the parameter bindings before each execution.  THis is a big
performance improvement over executing similar statements directly
and individually.  Another major benefit is that SQL injection attacks
are prevented by using parameter bindings instead of passing text strings.
</p>

<pre class="code">
with AdaBase;
with Connect;
with Ada.Text_IO;
with CommonText;
with AdaBase.Results.Sets;
with AdaBase.Logger.Facility;

procedure Prep_Stmt is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package CT  renames CommonText;
   package AR  renames AdaBase.Results;
   package ARS renames AdaBase.Results.Sets;
   package ALF renames AdaBase.Logger.Facility;

begin

   CON.DR.command_standard_logger (device => ALF.screen, action => ALF.attach);

   CON.connect_database;

   declare
      max_calories : aliased AR.Byte2 := 200;
      min_calories : constant AR.Byte2 := 5;
      row          : ARS.Datarow;
      stmt : CON.Stmt_Type := CON.DR.prepare_select
        (tables     => "fruits",
         columns    => "*",
         conditions => "color = ? and calories > :mincal and calories < ?");
   begin
      stmt.assign (1, "red");
      stmt.assign ("mincal", min_calories);
      stmt.assign (3, max_calories'Unchecked_Access);

      if stmt.execute then
         TIO.Put_Line ("execute succeeded");
         for c in Natural range 1 .. stmt.column_count loop
            TIO.Put_Line ("Column" & c'Img & " heading: " &
                          stmt.column_name (c));
         end loop;
         TIO.Put_Line ("returned rows: " & stmt.rows_returned'Img);
         loop
            row := stmt.fetch_next;
            exit when row.data_exhausted;
            TIO.Put_Line (row.column (2).as_string & " (" &
                          row.column ("color").as_string & ") " &
                          row.column ("calories").as_string  & " calories");
         end loop;
      else
         TIO.Put_Line ("execute failed");
      end if;
   end;

   declare
      sql : String := "INSERT INTO fruits (fruit, color, calories) " &
                      "VALUES ('potato','tan', 77)";
      stmt : CON.Stmt_Type := CON.DR.prepare (sql);
   begin
      if stmt.execute then
         TIO.Put_Line ("Inserted row " & stmt.last_insert_id'Img);
         TIO.Put_Line ("Affected rows: " & stmt.rows_affected'Img);
      end if;
      CON.DR.rollback;
   end;

   CON.DR.disconnect;

end Prep_Stmt;
</pre>
<p class="caption">Example code: testcases/prep_stmt/prep_stmt.adb</p>
<br/>
<pre class="output">
2016-05-02 18:28:28    mysql :       Connect : Connection to adabase_examples database succeeded.
2016-05-02 18:28:28    mysql :  Prepare Stmt : SELECT ALL * FROM fruits WHERE color = ? and calories > ? and calories < ?
2016-05-02 18:28:28    mysql :  Execute Stmt : Exec with 3 bound parameters
execute succeeded
Column 1 heading: id
Column 2 heading: fruit
Column 3 heading: color
Column 4 heading: calories
returned rows:  2
apple (red) 95 calories
tomato (red) 9 calories
2016-05-02 18:28:28    mysql :  Prepare Stmt : INSERT INTO fruits (fruit, color, calories) VALUES ('potato','tan', 77)
2016-05-02 18:28:28    mysql :  Execute Stmt : Exec without bound parameters
Inserted row  36
Affected rows:  1
2016-05-02 18:28:28    mysql :    Disconnect : Disconnect From database
</pre>
<p class="caption">Output using the MySQL driver</p>
<br/>
<pre class="output">
2016-05-12 14:27:02   sqlite :       Connect : Connection to file:///home/marino/adabase.sqlite database succeeded.
2016-05-12 14:27:02   sqlite :  Prepare Stmt : SELECT ALL * FROM fruits WHERE color = ? and calories > ? and calories < ?
2016-05-12 14:27:02   sqlite :  Execute Stmt : Exec with 3 bound parameters
execute succeeded
Column 1 heading: id
Column 2 heading: fruit
Column 3 heading: color
Column 4 heading: calories
returned rows:  0
apple (red) 95 calories
tomato (red) 9 calories
2016-05-12 14:27:02   sqlite :  Prepare Stmt : INSERT INTO fruits (fruit, color, calories) VALUES ('potato','tan', 77)
2016-05-12 14:27:02   sqlite :  Execute Stmt : Exec without bound parameters
Inserted row  38
Affected rows:  1
2016-05-12 14:27:02   sqlite :    Disconnect : Disconnect From database
</pre>
<p class="caption">Output using the SQLite driver</p>
<br/>
<p>{{ page.supported_drivers }}</p>
<p>{{ page.supported_stmts }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.stmt_execute }}</li>
    <li>{{ page.stmt_successful }}</li>
    <li>{{ page.stmt_discard_rest }}</li>
    <li>{{ page.stmt_assign }}</li>
    <li>{{ page.stmt_last_insert_id }}</li>
    <li>{{ page.stmt_rows_returned }}</li>
    <li>{{ page.stmt_affected }}</li>
    <li>{{ page.res_std_field }}</li>
    <li>{{ page.prepare }}</li>
    <li>{{ page.query }}</li>
    <li>{{ page.query_select }}</li>
    <li>{{ standard_logger }}</li>
  </ul>
</div>
