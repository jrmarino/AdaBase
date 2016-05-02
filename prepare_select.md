---
title: Prepare_select (Abstract SQL)
---

<div class="leftside">
<h3>AdaBase.Statement.Base.[STMT]_access function<br/>
AdaBase.Driver.Base.[DB].prepare_select  (
                          distinct    : Boolean := False;
                          tables      : String;
                          columns     : String;
                          conditions  : String := "";
                          groupby     : String := "";
                          having      : String := "";
                          order       : String := "";
                          limit       : TraxID := 0;
                          offset      : TraxID := 0)</h3>

<p>This function assembles a driver-specific (SQL dialect-specific)
SELECT query based on which arguments are provided.  Generally the
<i>limit</i> and <i>offset</i> parameters are the ones that vary the
most between dialects.  The only required parameters are <i>tables</i>
and <i>columns</i>.
</p>
<p>Afterwards, the assembled SQL string is prepared and returns
access to the statement object that hasn't yet been executed.  The user
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

   declare
   begin
      CON.connect_database;
   exception
      when others =>
         TIO.Put_Line ("database connect failed.");
         return;
   end;

   declare
      max_calories : aliased AR.byte2 := 200;
      min_calories : constant AR.byte2 := 5;
      row : ARS.DataRow_Access;
   begin
      CON.STMT := CON.DR.prepare_select
        (tables     => "fruits",
         columns    => "*",
         conditions => "color = ? and calories > :mincal and calories < ?");

      CON.STMT.assign (1, "red");
      CON.STMT.assign ("mincal", min_calories);
      CON.STMT.assign (3, max_calories'Unchecked_Access);

      if CON.STMT.execute then
         TIO.Put_Line ("execute succeeded");
         for c in Natural range 1 .. CON.STMT.column_count loop
            TIO.Put_Line ("Column" & c'Img & " heading: " &
                          CON.STMT.column_name (c));
         end loop;
         TIO.Put_Line ("returned rows: " & CON.STMT.rows_returned'Img);
         loop
            exit when not CON.STMT.fetch_next (row);
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
   begin
      CON.STMT := CON.DR.prepare (sql);
      if CON.STMT.execute then
         TIO.Put_Line ("Inserted row " & CON.STMT.last_insert_id'Img);
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
2016-05-02 18:28:28    mysql :  Prepare Stmt : SELECT ALL * FROM fruits WHERE color = ? and calories > ?       and calories < ?
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
2016-05-02 18:28:28    mysql :    Disconnect : Disconnect From database
</pre>
<p class="caption">Output using MySQL Driver</p>
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
    <li>{{ page.res_std_field }}</li>
    <li>{{ page.prepare }}</li>
    <li>{{ page.query }}</li>
    <li>{{ page.query_select }}</li>
    <li>{{ standard_logger }}</li>
  </ul>
</div>
