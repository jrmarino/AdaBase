---
title: Iterate over an entire result set
---

<div class="leftside">
<h3>procedure<br/>
AdaBase.Statement.Base.[STMT].iterate (process : not null access procedure)</h3>
<p>
There are two versions of this procedure which is used to perform identical
actions using data from every row of a result set.  This first version requires
access to a procedure that takes no arguments.  The desired data must have been
already bound to variables in the same scope as this procedure.  Internally, the
iterate procedure calls <b>fetch_bound</b> for every row in the result set,
meaning the bound variables are updated prior to the indicate procedure being
executed.
</p>

<pre class="code">
package AdaBase.Results.Sets is
   type DataRow is tagged private;
end AdaBase.Results.Sets;
</pre>

<h3>procedure<br/>
AdaBase.Statement.Base.[STMT].iterate (process : not null access
procedure (row : ARS.DataRow))</h3>
<p>
The second version of <b>iterate</b> requires access to a procedure that has a
single argument, a DataRow type.  Internally, the iterate procedure
calls <b>fetch_next</b> for every row in the result set and passing the resultant
DataRow back to the process procedure.

</p>
<pre class="code">
with AdaBase.Results.Sets;
with Connect;
with CommonText;
with Ada.Text_IO;

procedure Iterate is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package AR  renames AdaBase.Results;
   package CT  renames CommonText;

   fruit    : aliased AR.textual;
   color    : aliased AR.textual;
   calories : aliased AR.nbyte2;
   dashes   : String (1 .. 50) := (others => '=');

   procedure list_fruit;
   procedure list_hockey_teams (row : AR.Sets.DataRow);

   procedure list_fruit
   is
      pair : String := CT.USS (fruit) & " (" & CT.USS (color) & ")";
      plen : Natural := pair'Length;
      zone : String (1 .. 20) := (others => ' ');
   begin
      zone (1 .. plen) := pair;
      TIO.Put_Line (zone & " contains" & calories'Img & " calories");
   end list_fruit;

   procedure list_hockey_teams (row : AR.Sets.DataRow) is
   begin
      TIO.Put_Line (row.column ("city").as_string & " " &
                    row.column ("mascot").as_string & " (" &
                    row.column ("abbreviation").as_string & ")");
   end list_hockey_teams;

begin

   begin
      CON.connect_database;
   exception
      when others =>
         TIO.Put_Line ("database connect failed.");
         return;
   end;

   declare
      stmt : CON.Stmt_Type := CON.DR.query_select
                  (tables    => "fruits",
                  columns    => "fruit, calories, color",
                  conditions => "calories >= 50",
                  order      => "calories");
   begin
      stmt.bind ("fruit",    fruit'Unchecked_Access);
      stmt.bind ("calories", calories'Unchecked_Access);
      stmt.bind ("color",    color'Unchecked_Access);

      TIO.Put_Line ("Demonstrate STMT.iterate (query + bound variables)");
      TIO.Put_Line (dashes);
      TIO.Put_Line ("List of fruit the contain at least 50 calories");
      TIO.Put_Line (dashes);
      stmt.iterate (process => list_fruit'Access);
   end;

   declare
      stmt : CON.Stmt_Type := CON.DR.prepare_select
                  (tables    => "nhl_teams",
                  columns    => "*",
                  conditions => "city LIKE :pattern",
                  order      => "city");
   begin
      TIO.Put_Line ("");
      TIO.Put_Line ("Demonstrate STMT.iterate (prepare + data access)");
      TIO.Put_Line (dashes);
      TIO.Put_Line ("List of NHL teams in locations starting with 'C'");
      TIO.Put_Line (dashes);

      stmt.assign ("pattern", "C%");
      if stmt.execute then
         stmt.iterate (process => list_hockey_teams'Access);
      end if;
   end;

   CON.DR.disconnect;

end Iterate;
</pre>
<p class="caption">Example code: testcases/iterate/iterate.adb</p>
<br/>
<pre class="output">
Demonstrate STMT.iterate (query + bound variables)
==================================================
List of fruit the contain at least 50 calories
==================================================
pineapple (yellow)   contains 50 calories
orange (orange)      contains 65 calories
apple (red)          contains 95 calories
grapefruit (yellow)  contains 100 calories
banana (yellow)      contains 107 calories
avocado (green)      contains 150 calories

Demonstrate STMT.iterate (prepare + data access)
==================================================
List of NHL teams in locations starting with 'C'
==================================================
Calgary Flames (CAL)
Carolina Hurricanes (CAR)
Chicago Blackhawks (CHI)
Colorado Avalanche (COL)
Columbus Blue Jackets (CLM)
</pre>
<p class="caption">Output using MySQL and SQLite Drivers</p>
<br/>
<p>{{ page.supported_stmts }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.query }}</li>
    <li>{{ page.query_select }}</li>
    <li>{{ page.prepare }}</li>
    <li>{{ page.prepare_select }}</li>
    <li>{{ page.stmt_bind }}</li>
    <li>{{ page.stmt_assign }}</li>
    <li>{{ page.stmt_execute }}</li>
    <li>{{ page.fetch_next }}</li>
    <li>{{ page.fetch_bound }}</li>
    <li>{{ page.res_column }}</li>
    <li>{{ page.res_std_field }}</li>
    <li>{{ page.connect }}</li>
    <li>{{ page.disconnect }}</li>
  </ul>
</div>
