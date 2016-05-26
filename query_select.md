---
title: Query_select (Abstract SQL)
---

<div class="leftside">
<pre class="code">
package AdaBase is
   type Null_Priority is (native, nulls_first, nulls_last);
end AdaBase;
</pre>
<h3>AdaBase.Statement.Base.[STMT] function<br/>
AdaBase.Driver.Base.[DB].query_select  (
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
by all SQL drivers. The only required parameters are <i>tables</i>
and <i>columns</i>.  Of course, the <b>query</b> function accepts literal
SQL commands and can be used alternatively at the risk of portability.</p>
<p>This command creates direct statements that can retrieve results once,
and specifically it does not produce prepared statements.
</p>
<pre class="code">
with AdaBase;
with Connect;
with CommonText;
with Ada.Text_IO;
with AdaBase.Results.Sets;

procedure Query_Select is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ARS renames AdaBase.Results.Sets;
   package AR  renames AdaBase.Results;
   package CT  renames CommonText;

begin

   CON.connect_database;

   CON.DR.set_trait_column_case (AdaBase.upper_case);

   declare
      stmt : CON.Stmt_Type := CON.DR.query_select
               (tables    => "nhl_schedule as S " &
                             "JOIN nhl_teams T1 ON S.home_team = T1.team_id " &
                             "JOIN nhl_teams T2 ON S.away_team = T2.team_id",
               columns    => "S.event_code, " &
                             "T1.city as home_city, " &
                             "T1.mascot as home_mascot, " &
                             "T1.abbreviation as home_short, " &
                             "S.home_score, " &
                             "T2.city as away_city, " &
                             "T2.mascot as away_mascot, " &
                             "T2.abbreviation as away_short, " &
                             "S.away_score",
               conditions => "S.yyyswww < 1085011",
               order      => "S.yyyswww ASC",
               limit      => 10,
               offset     => 20);

   begin
      if not stmt.successful then
         TIO.Put_Line ("  Driver message: " & stmt.last_driver_message);
         TIO.Put_Line ("     Driver code: " & stmt.last_driver_code'Img);
         TIO.Put_Line ("       SQL State: " & stmt.last_sql_state);
      else
         for c in Natural range 1 .. stmt.column_count loop
            TIO.Put_Line ("Column" & c'Img & " heading: " &
                          stmt.column_name (c));
         end loop;
         TIO.Put_Line ("");
      end if;

      --  Demonstrate bind/fetch_bound
      declare
         event_code : aliased AR.NByte2;
         home_town, home_mascot  : aliased AR.Textual;
         away_town, away_mascot  : aliased AR.Textual;
         home_score, away_score  : aliased AR.NByte1;
      begin
         stmt.bind (1, event_code'Unchecked_Access);
         stmt.bind ("HOME_CITY", home_town'Unchecked_Access);
         stmt.bind ("AWAY_CITY", away_town'Unchecked_Access);
         stmt.bind (3, home_mascot'Unchecked_Access);
         stmt.bind ("AWAY_MASCOT", away_mascot'Unchecked_Access);
         stmt.bind ("HOME_SCORE", home_score'Unchecked_Access);
         stmt.bind ("AWAY_SCORE", away_score'Unchecked_Access);

         loop
            exit when not stmt.fetch_bound;

            TIO.Put ("In event" & event_code'Img & ", the " &
                     CT.USS (away_town) & " " & CT.USS (away_mascot) &
                     " visited the " &
                     CT.USS (home_town) & " " & CT.USS (home_mascot) &
                     " and ");
            if Integer (away_score) > Integer (home_score) then
               TIO.Put ("won");
            elsif Integer (away_score) < Integer (home_score) then
               TIO.Put ("lost");
            else
               TIO.Put ("tied");
            end if;
            TIO.Put_Line (away_score'Img & " to" & home_score'Img);
         end loop;
         TIO.Put_Line ("");
      end;
   end;

   declare
      --  demonstrate fetch_all
      stmt : CON.Stmt_Type := CON.DR.query_select
               (tables    => "fruits",
               columns    => "fruit, calories, color",
               conditions => "calories > 50",
               order      => "calories",
               limit      => 10);

      rowset : ARS.Datarow_Set := stmt.fetch_all;
   begin
      for row in Natural range 1 .. rowset'Length loop
         TIO.Put_Line (rowset (row).column (1).as_string & ":" &
                       rowset (row).column ("calories").as_nbyte2'Img &
                       " calories, " & rowset (row).column (3).as_string);
      end loop;
   end;

   CON.DR.disconnect;

end Query_Select;
</pre>
<p class="caption">Example code: testcases/query_select/query_select.adb</p>
<br/>
<pre class="output">
Column 1 heading: EVENT_CODE
Column 2 heading: HOME_CITY
Column 3 heading: HOME_MASCOT
Column 4 heading: HOME_SHORT
Column 5 heading: HOME_SCORE
Column 6 heading: AWAY_CITY
Column 7 heading: AWAY_MASCOT
Column 8 heading: AWAY_SHORT
Column 9 heading: AWAY_SCORE

In event 10084, the Dallas Stars visited the Nashville Predators and lost 1 to 3
In event 10090, the Boston Bruins visited the Minnesota Wild and lost 3 to 4
In event 10097, the Vancouver Canucks visited the Calgary Flames and won 5 to 4
In event 10101, the Columbus Blue Jackets visited the Phoenix Coyotes and lost 1 to 3
In event 10105, the Los Angeles Kings visited the San Jose Sharks and lost 1 to 3
In event 10125, the Colorado Avalanche visited the Edmunton Oilers and lost 2 to 3
In event 10134, the Phoenix Coyotes visited the Anaheim Ducks and won 4 to 2
In event 10137, the San Jose Sharks visited the Los Angeles Kings and won 1 to 0
In event 10150, the St. Louis Blues visited the Toronto Maple Leafs and won 5 to 4
In event 10155, the Buffalo Sabres visited the New York Islanders and won 7 to 1

orange: 65 calories, orange
apple: 95 calories, red
grapefruit: 100 calories, yellow
banana: 107 calories, yellow
avocado: 150 calories, green
</pre>
<p class="caption">Output using the MySQL driver</p>
<br/>
<p>{{ page.supported_drivers }}</p>
<p>{{ page.supported_stmts }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.stmt_successful }}</li>
    <li>{{ page.stmt_discard_rest }}</li>
    <li>{{ page.fetch_next }}</li>
    <li>{{ page.fetch_bound }}</li>
    <li>{{ page.fetch_all }}</li>
    <li>{{ page.stmt_bind }}</li>
    <li>{{ page.res_std_field }}</li>
    <li>{{ page.query }}</li>
    <li>{{ page.prepare }}</li>
    <li>{{ page.prepare_select }}</li>
  </ul>
</div>
