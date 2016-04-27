---
title: Fetch next set of data from a stored procedure
---

<div class="leftside">
<h3>Procedure<br/>
AdaBase.Statement.Base.[STMT].fetch_next_set (data_present : out Boolean;
                                              data_fetched : out Boolean)</h3>
<p>
This procedure advances to the next result set on a multiple result set
statement handle.  This is possible to obtain through the use of stored
procedures which can execute several SELECT queries serially.
</p>
<p>
It is possible for the database client to report that a result set is present,
but in fact there's no data contained within.  This can occur on non-SELECT
queries (e.g. an INSERT statement).  MySQL also produces an extra empty result
set at the end of every stored procedure execution for the purposes of passing
status data back.
</p>
<p>
Thus, the program should iterate on <i>data_present</i> variable, looping as
long as it remains True.  However, it should only try to read the result when
the <i>data_fetched</i> variable is True.
</p>
<pre class="code">
with AdaBase;
with Connect;
with Ada.Text_IO;
with AdaBase.Results.Sets;

procedure Stored_Procs is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ARS renames AdaBase.Results.Sets;

   procedure dump_result;
   procedure dump_result
   is
      function pad (S : String) return String;
      function pad (S : String) return String
      is
         field : String (1 .. 15) := (others => ' ');
         len   : Natural := S'Length;
      begin
         field (1 .. len) := S;
         return field;
      end pad;

      row     : ARS.DataRow_Access;
      numcols : constant Natural := CON.STMT.column_count;
   begin
      for c in Natural range 1 .. numcols loop
         TIO.Put (pad (CON.STMT.column_name (c)));
      end loop;
      TIO.Put_Line ("");
      for c in Natural range 1 .. numcols loop
         TIO.Put ("============== ");
      end loop;
      TIO.Put_Line ("");
      loop
         exit when not CON.STMT.fetch_next (row);
         for c in Natural range 1 .. numcols loop
            TIO.Put (pad (row.column (c).as_string));
         end loop;
         TIO.Put_Line ("");
      end loop;
      TIO.Put_Line ("");
   end dump_result;

   sql         : constant String := "CALL multiple_rowsets";
   set_fetched : Boolean := True;
   set_present : Boolean;

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
   loop
      if set_fetched then
         dump_result;
      end if;
      CON.STMT.fetch_next_set (set_present, set_fetched);
      exit when not set_present;
   end loop;

   CON.DR.disconnect;

end Stored_Procs;
</pre>
<p class="caption">testcases/stored_procs/stored_procs.adb</p>
<br/>
<pre class="output">
id             fruit          color          calories       
============== ============== ============== ============== 
3              avocado        green          150            
4              banana         yellow         107            
10             grapefruit     yellow         100            
1              apple          red            95             
18             orange         orange         65             
21             pineapple      yellow         50             
20             pear           yellow         45             
13             mango          orange         40             

fruit          
============== 
apple          
raspberry      
strawberry     
tomato         
cherry tomato  

team_id        abbreviation   
============== ============== 
29             PHO            
30             SJ             
31             WIN            
</pre>
<p class="caption">Output using MySQL Driver</p>
<br/>
<p>{{ page.supported_stmts }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.query }}</li>
    <li>{{ page.stmt_column_count }}</li>
    <li>{{ page.stmt_column_name }}</li>
    <li>{{ page.res_column }}</li>
    <li>{{ page.res_std_field }}</li>
    <li>{{ page.connect }}</li>
    <li>{{ page.disconnect }}</li>
  </ul>
</div>
