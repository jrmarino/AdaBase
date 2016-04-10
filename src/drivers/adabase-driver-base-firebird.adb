--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Strings.Fixed;

package body AdaBase.Driver.Base.Firebird is

   package ASF renames Ada.Strings.Fixed;

   ---------------
   --  execute  --
   ---------------
   overriding
   function execute (driver : Firebird_Driver; sql : String)
                     return AD.AffectedRows
   is
      result : AD.AffectedRows := 0;
   begin
      return result;  --  TODO
   end execute;


   -------------------------
   --  query_clear_table  --
   -------------------------
   overriding
   procedure query_clear_table (driver : Firebird_Driver;
                                table  : String)
   is
      --  Firebird has no "truncate" commands
      sql : constant String := "DELETE FROM " & table;
      AR  : ACB.AD.AffectedRows;
   begin
      AR := driver.execute (sql => sql);
   end query_clear_table;


   ------------------------
   --  query_drop_table  --
   ------------------------
   overriding
   procedure query_drop_table        (driver      : Firebird_Driver;
                                      tables      : String;
                                      when_exists : Boolean := False;
                                      cascade     : Boolean := False)
   is
      sql : AD.textual;
      AR  : AD.AffectedRows;
   begin
      if ASF.Index (Source => tables, Pattern => ",", From => 1) /= 0 then
         driver.log_problem (category => AD.execution, message =>
           SUS ("Multiple tables detected -- Firebird can only drop " &
                "one table at a time : " & tables));
         return;
      end if;
      if cascade then
         driver.log_nominal (category => AD.execution, message =>
           SUS ("Note that requested CASCADE has no effect on Firebird"));
      end if;
      case when_exists is
         when False => sql := SUS ("DROP TABLE " & tables);
         when True  => sql := SUS ("EXECUTE BLOCK AS BEGIN " &
                       "if (exists(select 1 from rdb$relations where " &
                       "rdb$relation_name = '" & tables & "')) then " &
                       "execute statement 'drop table " & tables &
                       ";'; END");
      end case;
      AR := driver.execute (sql => USS (sql));
   end query_drop_table;


   ----------------------
   --  last_insert_id  --
   ----------------------
   overriding
   function last_insert_id  (driver : Firebird_Driver) return AD.TraxID
   is
   begin
      return 0;  --  TODO
   end last_insert_id;


   ----------------------
   --  last_sql_state  --
   ----------------------
   overriding
   function last_sql_state (driver : Firebird_Driver) return AD.TSqlState
   is
   begin
      return AD.stateless;  --  TODO
   end last_sql_state;


   -----------------------
   --  last_error_info  --
   -----------------------
   overriding
   function last_error_info (driver : Firebird_Driver) return AD.Error_Info
   is
      result : AD.Error_Info;
   begin
      return result;   -- TODO
   end last_error_info;


   -------------
   --  query  --
   -------------
   overriding
   function query (driver : Firebird_Driver; sql : String)
                   return  AS.Base'Class
   is
      result : AS.Base;
   begin
      return result;  -- TODO
   end query;


   ------------------------------------------------------------------------
   --  PRIVATE ROUTINES NOT COVERED BY INTERFACES                        --
   ------------------------------------------------------------------------


end AdaBase.Driver.Base.Firebird;
