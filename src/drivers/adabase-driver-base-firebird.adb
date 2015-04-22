--
--  Copyright (c) 2015 John Marino <draco@marino.st>
--
--  Permission to use, copy, modify, and distribute this software for any
--  purpose with or without fee is hereby granted, provided that the above
--  copyright notice and this permission notice appear in all copies.
--
--  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
--  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
--  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
--  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
--  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
--  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
--  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--

package body AdaBase.Driver.Base.Firebird is

   overriding
   procedure query_drop_table        (driver      : Firebird_Driver;
                                      tables      : AD.textual;
                                      when_exists : Boolean := False;
                                      cascade     : Boolean := False)
   is
      sql : AD.textual;
      AR  : AD.AffectedRows;
   begin
      if AD.SU.Index (Source => tables, Pattern => ",") /= 0 then
         driver.log_problem (category => AD.execution, message =>
           SUS ("Multiple tables detected -- Firebird can only drop " &
                "one table at a time : " & USS (tables)));
         return;
      end if;
      if cascade then
         driver.log_nominal (category => AD.execution, message =>
           SUS ("Note that requested CASCADE has no effect on Firebird"));
      end if;
      case when_exists is
         when False => sql := SUS ("DROP TABLE " & USS (tables));
         when True  => sql := SUS ("EXECUTE BLOCK AS BEGIN " &
                       "if (exists(select 1 from rdb$relations where " &
                       "rdb$relation_name = '" & USS (tables) & "')) then " &
                       "execute statement 'drop table " & USS (tables) &
                       ";'; END");
      end case;
      AR := driver.execute (sql => sql);
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
   function query (driver : Firebird_Driver; sql : AD.textual)
                   return  AS.Base'Class
   is
      result : AS.Base;
   begin
      return result;  -- TODO
   end query;


   ------------------------------------------------------------------------
   --  PRIVATE ROUTINES NOT COVERED BY INTERFACES                        --
   ------------------------------------------------------------------------


   ------------------
   --  log_nominal --
   ------------------
--     procedure log_nominal (driver    : Firebird_Driver;
--                            category  : AD.LogCategory;
--                            message   : AD.textual)
--     is
--     begin
--           logger.log_nominal (driver   => AD.firebird,
--                               category => category,
--                               message  => message);
--     end log_nominal;


   ------------------
   --  log_problem --
   ------------------
--     procedure log_problem
--       (driver     : Firebird_Driver;
--        category   : AD.LogCategory;
--        message    : AD.textual;
--        pull_codes : Boolean := False;
--        break      : Boolean := False)
--     is
--        error_msg  : AD.textual      := AD.blank;
--        error_code : AD.DriverCodes  := 0;
--        sqlstate   : AD.TSqlState    := AD.stateless;
--     begin
--        if pull_codes then
--           error_msg  := driver.connection.driverMessage;
--           error_code := driver.connection.driverCode;
--           sqlstate   := driver.connection.SqlState;
--        end if;
--
--        logger.log_problem (driver     => AD.firebird,
--                            category   => category,
--                            message    => message,
--                            error_msg  => error_msg,
--                            error_code => error_code,
--                            sqlstate   => sqlstate,
--                            break      => break);
--     end log_problem;



end AdaBase.Driver.Base.Firebird;
