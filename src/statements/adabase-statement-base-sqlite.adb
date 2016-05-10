--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body AdaBase.Statement.Base.SQLite is


   ---------------------
   --  num_set_items  --
   ---------------------
   function num_set_items (nv : String) return Natural
   is
      result : Natural := 0;
   begin
      if not CT.IsBlank (nv) then
         result := 1;
         for x in nv'Range loop
            if nv (x) = ',' then
               result := result + 1;
            end if;
         end loop;
      end if;
      return result;
   end num_set_items;


   -------------------
   --  log_problem  --
   -------------------
--     procedure log_problem
--       (statement  : SQLite_statement;
--        category   : LogCategory;
--        message    : String;
--        pull_codes : Boolean := False;
--        break      : Boolean := False)
--     is
--        error_msg  : CT.Text     := CT.blank;
--        error_code : DriverCodes := 0;
--        sqlstate   : TSqlState   := stateless;
--     begin
--        if pull_codes then
--           error_msg  := CT.SUS (statement.last_driver_message);
--           error_code := statement.last_driver_code;
--           sqlstate   := statement.last_sql_state;
--        end if;
--
--        logger_access.all.log_problem
--            (driver     => statement.dialect,
--             category   => category,
--             message    => CT.SUS (message),
--             error_msg  => error_msg,
--             error_code => error_code,
--             sqlstate   => sqlstate,
--             break      => break);
--     end log_problem;

end AdaBase.Statement.Base.SQLite;
