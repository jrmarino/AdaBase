--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body AdaBase.Driver.Base.PostgreSQL is

   ------------------------
   --  last_driver_code  --
   ------------------------
   overriding
   function last_sql_state (driver : PostgreSQL_Driver) return TSqlState
   is
      --  Polled by driver.execute before result is cleared
   begin
      return driver.connection.SqlState;
   end last_sql_state;


   ------------------------
   --  last_driver_code  --
   ------------------------
   overriding
   function last_driver_code (driver : PostgreSQL_Driver) return DriverCodes
   is
   begin
      return driver.connection.driverCode;
   end last_driver_code;


   ---------------------------
   --  last_driver_message  --
   ---------------------------
   overriding
   function last_driver_message (driver : PostgreSQL_Driver) return String is
   begin
      return driver.connection.driverMessage;
   end last_driver_message;

end AdaBase.Driver.Base.PostgreSQL;
