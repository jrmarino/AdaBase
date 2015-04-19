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

with AdaBase.Statement.MySQL;

package body AdaBase.Driver.Base.MySQL is


   ------------------
   --  disconnect  --
   ------------------
   overriding
   procedure disconnect (driver : out MySQL_Driver)
   is
      msg : constant AD.textual :=
        SUS ("Disconnect From " & USS (driver.database) & "database");
      err : constant AD.textual :=
        SUS ("ACK! Disconnect attempted on inactive connection");
   begin
      if driver.connection_active then
         driver.connection.disconnect;
         driver.connection := null;
         driver.connection_active := False;

         driver.log_nominal (category => AD.disconnection,
                             message  => msg);
      else
         --  Non-fatal attempt to disconnect db when none is connected
         driver.log_problem (category => AD.disconnection,
                             message  => err);
      end if;
   end disconnect;


   ----------------
   --  rollback  --
   ----------------
   overriding
   procedure rollback (driver : MySQL_Driver)
   is
      use type ALF.AD.TransIsolation;
      err1 : constant AD.textual :=
        SUS ("ACK! Rollback attempted on inactive connection");
      err2 : constant AD.textual :=
        SUS ("ACK! Rollback attempted when autocommit mode set on");
      err3 : constant AD.textual :=
        SUS ("Rollback attempt failed");
   begin
      if not driver.connection_active then
         --  Non-fatal attempt to roll back when no database is connected
         driver.log_problem (category => AD.miscellaneous,
                             message  => err1);
         return;
      end if;
      if driver.connection.autoCommit then
         --  Non-fatal attempt to roll back when autocommit is on
         driver.log_problem (category => AD.miscellaneous,
                             message  => err2);
         return;
      end if;

      driver.connection.rollback;

   exception
      when ACM.ROLLBACK_FAIL =>
         driver.log_problem (category   => AD.miscellaneous,
                             message    => err3,
                             pull_codes => True);

   end rollback;


   --------------
   --  commit  --
   --------------
   overriding
   procedure commit (driver : MySQL_Driver)
   is
      use type ALF.AD.TransIsolation;
      err1 : constant AD.textual :=
        SUS ("ACK! Commit attempted on inactive connection");
      err2 : constant AD.textual :=
        SUS ("ACK! Commit attempted when autocommit mode set on");
      err3 : constant AD.textual :=
        SUS ("Commit attempt failed");
   begin
      if not driver.connection_active then
         --  Non-fatal attempt to commit when no database is connected
         driver.log_problem (category => AD.transaction,
                             message  => err1);
         return;
      end if;
      if driver.connection.autoCommit then
         --  Non-fatal attempt to commit when autocommit is on
         driver.log_problem (category => AD.transaction,
                             message  => err2);
         return;
      end if;

      driver.connection.all.commit;

   exception
      when ACM.COMMIT_FAIL =>
         driver.log_problem (category   => AD.transaction,
                             message    => err3,
                             pull_codes => True);
   end commit;


   ----------------------
   --  last_insert_id  --
   ----------------------
   overriding
   function last_insert_id (driver : MySQL_Driver) return AD.TraxID
   is
   begin
      return driver.connection.all.lastInsertID;
   end last_insert_id;


   ------------------------
   --  last_driver_code  --
   ------------------------
   overriding
   function last_sql_state (driver : MySQL_Driver) return AD.TSqlState
   is
   begin
      return driver.connection.all.SqlState;
   end last_sql_state;


   ---------------------------
   --  last_driver_message  --
   ---------------------------
   overriding
   function last_error_info (driver : MySQL_Driver) return AD.Error_Info
   is
      result : AD.Error_Info;
   begin
      result.sql_state      := driver.connection.all.SqlState;
      result.driver_code    := driver.connection.all.driverCode;
      result.driver_message := driver.connection.all.driverMessage;
      return result;
   end last_error_info;


   ---------------------
   --  query_literal  --
   ---------------------
   overriding
   function query_literal (driver : MySQL_Driver;
                           sql    : AD.textual)
                           return  AS.Base'Class
   is
      result : AS.MySQL.MySQL_statement;
      err1 : constant AD.textual :=
        SUS ("ACK! Query attempted on inactive connection");
   begin
      if driver.connection_active then
         driver.local_connection.all.initializeStatement (stmt => result);
         --  driver.connection.initializeStatement;
         --  set sql ???
         --  driver.connection.execute (sql => sql);
         --  Result.successful := True;
         null;
      else
         --  Non-fatal attempt to query an unccnnected database
         driver.log_problem (category => AD.execution,
                             message  => err1);
      end if;
      return result;
   end query_literal;

   ------------------------------------------------------------------------
   --  PUBLIC ROUTINES NOT COVERED BY INTERFACES                         --
   ------------------------------------------------------------------------

   ---------------------------------
   --  trait_compressed_protocol  --
   ---------------------------------
   function trait_protocol_compressed (driver : MySQL_Driver) return Boolean
   is
   begin
      return driver.local_connection.all.compressed;
   end trait_protocol_compressed;


   --------------------------------
   --  trait_multiquery_enabled  --
   --------------------------------
   function trait_multiquery_enabled (driver : MySQL_Driver) return Boolean
   is
   begin
      return driver.local_connection.all.multiquery;
   end trait_multiquery_enabled;


   --------------------------------
   --  trait_query_buffers_used  --
   --------------------------------
   function trait_query_buffers_used  (driver : MySQL_Driver) return Boolean
   is
   begin
      return driver.local_connection.all.useBuffer;
   end trait_query_buffers_used;


   --------------------------------------
   --  set_trait_compressed_protocol  --
   -------------------------------------
   procedure set_trait_protocol_compressed (driver : MySQL_Driver;
                                            trait  : Boolean)
   is
   begin
      driver.local_connection.all.setCompressed (compressed => trait);
   end set_trait_protocol_compressed;


   ------------------------------------
   --  set_trait_multiquery_enabled  --
   ------------------------------------
   procedure set_trait_multiquery_enabled (driver : MySQL_Driver;
                                           trait  : Boolean)
   is
   begin
      driver.local_connection.all.setMultiQuery (multiple => trait);
   end set_trait_multiquery_enabled;


   ------------------------------
   --  set_query_buffers_used  --
   ------------------------------
   procedure set_query_buffers_used (driver : MySQL_Driver;
                                     trait  : Boolean)
   is
   begin
      driver.local_connection.all.setUseBuffer (buffered => trait);
   end set_query_buffers_used;


   ------------------------------------------------------------------------
   --  PRIVATE ROUTINES NOT COVERED BY INTERFACES                        --
   ------------------------------------------------------------------------

   ------------------
   --  initialize  --
   ------------------
   overriding
   procedure initialize (Object : in out MySQL_Driver)
   is
   begin
      Object.connection       := backend'Access;
      Object.local_connection := backend'Access;
   end initialize;


   ------------------
   --  log_nominal --
   ------------------
   procedure log_nominal (driver    : MySQL_Driver;
                          category  : AD.LogCategory;
                          message   : AD.textual)
   is
   begin
         logger.log_nominal (driver   => ALF.AD.mysql,
                             category => category,
                             message  => message);
   end log_nominal;


   ------------------
   --  log_problem --
   ------------------
   procedure log_problem
     (driver     : MySQL_Driver;
      category   : AD.LogCategory;
      message    : AD.textual;
      pull_codes : Boolean := False;
      break      : Boolean := False)
   is
      error_msg  : AD.textual      := AD.blank;
      error_code : AD.DriverCodes  := 0;
      sqlstate   : AD.TSqlState    := AD.stateless;
   begin
      if pull_codes then
         error_msg  := driver.connection.driverMessage;
         error_code := driver.connection.driverCode;
         sqlstate   := driver.connection.SqlState;
      end if;

      logger.log_problem (driver     => AD.mysql,
                          category   => category,
                          message    => message,
                          error_msg  => error_msg,
                          error_code => error_code,
                          sqlstate   => sqlstate,
                          break      => break);
   end log_problem;

end AdaBase.Driver.Base.MySQL;
