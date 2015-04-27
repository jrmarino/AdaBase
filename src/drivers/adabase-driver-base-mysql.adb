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
      msg : constant textual :=
        SUS ("Disconnect From " & USS (driver.database) & "database");
      err : constant textual :=
        SUS ("ACK! Disconnect attempted on inactive connection");
   begin
      if driver.connection_active then
         driver.connection.disconnect;
         driver.connection := null;
         driver.connection_active := False;

         driver.log_nominal (category => disconnecting,
                             message  => msg);
      else
         --  Non-fatal attempt to disconnect db when none is connected
         driver.log_problem (category => disconnecting,
                             message  => err);
      end if;
   end disconnect;


   ----------------
   --  rollback  --
   ----------------
   overriding
   procedure rollback (driver : MySQL_Driver)
   is
      use type TransIsolation;
      err1 : constant textual :=
        SUS ("ACK! Rollback attempted on inactive connection");
      err2 : constant textual :=
        SUS ("ACK! Rollback attempted when autocommit mode set on");
      err3 : constant textual :=
        SUS ("Rollback attempt failed");
   begin
      if not driver.connection_active then
         --  Non-fatal attempt to roll back when no database is connected
         driver.log_problem (category => miscellaneous,
                             message  => err1);
         return;
      end if;
      if driver.connection.autoCommit then
         --  Non-fatal attempt to roll back when autocommit is on
         driver.log_problem (category => miscellaneous,
                             message  => err2);
         return;
      end if;

      driver.connection.rollback;

   exception
      when ACM.ROLLBACK_FAIL =>
         driver.log_problem (category   => miscellaneous,
                             message    => err3,
                             pull_codes => True);

   end rollback;


   --------------
   --  commit  --
   --------------
   overriding
   procedure commit (driver : MySQL_Driver)
   is
      use type TransIsolation;
      err1 : constant textual :=
        SUS ("ACK! Commit attempted on inactive connection");
      err2 : constant textual :=
        SUS ("ACK! Commit attempted when autocommit mode set on");
      err3 : constant textual :=
        SUS ("Commit attempt failed");
   begin
      if not driver.connection_active then
         --  Non-fatal attempt to commit when no database is connected
         driver.log_problem (category => transaction,
                             message  => err1);
         return;
      end if;
      if driver.connection.autoCommit then
         --  Non-fatal attempt to commit when autocommit is on
         driver.log_problem (category => transaction,
                             message  => err2);
         return;
      end if;

      driver.connection.all.commit;

   exception
      when ACM.COMMIT_FAIL =>
         driver.log_problem (category   => transaction,
                             message    => err3,
                             pull_codes => True);
   end commit;


   ----------------------
   --  last_insert_id  --
   ----------------------
   overriding
   function last_insert_id (driver : MySQL_Driver) return TraxID
   is
   begin
      return driver.connection.all.lastInsertID;
   end last_insert_id;


   ------------------------
   --  last_driver_code  --
   ------------------------
   overriding
   function last_sql_state (driver : MySQL_Driver) return TSqlState
   is
   begin
      return driver.connection.all.SqlState;
   end last_sql_state;


   ---------------------------
   --  last_driver_message  --
   ---------------------------
   overriding
   function last_error_info (driver : MySQL_Driver) return Error_Info
   is
      result : Error_Info;
   begin
      result.sql_state      := driver.connection.all.SqlState;
      result.driver_code    := driver.connection.all.driverCode;
      result.driver_message := driver.connection.all.driverMessage;
      return result;
   end last_error_info;


   ---------------
   --  execute  --
   ---------------
   overriding
   function execute (driver : MySQL_Driver; sql : String)
                     return AffectedRows
   is
      result : AffectedRows := 0;
      err1 : constant textual :=
        SUS ("ACK! Execution attempted on inactive connection");
   begin
      if driver.connection_active then
         result := driver.connection.all.execute (sql => sql);
         driver.log_nominal (category => execution, message => SUS (sql));
      else
         --  Non-fatal attempt to query an unccnnected database
         driver.log_problem (category => execution,
                             message  => err1);
      end if;
      return result;
   exception
      when ACM.QUERY_FAIL =>
         driver.log_problem (category   => execution,
                             message    => SUS (sql),
                             pull_codes => True);
         return 0;
   end execute;

   -------------
   --  query  --
   -------------
   overriding
   function query (driver : MySQL_Driver; sql : String)
                   return  AS.Base'Class
   is
      result : AS.MySQL.MySQL_statement;
      err1 : constant textual :=
        SUS ("ACK! Query attempted on inactive connection");
   begin
      if driver.connection_active then
         driver.local_connection.all.initializeStatement (stmt => result);
         --  driver.connection.initializeStatement;
         --  set sql ???
         --  driver.connection.execute (sql => sql);
         --  Result.successful := True;
         driver.log_nominal (category => execution, message => SUS (sql));
         null;
      else
         --  Non-fatal attempt to query an unccnnected database
         driver.log_problem (category => execution,
                             message  => err1);
      end if;
      return result;
   end query;

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


   ---------------------
   --  basic_connect  --
   ---------------------
   overriding
   procedure basic_connect (driver   : out MySQL_Driver;
                            database : String;
                            username : String;
                            password : String;
                            socket   : String)
   is
   begin
      driver.private_connect (database => database,
                              username => username,
                              password => password,
                              socket   => socket);
   end basic_connect;


   ---------------------
   --  basic_connect  --
   ---------------------
   overriding
   procedure basic_connect (driver   : out MySQL_Driver;
                            database : String;
                            username : String;
                            password : String;
                            hostname : String;
                            port     : PosixPort)
   is
   begin
      driver.private_connect (database => database,
                              username => username,
                              password => password,
                              hostname => hostname,
                              port     => port);
   end basic_connect;


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
      Object.dialect          := driver_mysql;
   end initialize;


   -----------------------
   --  private_connect  --
   -----------------------
   procedure private_connect (driver   : out MySQL_Driver;
                              database : String;
                              username : String;
                              password : String;
                              hostname : String := blankstring;
                              socket   : String := blankstring;
                              port     : PosixPort := portless)
   is
      err1 : constant textual :=
        SUS ("ACK! Reconnection attempted on active connection");
      nom  : constant textual :=
        SUS ("Connection to " & database & " database succeeded.");
   begin
      if driver.connection_active then
         driver.log_problem (category => execution,
                             message  => err1);
         return;
      end if;
      driver.connection.connect (database => database,
                                 username => username,
                                 password => password,
                                 socket   => socket,
                                 hostname => hostname,
                                 port     => port);

      driver.connection_active := driver.connection.all.connected;

      driver.log_nominal (category => connecting, message => nom);
   exception
      when Error : others =>
         driver.log_problem
           (category => connecting,
            break    => True,
            message  => SUS (ACM.EX.Exception_Message (X => Error)));
   end private_connect;

end AdaBase.Driver.Base.MySQL;
