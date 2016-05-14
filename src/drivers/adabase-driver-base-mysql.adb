--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Exceptions;

package body AdaBase.Driver.Base.MySQL is

   package EX renames Ada.Exceptions;

   ------------------
   --  disconnect  --
   ------------------
   overriding
   procedure disconnect (driver : out MySQL_Driver)
   is
      msg : constant CT.Text :=
        CT.SUS ("Disconnect From " & CT.USS (driver.database) & "database");
      err : constant CT.Text :=
        CT.SUS ("ACK! Disconnect attempted on inactive connection");
   begin
      if driver.connection_active then
         driver.connection.disconnect;
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
      err1 : constant CT.Text :=
        CT.SUS ("ACK! Rollback attempted on inactive connection");
      err2 : constant CT.Text :=
        CT.SUS ("ACK! Rollback attempted when autocommit mode set on");
      err3 : constant CT.Text :=
        CT.SUS ("Rollback attempt failed");
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
      err1 : constant CT.Text :=
        CT.SUS ("ACK! Commit attempted on inactive connection");
      err2 : constant CT.Text :=
        CT.SUS ("ACK! Commit attempted when autocommit mode set on");
      err3 : constant CT.Text := CT.SUS ("Commit attempt failed");
      msg1 : constant CT.Text := CT.SUS ("END TRANSACTION (COMMIT)");
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

      driver.connection.commit;
      driver.log_nominal (category => transaction, message => msg1);
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


   ------------------------
   --  last_driver_code  --
   ------------------------
   overriding
   function last_driver_code (driver : MySQL_Driver) return DriverCodes
   is
   begin
      return driver.connection.all.driverCode;
   end last_driver_code;


   ---------------------------
   --  last_driver_message  --
   ---------------------------
   overriding
   function last_driver_message (driver : MySQL_Driver) return String
   is
   begin
      return driver.connection.all.driverMessage;
   end last_driver_message;


   ---------------
   --  execute  --
   ---------------
   overriding
   function execute (driver : MySQL_Driver; sql : String)
                     return AffectedRows
   is
      result : AffectedRows := 0;
      err1 : constant CT.Text :=
        CT.SUS ("ACK! Execution attempted on inactive connection");
   begin
      if driver.connection_active then
         driver.connection.all.execute (sql => sql);
         result := driver.connection.all.rows_affected_by_execution;
         driver.log_nominal (category => execution, message => CT.SUS (sql));
      else
         --  Non-fatal attempt to query an unccnnected database
         driver.log_problem (category => execution,
                             message  => err1);
      end if;
      return result;
   exception
      when ACM.QUERY_FAIL =>
         driver.log_problem (category   => execution,
                             message    => CT.SUS (sql),
                             pull_codes => True);
         return 0;
   end execute;


   ------------------------------------------------------------------------
   --  ROUTINES OF ALL DRIVERS NOT COVERED BY INTERFACES (TECH REASON)   --
   ------------------------------------------------------------------------


   -------------
   --  query  --
   -------------
   function query (driver : MySQL_Driver; sql : String)
                   return ASM.MySQL_statement is
   begin
      return driver.private_query (sql);
   end query;


   ---------------
   --  prepare  --
   ---------------
   function prepare (driver : MySQL_Driver; sql : String)
                     return ASM.MySQL_statement is
   begin
      return driver.private_prepare (sql);
   end prepare;


   --------------------
   --  query_select  --
   --------------------
   function query_select (driver      : MySQL_Driver;
                          distinct    : Boolean := False;
                          tables      : String;
                          columns     : String;
                          conditions  : String := blankstring;
                          groupby     : String := blankstring;
                          having      : String := blankstring;
                          order       : String := blankstring;
                          null_sort   : NullPriority := native;
                          limit       : TraxID := 0;
                          offset      : TraxID := 0)
                          return ASM.MySQL_statement
   is
      vanilla : String := assembly_common_select
        (distinct, tables, columns, conditions, groupby, having, order);
   begin
      if null_sort /= native then
         driver.log_nominal
           (category => execution,
            message => CT.SUS ("Note that NULLS FIRST/LAST is not " &
                "supported by MySQL so the null_sort setting is ignored"));
      end if;
      if limit > 0 then
         if offset > 0 then
            return driver.private_query (vanilla &
                                          " LIMIT" & limit'Img &
                                          " OFFSET" & offset'Img);
         else
            return driver.private_query (vanilla & " LIMIT" & limit'Img);
         end if;
      end if;
      return driver.private_query (vanilla);
   end query_select;


   ----------------------
   --  prepare_select  --
   ----------------------
   function prepare_select (driver      : MySQL_Driver;
                            distinct    : Boolean := False;
                            tables      : String;
                            columns     : String;
                            conditions  : String := blankstring;
                            groupby     : String := blankstring;
                            having      : String := blankstring;
                            order       : String := blankstring;
                            null_sort   : NullPriority := native;
                            limit       : TraxID := 0;
                            offset      : TraxID := 0)
                            return ASM.MySQL_statement
   is
      vanilla : String := assembly_common_select
        (distinct, tables, columns, conditions, groupby, having, order);
   begin
      if null_sort /= native then
         driver.log_nominal
           (category => execution,
            message => CT.SUS ("Note that NULLS FIRST/LAST is not " &
                "supported by MySQL so the null_sort setting is ignored"));
      end if;
      if limit > 0 then
         if offset > 0 then
            return driver.private_prepare (vanilla &
                                          " LIMIT" & limit'Img &
                                          " OFFSET" & offset'Img);
         else
            return driver.private_prepare (vanilla & " LIMIT" & limit'Img);
         end if;
      end if;
      return driver.private_prepare (vanilla);
   end prepare_select;


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
   overriding
   procedure set_trait_multiquery_enabled (driver : MySQL_Driver;
                                           trait  : Boolean)
   is
   begin
      driver.local_connection.all.setMultiQuery (multiple => trait);
   end set_trait_multiquery_enabled;


   ------------------------------
   --  set_query_buffers_used  --
   ------------------------------
   procedure set_trait_query_buffers_used (driver : MySQL_Driver;
                                           trait  : Boolean)
   is
   begin
      driver.local_connection.all.setUseBuffer (buffered => trait);
   end set_trait_query_buffers_used;


   ---------------------
   --  basic_connect  --
   ---------------------
   overriding
   procedure basic_connect (driver   : out MySQL_Driver;
                            database : String;
                            username : String := blankstring;
                            password : String := blankstring;
                            socket   : String := blankstring)
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
                            username : String := blankstring;
                            password : String := blankstring;
                            hostname : String := blankstring;
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
      err1 : constant CT.Text :=
        CT.SUS ("ACK! Reconnection attempted on active connection");
      nom  : constant CT.Text :=
        CT.SUS ("Connection to " & database & " database succeeded.");
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
            message  => CT.SUS (ACM.EX.Exception_Message (X => Error)));
   end private_connect;


   ---------------------
   --  private_query  --
   ---------------------
   function private_query (driver : MySQL_Driver; sql : String)
                           return ASM.MySQL_statement
   is
      duplicate : aliased String := sql;
      err1 : constant CT.Text :=
        CT.SUS ("ACK! Query attempted on inactive connection");
   begin
      if driver.connection_active then
         declare
            err2 : constant CT.Text := CT.SUS ("Query failed!");
            statement : ASM.MySQL_statement
                (type_of_statement => AID.ASB.direct_statement,
                 log_handler       => logger'Access,
                 mysql_conn        => driver.local_connection,
                 initial_sql       => duplicate'Unchecked_Access,
                 con_error_mode    => driver.trait_error_mode,
                 con_case_mode     => driver.trait_column_case,
                 con_max_blob      => driver.trait_max_blob_size,
                 con_buffered      => driver.trait_query_buffers_used);
         begin
            if statement.successful then
               driver.log_nominal (category => execution,
                                   message => CT.SUS ("query succeeded," &
                                       statement.rows_returned'Img &
                                       " rows returned"));
            else
               driver.log_nominal (category => execution, message => err2);
            end if;
            return statement;
         exception
            when RES : others =>
               --  Fatal attempt to create a direct statement
               driver.log_problem
                 (category   => execution,
                  message    => CT.SUS (EX.Exception_Message (RES)),
                  pull_codes => True,
                  break      => True);
         end;
      else
         --  Fatal attempt to query an unconnected database
         driver.log_problem (category => execution,
                             message  => err1,
                             break    => True);
      end if;
      --  We never get here, the driver.log_problem throws exception first
      raise ACM.STMT_NOT_VALID
        with "failed to return MySQL statement";
   end private_query;


   -----------------------
   --  private_prepare  --
   -----------------------
   function private_prepare (driver : MySQL_Driver; sql : String)
                             return ASM.MySQL_statement
   is
      duplicate : aliased String := sql;
      err1 : constant CT.Text :=
        CT.SUS ("ACK! Query attempted on inactive connection");
   begin
      if driver.connection_active then
         declare
            statement : ASM.MySQL_statement
                (type_of_statement => AID.ASB.prepared_statement,
                 log_handler       => logger'Access,
                 mysql_conn        => driver.local_connection,
                 initial_sql       => duplicate'Unchecked_Access,
                 con_error_mode    => driver.trait_error_mode,
                 con_case_mode     => driver.trait_column_case,
                 con_max_blob      => driver.trait_max_blob_size,
                 con_buffered      => driver.trait_query_buffers_used);
         begin
            return statement;
         exception
            when RES : others =>
               --  Fatal attempt to prepare a statement
               driver.log_problem
                 (category   => statement_preparation,
                  message    => CT.SUS (EX.Exception_Message (RES)),
                  pull_codes => True,
                  break      => True);
         end;
      else
         --  Fatal attempt to query an unconnected database
         driver.log_problem (category => statement_preparation,
                             message  => err1,
                             break    => True);
      end if;
      --  We never get here, the driver.log_problem throws exception first
      raise ACM.STMT_NOT_VALID
        with "failed to return MySQL statement";
   end private_prepare;


end AdaBase.Driver.Base.MySQL;
