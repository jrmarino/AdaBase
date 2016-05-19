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


   ----------------------
   --  last_insert_id  --
   ----------------------
   overriding
   function last_insert_id (driver : PostgreSQL_Driver) return TraxID
   is
   begin
      return driver.connection.lastInsertID;
   end last_insert_id;


   ------------------
   --  disconnect  --
   ------------------
   overriding
   procedure disconnect (driver : out PostgreSQL_Driver) is
   begin
      --  TO BE IMPLEMENTED
      null;
   end disconnect;


   --------------
   --  commit  --
   --------------
   overriding
   procedure commit (driver : PostgreSQL_Driver)
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
         driver.log_problem (category => transaction, message  => err1);
         return;
      end if;
      if driver.connection.autoCommit then
         --  Non-fatal attempt to commit when autocommit is on
         driver.log_problem (category => transaction, message  => err2);
         return;
      end if;
      driver.connection.commit;
      driver.log_nominal (category => transaction, message => msg1);
   exception
      when CON.COMMIT_FAIL =>
         driver.log_problem (category   => transaction,
                             message    => err3,
                             pull_codes => True);
   end commit;


   ----------------
   --  rollback  --
   ----------------
   overriding
   procedure rollback (driver : PostgreSQL_Driver)
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
      when CON.ROLLBACK_FAIL =>
         driver.log_problem (category   => miscellaneous,
                             message    => err3,
                             pull_codes => True);
   end rollback;


--     -------------
--     --  query  --
--     -------------
--     function query (driver : PostgreSQL_Driver; sql : String)
--                     return SMT.PostgreSQL_statement is
--     begin
--        --  TO BE IMPLEMENTED
--        raise CON.UNSUPPORTED_BY_PGSQL;
--     end query;
--
--     function prepare (driver : PostgreSQL_Driver; sql : String)
--                       return SMT.PostgreSQL_statement is
--     begin
--        --  TO BE IMPLEMENTED
--        raise CON.UNSUPPORTED_BY_PGSQL;
--     end prepare;
--
--
--     --------------------
--     --  query_select  --
--     --------------------
--     function query_select   (driver     : PostgreSQL_Driver;
--                              distinct   : Boolean := False;
--                              tables     : String;
--                              columns    : String;
--                              conditions : String := blankstring;
--                              groupby    : String := blankstring;
--                              having     : String := blankstring;
--                              order      : String := blankstring;
--                              null_sort  : NullPriority := native;
--                              limit      : TraxID := 0;
--                              offset     : TraxID := 0)
--                              return SMT.PostgreSQL_statement is
--     begin
--        --  TO BE IMPLEMENTED
--        raise CON.UNSUPPORTED_BY_PGSQL;
--     end query_select;
--
--
--     ----------------------
--     --  prepare_select  --
--     ----------------------
--     function prepare_select (driver     : PostgreSQL_Driver;
--                              distinct   : Boolean := False;
--                              tables     : String;
--                              columns    : String;
--                              conditions : String := blankstring;
--                              groupby    : String := blankstring;
--                              having     : String := blankstring;
--                              order      : String := blankstring;
--                              null_sort  : NullPriority := native;
--                              limit      : TraxID := 0;
--                              offset     : TraxID := 0)
--                              return SMT.PostgreSQL_statement is
--     begin
--        --  TO BE IMPLEMENTED
--        raise CON.UNSUPPORTED_BY_PGSQL;
--     end prepare_select;


   ------------------
   --  initialize  --
   ------------------
   overriding
   procedure initialize (Object : in out PostgreSQL_Driver) is
   begin
      Object.connection       := Object.local_connection'Unchecked_Access;
      Object.dialect          := driver_postgresql;
   end initialize;


   ------------------------
   --  basic_connect #1  --
   ------------------------
   overriding
   procedure basic_connect (driver   : out PostgreSQL_Driver;
                            database : String;
                            username : String := blankstring;
                            password : String := blankstring;
                            socket   : String := blankstring) is
   begin
      driver.private_connect (database => database,
                              username => username,
                              password => password,
                              socket   => socket);
   end basic_connect;


   ------------------------
   --  basic_connect #2  --
   ------------------------
   overriding
   procedure basic_connect (driver   : out PostgreSQL_Driver;
                            database : String;
                            username : String := blankstring;
                            password : String := blankstring;
                            hostname : String := blankstring;
                            port     : PosixPort) is
   begin
      driver.private_connect (database => database,
                              username => username,
                              password => password,
                              hostname => hostname,
                              port     => port);
   end basic_connect;


   -----------------------
   --  private_connect  --
   -----------------------
   procedure private_connect (driver   : out PostgreSQL_Driver;
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

      driver.connection_active := driver.connection.connected;

      driver.log_nominal (category => connecting, message => nom);
   exception
      when Error : others =>
         driver.log_problem
           (category => connecting,
            break    => True,
            message  => CT.SUS (CON.EX.Exception_Message (X => Error)));
   end private_connect;


   ---------------
   --  execute  --
   ---------------
   overriding
   function execute (driver : PostgreSQL_Driver; sql : String)
                     return AffectedRows
   is
      trsql   : String := CT.trim_sql (sql);
      nquery  : Natural := CT.count_queries (trsql);
      aborted : constant AffectedRows := 0;
      err1    : constant CT.Text :=
                 CT.SUS ("ACK! Execution attempted on inactive connection");
      err2    : constant String :=
                         "Driver is configured to allow only one query at " &
                         "time, but this SQL contains multiple queries: ";
   begin
      if not driver.connection_active then
         --  Fatal attempt to query an unccnnected database
         driver.log_problem (category => execution,
                             message  => err1,
                             break    => True);
         return aborted;
      end if;

      if nquery > 1 and then not driver.trait_multiquery_enabled then
         --  Fatal attempt to execute multiple queries when it's not permitted
         driver.log_problem (category   => execution,
                             message    => CT.SUS (err2 & trsql),
                             break      => True);
         return aborted;
      end if;

      declare
         result : AffectedRows;
      begin
         for query_index in Positive range 1 .. nquery loop
            result := 0;
            if nquery = 1 then
               driver.connection.execute (trsql);
               driver.log_nominal (execution, CT.SUS (trsql));
            else
               declare
                  SQ : constant String := CT.subquery (trsql, query_index);
               begin
                  driver.connection.execute (SQ);
                  driver.log_nominal (execution, CT.SUS (SQ));
               end;
            end if;
         end loop;
         result := driver.connection.rows_affected_by_execution;
         return result;
      exception
         when CON.QUERY_FAIL =>
            driver.log_problem (category   => execution,
                                message    => CT.SUS (sql),
                                pull_codes => True);
            return aborted;
      end;
   end execute;

end AdaBase.Driver.Base.PostgreSQL;