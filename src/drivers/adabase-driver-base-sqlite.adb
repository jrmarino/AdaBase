--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body AdaBase.Driver.Base.SQLite is

   ----------------------
   --  last_insert_id  --
   ----------------------
   overriding
   function last_insert_id (driver : SQLite_Driver) return TraxID
   is
   begin
      return driver.connection.all.lastInsertID;
   end last_insert_id;


   ------------------------
   --  last_driver_code  --
   ------------------------
   overriding
   function last_sql_state (driver : SQLite_Driver) return TSqlState
   is
   begin
      return driver.connection.all.SqlState;
   end last_sql_state;


   ------------------------
   --  last_driver_code  --
   ------------------------
   overriding
   function last_driver_code (driver : SQLite_Driver) return DriverCodes
   is
   begin
      return driver.connection.all.driverCode;
   end last_driver_code;


   ---------------------------
   --  last_driver_message  --
   ---------------------------
   overriding
   function last_driver_message (driver : SQLite_Driver) return String
   is
   begin
      return driver.connection.all.driverMessage;
   end last_driver_message;


   ---------------
   --  execute  --
   ---------------
   overriding
   function execute (driver : SQLite_Driver; sql : String)
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
         --  SQLITE3 execute supports multiquery in all cases, so it is not
         --  necessary to loop through subqueries.  We send the trimmed
         --  compound query as it was received.
         driver.connection.execute (trsql);
         driver.log_nominal (execution, CT.SUS (trsql));
         result := driver.connection.rows_affected_by_execution;
         return result;
      exception
         when ACS.QUERY_FAIL =>
            driver.log_problem (category   => execution,
                                message    => CT.SUS (sql),
                                pull_codes => True);
            return aborted;
      end;
   end execute;


   ------------------------------------------------------------------------
   --  ROUTINES OF ALL DRIVERS NOT COVERED BY INTERFACES (TECH REASON)   --
   ------------------------------------------------------------------------


   -------------
   --  query  --
   -------------
   function query (driver : SQLite_Driver; sql : String)
                   return ASS.SQLite_statement is
   begin
      return driver.private_statement (sql => sql, prepared => False);
   end query;


   ---------------
   --  prepare  --
   ---------------
   function prepare (driver : SQLite_Driver; sql : String)
                     return ASS.SQLite_statement is
   begin
      return driver.private_statement (sql => sql, prepared => True);
   end prepare;


   --------------------
   --  query_select  --
   --------------------
   function query_select (driver      : SQLite_Driver;
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
                          return ASS.SQLite_statement
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
            return driver.private_statement (vanilla &
                                               " LIMIT" & limit'Img &
                                               " OFFSET" & offset'Img,
                                             False);
         else
            return driver.private_statement (vanilla & " LIMIT" & limit'Img,
                                         False);
         end if;
      end if;
      return driver.private_statement (vanilla, False);
   end query_select;


   ----------------------
   --  prepare_select  --
   ----------------------
   function prepare_select (driver      : SQLite_Driver;
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
                            return ASS.SQLite_statement
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
            return driver.private_statement (vanilla &
                                               " LIMIT" & limit'Img &
                                               " OFFSET" & offset'Img,
                                             True);
         else
            return driver.private_statement (vanilla & " LIMIT" & limit'Img,
                                             True);
         end if;
      end if;
      return driver.private_statement (vanilla, True);
   end prepare_select;

   ------------------------------------------------------------------------
   --  PUBLIC ROUTINES NOT COVERED BY INTERFACES                         --
   ------------------------------------------------------------------------

   ---------------------
   --  basic_connect  --
   ---------------------
   overriding
   procedure basic_connect (driver   : out SQLite_Driver;
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
   procedure basic_connect (driver   : out SQLite_Driver;
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
   procedure initialize (Object : in out SQLite_Driver)
   is
   begin
      Object.connection       := Object.local_connection'Unchecked_Access;
      Object.dialect          := driver_sqlite;
   end initialize;


   -----------------------
   --  private_connect  --
   -----------------------
   procedure private_connect (driver   : out SQLite_Driver;
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
            message  => CT.SUS (ACS.EX.Exception_Message (X => Error)));
   end private_connect;


   -------------------------
   --  private_statement  --
   -------------------------
   function private_statement (driver   : SQLite_Driver;
                               sql      : String;
                               prepared : Boolean)
                               return ASS.SQLite_statement
   is
      stype  : AID.ASB.stmt_type := AID.ASB.direct_statement;
      logcat : LogCategory       := execution;
      err1   : constant CT.Text  :=
               CT.SUS ("ACK! Query attempted on inactive connection");
      duplicate : aliased String := sql;
   begin
      if prepared then
         stype  := AID.ASB.prepared_statement;
         logcat := statement_preparation;
      end if;
      if driver.connection_active then
         declare
            statement : ASS.SQLite_statement
              (type_of_statement => stype,
               log_handler       => logger'Access,
               sqlite_conn       => ACS.SQLite_Connection_Access
                                    (driver.connection),
               initial_sql       => duplicate'Unchecked_Access,
               con_error_mode    => driver.trait_error_mode,
               con_case_mode     => driver.trait_column_case,
               con_max_blob      => driver.trait_max_blob_size);
         begin
            if not prepared then
               if statement.successful then
                  driver.log_nominal
                    (category => logcat,
                     message => CT.SUS ("query succeeded," &
                         statement.rows_returned'Img & " rows returned"));
               else
                  driver.log_nominal (category => execution,
                                      message => CT.SUS ("Query failed!"));
               end if;
            end if;
            return statement;
         exception
            when RES : others =>
               --  Fatal attempt to prepare a statement
               --  Logged already by stmt initialization
               --  Should be internally marked as unsuccessful
               return statement;
         end;
      else
         --  Fatal attempt to query an unconnected database
         driver.log_problem (category => logcat,
                             message  => err1,
                             break    => True);
      end if;
      --  We never get here, the driver.log_problem throws exception first
      raise ACS.STMT_NOT_VALID
        with "failed to return SQLite statement";
   end private_statement;


   ------------------------
   --  query_drop_table  --
   ------------------------
   overriding
   procedure query_drop_table (driver      : SQLite_Driver;
                               tables      : String;
                               when_exists : Boolean := False;
                               cascade     : Boolean := False)

   is
      sql : CT.Text;
      AR  : AffectedRows;
   begin
      if CT.contains (tables, ",") then
         driver.log_problem
           (category => execution,
            message => CT.SUS ("Multiple tables detected -- SQLite" &
                " can only drop one table at a time : " & tables));
         return;
      end if;
      case when_exists is
         when True  => sql := CT.SUS ("DROP TABLE IF EXISTS " & tables);
         when False => sql := CT.SUS ("DROP TABLE " & tables);
      end case;
      if cascade then
         driver.log_nominal
           (category => note,
            message => CT.SUS ("Requested CASCADE has no effect on SQLite"));
      end if;
      AR := driver.execute (sql => CT.USS (sql));
   exception
      when ACS.QUERY_FAIL =>
         driver.log_problem (category   => execution,
                             message    => sql,
                             pull_codes => True);
   end query_drop_table;


   -------------------------
   --  query_clear_table  --
   -------------------------
   overriding
   procedure query_clear_table (driver : SQLite_Driver;
                                table  : String)
   is
      --  SQLite has no "truncate" commands
      sql : constant String := "DELETE FROM " & table;
      AR  : AffectedRows;
   begin
      AR := driver.execute (sql => sql);
   exception
      when ACS.QUERY_FAIL =>
         driver.log_problem (category   => execution,
                             message    => CT.SUS (sql),
                             pull_codes => True);
   end query_clear_table;


end AdaBase.Driver.Base.SQLite;
