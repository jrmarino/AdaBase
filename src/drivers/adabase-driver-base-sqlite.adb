--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body AdaBase.Driver.Base.SQLite is

   ---------------
   --  execute  --
   ---------------
   overriding
   function execute (driver : SQLite_Driver; sql : String)
                     return Affected_Rows
   is
      trsql   : constant String := CT.trim_sql (sql);
      nquery  : constant Natural := CT.count_queries (trsql);
      aborted : constant Affected_Rows := 0;
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
         result : Affected_Rows;
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
                                message    => CT.SUS (trsql),
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
                          null_sort   : Null_Priority := native;
                          limit       : Trax_ID := 0;
                          offset      : Trax_ID := 0)
                          return ASS.SQLite_statement is
   begin
      return driver.private_statement
        (prepared => False,
         sql      => driver.sql_assemble (distinct   => distinct,
                                          tables     => tables,
                                          columns    => columns,
                                          conditions => conditions,
                                          groupby    => groupby,
                                          having     => having,
                                          order      => order,
                                          null_sort  => null_sort,
                                          limit      => limit,
                                          offset     => offset));
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
                            null_sort   : Null_Priority := native;
                            limit       : Trax_ID := 0;
                            offset      : Trax_ID := 0)
                            return ASS.SQLite_statement is
   begin
      return driver.private_statement
        (prepared => True,
         sql      => driver.sql_assemble (distinct   => distinct,
                                          tables     => tables,
                                          columns    => columns,
                                          conditions => conditions,
                                          groupby    => groupby,
                                          having     => having,
                                          order      => order,
                                          null_sort  => null_sort,
                                          limit      => limit,
                                          offset     => offset));
   end prepare_select;

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
   overriding
   procedure private_connect (driver   : out SQLite_Driver;
                              database : String;
                              username : String;
                              password : String;
                              hostname : String     := blankstring;
                              socket   : String     := blankstring;
                              port     : Posix_Port := portless)
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
      stype  : AID.ASB.Stmt_Type := AID.ASB.direct_statement;
      logcat : Log_Category      := execution;
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
            when others =>
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
      pragma Warnings (Off, "*ARSILENT*");
      declare
         ARSILENT  : Affected_Rows;
      begin
         ARSILENT := driver.execute (sql => CT.USS (sql));
      end;
      pragma Warnings (On, "*ARSILENT*");
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
   begin
      pragma Warnings (Off, "*ARSILENT*");
      declare
         ARSILENT  : Affected_Rows;
      begin
         ARSILENT := driver.execute (sql => sql);
      exception
         when ACS.QUERY_FAIL =>
            driver.log_problem (category   => execution,
                                message    => CT.SUS (sql),
                                pull_codes => True);
      end;
      pragma Warnings (On, "*ARSILENT*");
   end query_clear_table;


   --------------------
   --  sql_assemble  --
   --------------------
   function sql_assemble (driver     : SQLite_Driver;
                          distinct   : Boolean := False;
                          tables     : String;
                          columns    : String;
                          conditions : String := blankstring;
                          groupby    : String := blankstring;
                          having     : String := blankstring;
                          order      : String := blankstring;
                          null_sort  : Null_Priority := native;
                          limit      : Trax_ID := 0;
                          offset     : Trax_ID := 0) return String
   is
      vanilla   : constant String := assembly_common_select
        (distinct, tables, columns, conditions, groupby, having, order);
   begin
      if null_sort /= native then
         driver.log_nominal
           (category => execution,
            message => CT.SUS ("Note that NULLS FIRST/LAST is not " &
                "supported by SQLite so the null_sort setting is ignored"));
      end if;
      if limit > 0 then
         if offset > 0 then
            return vanilla & " LIMIT" & limit'Img & " OFFSET" & offset'Img;
         else
            return vanilla & " LIMIT" & limit'Img;
         end if;
      end if;
      return vanilla;
   end sql_assemble;


   -----------------------------
   --  call_stored_procedure  --
   -----------------------------
   function call_stored_procedure (driver           : SQLite_Driver;
                                   stored_procedure : String;
                                   proc_arguments   : String)
                                   return ASS.SQLite_statement is
   begin
      raise ACS.UNSUPPORTED_BY_SQLITE
        with "SQLite does not have the capability of stored procedures";
      return driver.query ("this never runs.");
   end call_stored_procedure;


end AdaBase.Driver.Base.SQLite;
