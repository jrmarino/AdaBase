--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Results.Sets;

package body AdaBase.Driver.Base.PostgreSQL is

   package ARS renames AdaBase.Results.Sets;

   -------------
   --  query  --
   -------------
   function query (driver : PostgreSQL_Driver; sql : String)
                   return SMT.PostgreSQL_statement is
   begin
      return driver.private_statement (sql => sql, prepared => False);
   end query;


   ---------------
   --  prepare  --
   ---------------
   function prepare (driver : PostgreSQL_Driver; sql : String)
                     return SMT.PostgreSQL_statement is
   begin
      return driver.private_statement (sql => sql, prepared => True);
   end prepare;


   ----------------------
   --  prepare_select  --
   ----------------------
   function prepare_select (driver     : PostgreSQL_Driver;
                            distinct   : Boolean := False;
                            tables     : String;
                            columns    : String;
                            conditions : String := blankstring;
                            groupby    : String := blankstring;
                            having     : String := blankstring;
                            order      : String := blankstring;
                            null_sort  : Null_Priority := native;
                            limit      : Trax_ID := 0;
                            offset     : Trax_ID := 0)
                            return SMT.PostgreSQL_statement is
   begin
      return driver.private_statement
        (prepared => True,
         sql      => sql_assemble (distinct   => distinct,
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


   --------------------
   --  query_select  --
   --------------------
   function query_select   (driver     : PostgreSQL_Driver;
                            distinct   : Boolean := False;
                            tables     : String;
                            columns    : String;
                            conditions : String := blankstring;
                            groupby    : String := blankstring;
                            having     : String := blankstring;
                            order      : String := blankstring;
                            null_sort  : Null_Priority := native;
                            limit      : Trax_ID := 0;
                            offset     : Trax_ID := 0)
                            return SMT.PostgreSQL_statement is
   begin
      return driver.private_statement
        (prepared => False,
         sql      => sql_assemble (distinct   => distinct,
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


   --------------------
   --  sql_assemble  --
   --------------------
   function sql_assemble (distinct   : Boolean := False;
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
      rockyroad : CT.Text;
      vanilla   : constant String := assembly_common_select
        (distinct, tables, columns, conditions, groupby, having, order);
   begin
      rockyroad := CT.SUS (vanilla);
      if not CT.IsBlank (order) and then null_sort /= native then
         case null_sort is
            when native      => null;
            when nulls_first => CT.SU.Append (rockyroad, " NULLS FIRST");
            when nulls_last  => CT.SU.Append (rockyroad, " NULLS LAST");
         end case;
      end if;
      if limit > 0 then
         if offset > 0 then
            return CT.USS (rockyroad) & " LIMIT" & limit'Img &
                                        " OFFSET" & offset'Img;
         else
            return CT.USS (rockyroad) & " LIMIT" & limit'Img;
         end if;
      end if;
      return CT.USS (rockyroad);
   end sql_assemble;


   ------------------
   --  initialize  --
   ------------------
   overriding
   procedure initialize (Object : in out PostgreSQL_Driver) is
   begin
      Object.connection       := Object.local_connection'Unchecked_Access;
      Object.dialect          := driver_postgresql;
   end initialize;


   -----------------------
   --  private_connect  --
   -----------------------
   overriding
   procedure private_connect (driver   : out PostgreSQL_Driver;
                              database : String;
                              username : String;
                              password : String;
                              hostname : String := blankstring;
                              socket   : String := blankstring;
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
         --  In order to support INSERT INTO .. RETURNING, we have to execute
         --  multiqueries individually because we are scanning the first 7
         --  characters to be "INSERT " after converting to upper case.
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
                                message    => CT.SUS (trsql),
                                pull_codes => True);
            return aborted;
      end;
   end execute;


   -------------------------
   --  private_statement  --
   -------------------------
   function private_statement (driver   : PostgreSQL_Driver;
                               sql      : String;
                               nextsets : String := "";
                               prepared : Boolean)
                               return SMT.PostgreSQL_statement
   is
      stype     : AID.ASB.Stmt_Type := AID.ASB.direct_statement;
      logcat    : Log_Category      := execution;
      duplicate : aliased String    := sql;
      dupensets : aliased String    := nextsets;
      err1      : constant CT.Text  :=
                  CT.SUS ("ACK! Query attempted on inactive connection");
   begin
      if prepared then
         stype  := AID.ASB.prepared_statement;
         logcat := statement_preparation;
      end if;
      if driver.connection_active then
         global_statement_counter := global_statement_counter + 1;
         declare
            buffered_mode : constant Boolean := not driver.async_cmd_mode;
            statement : SMT.PostgreSQL_statement
              (type_of_statement => stype,
               log_handler       => logger'Access,
               pgsql_conn        => CON.PostgreSQL_Connection_Access
                                    (driver.connection),
               identifier        => global_statement_counter,
               initial_sql       => duplicate'Unchecked_Access,
               next_calls        => dupensets'Unchecked_Access,
               con_error_mode    => driver.trait_error_mode,
               con_case_mode     => driver.trait_column_case,
               con_max_blob      => driver.trait_max_blob_size,
               con_buffered      => buffered_mode);
         begin
            if not prepared then
               if statement.successful then
                  driver.log_nominal
                    (category => logcat,
                     message  => CT.SUS ("query" &
                                 global_statement_counter'Img &
                                 " succeeded," & statement.rows_returned'Img &
                                 " rows returned"));
               else
                  driver.log_nominal
                    (category => execution,
                     message  => CT.SUS ("Query" &
                                 global_statement_counter'Img & " failed!"));
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
      raise CON.STMT_NOT_VALID
        with "failed to return statement";

   end private_statement;


   --------------------------------
   --  trait_query_buffers_used  --
   --------------------------------
   function trait_query_buffers_used  (driver : PostgreSQL_Driver)
                                       return Boolean is
   begin
      return not (driver.async_cmd_mode);
   end trait_query_buffers_used;


   ------------------------------
   --  set_query_buffers_used  --
   ------------------------------
   procedure set_trait_query_buffers_used (driver : PostgreSQL_Driver;
                                           trait  : Boolean)
   is
      --  Once the asynchronous command mode is supported (meaning that the
      --  driver has to manually coordinate sending the queries and fetching
      --  the rows one by one), then this procedure just changes
      --  driver.async_cmd_mode => False.
   begin
      raise CON.UNSUPPORTED_BY_PGSQL
        with "Single row mode is not currently supported";
   end set_trait_query_buffers_used;


   -----------------------------
   --  call_stored_procedure  --
   -----------------------------
   function call_stored_procedure (driver           : PostgreSQL_Driver;
                                   stored_procedure : String;
                                   proc_arguments   : String)
                                   return SMT.PostgreSQL_statement
   is
      SQL : constant String := "SELECT " & stored_procedure &
                      " (" & proc_arguments & ")";
      stmt : SMT.PostgreSQL_statement :=
        driver.private_statement (sql => SQL, prepared => False);
   begin
      if stmt.returned_refcursors then
         if driver.trait_autocommit then
            raise CON.STORED_PROCEDURES with "When executing stored " &
              "procedures that return references to result sets, autocommit " &
              "mode must be OFF (it is currently ON).";
         end if;
         declare
            fullset  : constant ARS.Datarow_Set := stmt.fetch_all;
            nextcall : constant String := fullset (1).column (1).as_string;
            calls    : CT.Text;
         begin
            for x in Natural range 2 .. fullset'Length loop
               if not CT.IsBlank (calls) then
                  CT.SU.Append (calls, ',');
               end if;
               CT.SU.Append (calls, fullset (x).column (1).as_string);
            end loop;
            declare
               SQL2 : constant String := "FETCH ALL IN " &
                                 ASCII.Quotation & nextcall & ASCII.Quotation;
            begin
               return driver.private_statement (sql      => SQL2,
                                                nextsets => CT.USS (calls),
                                                prepared => False);
            end;
         end;
      else
         return stmt;
      end if;
   end call_stored_procedure;


end AdaBase.Driver.Base.PostgreSQL;
