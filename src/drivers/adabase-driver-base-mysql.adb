--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Exceptions;

package body AdaBase.Driver.Base.MySQL is

   package EX renames Ada.Exceptions;

   ---------------
   --  execute  --
   ---------------
   overriding
   function execute (driver : MySQL_Driver; sql : String)
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
         --  MySQL execute is configured to support multiquery at this point
         --  so it is not necessary to loop through subqueries.  We send the
         --  trimmed compound query as it was received.
         driver.connection.execute (trsql);
         driver.log_nominal (execution, CT.SUS (trsql));
         result := driver.connection.rows_affected_by_execution;
         return result;
      exception
         when ACM.QUERY_FAIL =>
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
                          return ASM.MySQL_statement is
   begin
      return driver.private_query
        (driver.sql_assemble (distinct   => distinct,
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
                            return ASM.MySQL_statement is
   begin
      return driver.private_prepare
        (driver.sql_assemble (distinct   => distinct,
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
   --  PUBLIC ROUTINES NOT COVERED BY INTERFACES                         --
   ------------------------------------------------------------------------

   ---------------------------------
   --  trait_compressed_protocol  --
   ---------------------------------
   function trait_protocol_compressed (driver : MySQL_Driver) return Boolean
   is
   begin
      return driver.connection.compressed;
   end trait_protocol_compressed;


   --------------------------------
   --  trait_query_buffers_used  --
   --------------------------------
   function trait_query_buffers_used  (driver : MySQL_Driver) return Boolean
   is
   begin
      return driver.connection.useBuffer;
   end trait_query_buffers_used;


   --------------------------------------
   --  set_trait_compressed_protocol  --
   -------------------------------------
   procedure set_trait_protocol_compressed (driver : MySQL_Driver;
                                            trait  : Boolean)
   is
   begin
      driver.connection.setCompressed (compressed => trait);
   end set_trait_protocol_compressed;


   ------------------------------
   --  set_query_buffers_used  --
   ------------------------------
   procedure set_trait_query_buffers_used (driver : MySQL_Driver;
                                           trait  : Boolean)
   is
   begin
      driver.connection.setUseBuffer (buffered => trait);
   end set_trait_query_buffers_used;

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
      Object.connection       := Object.local_connection'Unchecked_Access;
      Object.dialect          := driver_mysql;
   end initialize;


   -----------------------
   --  private_connect  --
   -----------------------
   overriding
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
                 mysql_conn        => ACM.MySQL_Connection_Access
                                      (driver.connection),
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
                 mysql_conn        => ACM.MySQL_Connection_Access
                                      (driver.connection),
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


   --------------------
   --  sql_assemble  --
   --------------------
   function sql_assemble (driver     : MySQL_Driver;
                          distinct   : Boolean := False;
                          tables     : String;
                          columns    : String;
                          conditions : String := blankstring;
                          groupby    : String := blankstring;
                          having     : String := blankstring;
                          order      : String := blankstring;
                          null_sort  : NullPriority := native;
                          limit      : TraxID := 0;
                          offset     : TraxID := 0) return String
   is
      vanilla   : String := assembly_common_select
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
            return vanilla & " LIMIT" & limit'Img & " OFFSET" & offset'Img;
         else
            return vanilla & " LIMIT" & limit'Img;
         end if;
      end if;
      return vanilla;
   end sql_assemble;


end AdaBase.Driver.Base.MySQL;
