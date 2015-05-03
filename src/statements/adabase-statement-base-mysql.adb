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


with Ada.Exceptions;
with Ada.Characters.Handling;

package body AdaBase.Statement.Base.MySQL is

   package EX renames Ada.Exceptions;
   package ACH renames Ada.Characters.Handling;

   --------------------
   --  discard_rest  --
   --------------------
   overriding
   procedure discard_rest  (Stmt : out MySQL_statement;
                            was_complete : out Boolean)
   is
      --  use type ACM.MySQL_Connection_Access;
      --  use type ABM.MYSQL_STMT_Access;
   begin
      --  case Stmt.type_of_statement is
      --   when direct_statement   => was_complete := Stmt.connection = null;
      --   when prepared_statement => was_complete := Stmt.connection = null;
      --  end case;
      was_complete := False;
      Stmt.clear_buffer;
   end discard_rest;


   --------------------
   --  column_count  --
   --------------------
   overriding
   function column_count (Stmt : MySQL_statement) return Natural
   is
   begin
      return Stmt.num_columns;
   end column_count;


   ------------------
   --  successful  --
   ------------------
   overriding
   function successful  (Stmt : MySQL_statement) return Boolean
   is
   begin
      return Stmt.successful_execution;
   end successful;


   ---------------------------
   --  last_driver_message  --
   ---------------------------
   overriding
   function last_driver_message (Stmt : MySQL_statement) return String
   is
   begin
      case Stmt.type_of_statement is
         when direct_statement   =>
            return Stmt.mysql_conn.all.driverMessage;
         when prepared_statement =>
            return Stmt.mysql_conn.all.prep_DriverMessage (Stmt.stmt_handle);
      end case;

   end last_driver_message;


   ----------------------
   --  last_insert_id  --
   ----------------------
   overriding
   function last_insert_id (Stmt : MySQL_statement) return TraxID
   is
   begin
      case Stmt.type_of_statement is
         when direct_statement   =>
            return Stmt.mysql_conn.all.lastInsertID;
         when prepared_statement =>
            return Stmt.mysql_conn.all.prep_LastInsertID (Stmt.stmt_handle);
      end case;
   end last_insert_id;


   ----------------------
   --  last_sql_state  --
   ----------------------
   overriding
   function last_sql_state (Stmt : MySQL_statement) return TSqlState is
   begin
      case Stmt.type_of_statement is
         when direct_statement   =>
            return Stmt.mysql_conn.all.SqlState;
         when prepared_statement =>
            return Stmt.mysql_conn.all.prep_SqlState (Stmt.stmt_handle);
      end case;
   end last_sql_state;


   ------------------------
   --  last_driver_code  --
   ------------------------
   overriding
   function last_driver_code (Stmt : MySQL_statement) return DriverCodes is
   begin
      case Stmt.type_of_statement is
         when direct_statement   =>
            return Stmt.mysql_conn.all.driverCode;
         when prepared_statement =>
            return Stmt.mysql_conn.all.prep_DriverCode (Stmt.stmt_handle);
      end case;
   end last_driver_code;


   --------------------
   --  clear_buffer  --
   --------------------
   procedure clear_buffer (Stmt : out MySQL_statement)
   is
      use type ABM.MYSQL_RES_Access;
      use type ABM.MYSQL_STMT_Access;
   begin
      case Stmt.type_of_statement is
         when direct_statement =>
            if Stmt.result_handle /= null then
               Stmt.mysql_conn.all.free_result (Stmt.result_handle);
            end if;
         when prepared_statement =>
            if Stmt.stmt_handle /= null then
               Stmt.mysql_conn.all.prep_free_result (Stmt.stmt_handle);
            end if;
      end case;
      Stmt.num_columns := 0;

      --  MORE
   end clear_buffer;


   ----------------------------
   --  execute  (version 1)  --
   ----------------------------
   overriding
   function execute (Stmt : MySQL_statement) return Boolean
   is
   begin
      if Stmt.type_of_statement = direct_statement then
         raise INVALID_FOR_DIRECT_QUERY
           with "execute is for prepared statements";
      end if;
      return False;
   end execute;


   ----------------------------
   --  execute  (version 2)  --
   ----------------------------
   overriding
   function execute (Stmt : MySQL_statement; bind_piped : String)
                     return Boolean
   is

   begin
      if Stmt.type_of_statement = direct_statement then
         raise INVALID_FOR_DIRECT_QUERY
           with "execute is for prepared statements";
      end if;
      return False;
   end execute;

   ------------------
   --  initialize  --
   ------------------
   overriding
   procedure initialize (Object : in out MySQL_statement)
   is
   begin
      logger_access     := Object.log_handler;
      Object.dialect    := driver_mysql;
      Object.connection := ACB.Base_Connection_Access (Object.mysql_conn);
      Object.sql_final := new String (Object.query_access'Range);
      case Object.type_of_statement is
         when direct_statement =>
            Object.sql_final.all := Object.query_access.all;
            Object.internal_execute;
         when prepared_statement =>
            Object.transform_sql (sql => Object.query_access.all,
                                  new_sql => Object.sql_final.all);
      end case;
   end initialize;


   ---------------------
   --  direct_result  --
   ---------------------
   procedure direct_result (Stmt    : in out MySQL_statement;
                            present : out Boolean)
   is
      use type ABM.MYSQL_RES_Access;
   begin
      case Stmt.con_buffered is
         when True => Stmt.mysql_conn.all.store_result
              (result_handle => Stmt.result_handle);
         when False => Stmt.mysql_conn.all.use_result
              (result_handle => Stmt.result_handle);
      end case;
      present := Stmt.result_handle /= null;
   end direct_result;


   ---------------------
   --  rows_returned  --
   ---------------------
   overriding
   function rows_returned (Stmt : MySQL_statement) return AffectedRows
   is
   begin
      if not Stmt.successful_execution then
         raise PRIOR_EXECUTION_FAILED
           with "Has query been executed yet?";
      end if;
      if Stmt.result_present then
         if Stmt.con_buffered then
            return Stmt.size_of_rowset;
         else
            raise INVALID_FOR_RESULT_SET
              with "Row set size is not known (Use query buffers to fix)";
         end if;
      else
         raise INVALID_FOR_RESULT_SET
           with "Result set not found; use rows_affected";
      end if;
   end rows_returned;


   -------------------------------
   --  scan_column_information  --
   -------------------------------
   procedure scan_column_information (Stmt : in out MySQL_statement)
   is
      use type ABM.MYSQL_FIELD_Access;
      field : ABM.MYSQL_FIELD_Access;
      function fn (raw : String) return stmttext;
      function fn (raw : String) return stmttext is
      begin
         case Stmt.con_case_mode is
            when upper_case =>
               return SU.To_Unbounded_String (ACH.To_Upper (raw));
            when lower_case =>
               return SU.To_Unbounded_String (ACH.To_Lower (raw));
            when natural_case =>
               return SU.To_Unbounded_String (raw);
         end case;
      end fn;
   begin
      loop
         field := Stmt.mysql_conn.all.fetch_field
           (result_handle => Stmt.result_handle);
         exit when field = null;
         declare
            info : column_info;
         begin
            info.field_name    := fn (Stmt.mysql_conn.all.field_name_field
                                      (field => field));
            info.null_possible := Stmt.mysql_conn.all.field_allows_null
              (field => field);
            info.mysql_type    := field.field_type;
            Stmt.mysql_conn.all.field_data_type
              (field    => field,
               std_type => info.field_type,
               size     => info.field_size);
            Stmt.column_info.Append (New_Item => info);
         end;
      end loop;
   end scan_column_information;


   ----------------------------------
   --  internal_execute  (direct)  --
   ----------------------------------
   procedure internal_execute (Stmt : in out MySQL_statement)
   is
      --  num_columns is already 0, success_execution is already false
   begin
      Stmt.connection.all.execute (sql => Stmt.sql_final.all);
      Stmt.log_nominal (category => statement_execution,
                        message => Stmt.sql_final.all);
      Stmt.direct_result (present => Stmt.result_present);
      Stmt.successful_execution := True;
      if Stmt.result_present then
         Stmt.num_columns := Stmt.mysql_conn.all.fields_in_result
                               (Stmt.result_handle);
         if Stmt.con_buffered then
            Stmt.size_of_rowset := Stmt.mysql_conn.all.rows_in_result
                                     (Stmt.result_handle);
         end if;
         Stmt.scan_column_information;
      else
         declare
            returned_cols : Natural;
         begin
            returned_cols := Stmt.mysql_conn.all.field_count;
            if returned_cols = 0 then
               Stmt.impacted := Stmt.mysql_conn.rows_affected_by_execution;
            else
               raise ACM.RESULT_FAIL with "Columns returned without result";
            end if;
         end;
      end if;

   exception
      when ACM.QUERY_FAIL =>
         Stmt.log_problem (category   => statement_execution,
                           message    => Stmt.sql_final.all,
                           pull_codes => True);
      when RES : ACM.RESULT_FAIL =>
         Stmt.log_problem (category   => statement_execution,
                           message    => EX.Exception_Message (X => RES),
                           pull_codes => True);
   end internal_execute;

end AdaBase.Statement.Base.MySQL;
