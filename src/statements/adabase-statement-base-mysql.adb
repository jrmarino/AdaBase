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

package body AdaBase.Statement.Base.MySQL is


   --------------------
   --  discard_rest  --
   --------------------
   overriding
   procedure discard_rest  (Stmt : out MySQL_statement;
                            was_complete : out Boolean)
   is
      use type ACM.MySQL_Connection_Access;
      use type ABM.MYSQL_STMT_Access;
   begin
      case Stmt.type_of_statement is
         when direct_statement   => was_complete := Stmt.con_handle = null;
         when prepared_statement => was_complete := Stmt.stmt_handle = null;
      end case;
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
            return Stmt.con_handle.all.driverMessage;
         when prepared_statement =>
            return Stmt.con_handle.all.prep_DriverMessage (Stmt.stmt_handle);
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
            return Stmt.con_handle.all.lastInsertID;
         when prepared_statement =>
            return Stmt.con_handle.all.prep_LastInsertID (Stmt.stmt_handle);
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
            return Stmt.con_handle.all.SqlState;
         when prepared_statement =>
            return Stmt.con_handle.all.prep_SqlState (Stmt.stmt_handle);
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
            return Stmt.con_handle.all.driverCode;
         when prepared_statement =>
            return Stmt.con_handle.all.prep_DriverCode (Stmt.stmt_handle);
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
            if Stmt.res_handle /= null then
               Stmt.con_handle.all.free_result (Stmt.res_handle);
            end if;
         when prepared_statement =>
            if Stmt.stmt_handle /= null then
               Stmt.con_handle.all.prep_free_result (Stmt.stmt_handle);
            end if;
      end case;
      Stmt.num_columns := 0;

      --  MORE
   end clear_buffer;


end AdaBase.Statement.Base.MySQL;
