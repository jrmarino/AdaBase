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

package body AdaBase.Statement.MySQL is

   overriding
   function closeCursor (Stmt : MySQL_statement) return Boolean
   is
   begin
      return True;
   end closeCursor;

   overriding
   function columnCount (Stmt : MySQL_statement) return Integer
   is
   begin
      return 5;
   end columnCount;


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
   --  transfer_connection  --
   ---------------------------
   procedure transfer_connection (Stmt        : out MySQL_statement;
                                  connection  : ABM.MYSQL_Access;
                                  error_mode  : AD.ErrorMode;
                                  case_mode   : AD.CaseMode;
                                  string_mode : AD.StringMode;
                                  max_blob    : AD.BLOB_maximum;
                                  buffered    : Boolean)
   is
   begin
      if Stmt.constructed then
         raise ILLEGAL_TRANSFER;
      end if;

      Stmt.con_handle      := connection;
      Stmt.con_error_mode  := error_mode;
      Stmt.con_case_mode   := case_mode;
      Stmt.con_string_mode := string_mode;
      Stmt.con_max_blob    := max_blob;
      Stmt.con_buffered    := buffered;
      Stmt.constructed     := True;
   end transfer_connection;




end AdaBase.Statement.MySQL;
