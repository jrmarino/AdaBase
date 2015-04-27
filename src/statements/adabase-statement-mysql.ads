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

with AdaBase.Interfaces.Statement;
with AdaBase.Bindings.MySQL;

package AdaBase.Statement.MySQL is

   package AIS renames AdaBase.Interfaces.Statement;
   package ABM renames AdaBase.Bindings.MySQL;

   type MySQL_statement is new Base and AIS.iStatement with private;

   overriding
   function successful  (Stmt : MySQL_statement) return Boolean;

   overriding
   function closeCursor (Stmt : MySQL_statement) return Boolean;

   overriding
   function columnCount (Stmt : MySQL_statement) return Integer;

   ------------------------------------------------
   --  These routines are not part of interface  --
   ------------------------------------------------

   ILLEGAL_TRANSFER : exception;

   procedure transfer_connection (Stmt        : out MySQL_statement;
                                  connection  : ABM.MYSQL_Access;
                                  error_mode  : ErrorMode;
                                  case_mode   : CaseMode;
                                  string_mode : StringMode;
                                  max_blob    : BLOB_maximum;
                                  buffered    : Boolean);

private
   type MySQL_statement is new Base and AIS.iStatement with record
      constructed     : Boolean := False;
      con_handle      : ABM.MYSQL_Access;
      con_error_mode  : ErrorMode;
      con_case_mode   : CaseMode;
      con_string_mode : StringMode;
      con_max_blob    : BLOB_maximum;
      con_buffered    : Boolean;

      stmt_handle     : ABM.MYSQL_STMT_Access := null;
   end record;

end AdaBase.Statement.MySQL;
