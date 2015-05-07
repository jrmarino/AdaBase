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

with AdaBase.Results.Sets;

package AdaBase.Interfaces.Statement is

   type iStatement is limited interface;

   function successful    (Stmt : iStatement) return Boolean is abstract;
   function column_count  (Stmt : iStatement) return Natural is abstract;

   function last_driver_message (Stmt : iStatement) return String is abstract;
   function last_insert_id      (Stmt : iStatement) return TraxID is abstract;
   function last_sql_state      (Stmt : iStatement) return TSqlState
                                 is abstract;
   function last_driver_code    (Stmt : iStatement) return DriverCodes
                                 is abstract;

   procedure discard_rest   (Stmt : out iStatement;
                             was_complete : out Boolean) is null;

   function execute         (Stmt : iStatement) return Boolean is abstract;
   function execute         (Stmt : iStatement; bind_piped : String)
                             return Boolean is abstract;

   function rows_affected   (Stmt : iStatement)
                             return AffectedRows is abstract;

   function rows_returned   (Stmt : iStatement)
                             return AffectedRows is abstract;

   function column_name     (Stmt : iStatement; index : Positive)
                             return String is abstract;

   function column_table    (Stmt : iStatement; index : Positive)
                             return String is abstract;

   function column_native_type (Stmt : iStatement; index : Positive)
                                return field_types is abstract;

   function fetch_next      (Stmt : iStatement)
                             return AdaBase.Results.Sets.DataRow_Access
                             is abstract;

   function fetch_all       (Stmt : iStatement)
                             return AdaBase.Results.Sets.DataRowSet
                             is abstract;

   function fetch_bound     (Stmt : iStatement) return Boolean is abstract;

   --  bindValue  (variant)
   --  bindTimeValue (generic time)
   --  bindColumn
--  fetch_nextRowSet
--  fetch_bound

end AdaBase.Interfaces.Statement;
