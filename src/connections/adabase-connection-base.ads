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

with AdaBase.Interfaces.Connection;

package AdaBase.Connection.Base is

   package AIC renames AdaBase.Interfaces.Connection;

   type Base_Connection is abstract new Base_Pure and
                                        AIC.iConnection with private;
   type Base_Connection_Access is access all Base_Connection'Class;

   overriding
   function autoCommit (conn : Base_Connection) return Boolean;

   overriding
   procedure setCaseMode (conn : out Base_Connection; mode : CaseMode);

   overriding
   function getCaseMode (conn : Base_Connection) return CaseMode;

   overriding
   procedure setStringMode (conn : out Base_Connection; mode : StringMode);

   overriding
   procedure setErrorMode (conn : out Base_Connection; mode : ErrorMode);

   overriding
   function getErrorMode (conn : Base_Connection) return ErrorMode;

   overriding
   function getStringMode (conn : Base_Connection) return StringMode;

   overriding
   procedure setMaxBlobSize (conn : out Base_Connection;
                             maxsize : BLOB_maximum);

   overriding
   function maxBlobSize (conn : Base_Connection) return BLOB_maximum;

   overriding
   function transactionIsolation (conn : Base_Connection)
                                  return TransIsolation;

   overriding
   function serverVersion (conn : Base_Connection)
                           return String;

   overriding
   function serverInfo    (conn : Base_Connection)
                           return String;

   overriding
   function clientVersion (conn : Base_Connection)
                           return String;

   overriding
   function clientInfo    (conn : Base_Connection)
                           return String;

   overriding
   function connected     (conn : Base_Connection) return Boolean;


private

   type Base_Connection is abstract new Base_Pure and AIC.iConnection with
      record
         prop_auto_commit    : Boolean          := False;
         prop_active         : Boolean          := False;
         prop_trax_isolation : TransIsolation   := repeatable_read;
         prop_error_mode     : ErrorMode        := warning;
         prop_case_mode      : CaseMode         := natural_case;
         prop_string_mode    : StringMode       := return_null;
         prop_max_blob       : BLOB_maximum     := 2 ** 12;  -- 4kb

         info_server         : textual := blank;
         info_server_version : textual := blank;
         info_client         : textual := blank;
         info_client_version : textual := blank;
      end record;

   function SUS (fixed : String) return textual;
   function USS (loose : textual) return String;

end AdaBase.Connection.Base;
