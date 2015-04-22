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
with AdaBase.DataTypes;

package AdaBase.Connection.Base is

   package AIC renames AdaBase.Interfaces.Connection;
   package AD renames AdaBase.DataTypes;

   type Base_Connection is abstract new Base_Pure and
                                        AIC.iConnection with private;
   type Base_Connection_Access is access all Base_Connection'Class;

   overriding
   function autoCommit (conn : Base_Connection) return Boolean;

   overriding
   procedure setCaseMode (conn : out Base_Connection; mode : AD.CaseMode);

   overriding
   function caseMode (conn : Base_Connection) return AD.CaseMode;

   overriding
   procedure setStringMode (conn : out Base_Connection; mode : AD.StringMode);

   overriding
   procedure setErrorMode (conn : out Base_Connection; mode : AD.ErrorMode);

   overriding
   function ErrorMode (conn : Base_Connection) return AD.ErrorMode;

   overriding
   function stringMode (conn : Base_Connection) return AD.StringMode;

   overriding
   procedure setMaxBlobSize (conn : out Base_Connection;
                             maxsize : AD.BLOB_maximum);

   overriding
   function maxBlobSize (conn : Base_Connection) return AD.BLOB_maximum;

   overriding
   function transactionIsolation (conn : Base_Connection)
                                  return AD.TransIsolation;

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
         prop_connected      : Boolean             := False;
         prop_auto_commit    : Boolean             := False;
         prop_active         : Boolean             := False;
         prop_trax_isolation : AD.TransIsolation   := AD.repeatable_read;
         prop_error_mode     : AD.ErrorMode        := AD.warning;
         prop_case_mode      : AD.CaseMode         := AD.natural_case;
         prop_string_mode    : AD.StringMode       := AD.return_null;
         prop_max_blob       : AD.BLOB_maximum     := 2 ** 12;  -- 4kb

         info_server         : AD.textual := AD.blank;
         info_server_version : AD.textual := AD.blank;
         info_client         : AD.textual := AD.blank;
         info_client_version : AD.textual := AD.blank;
      end record;

   function SUS (fixed : String) return AD.textual;
   function USS (loose : AD.textual) return String;

end AdaBase.Connection.Base;
