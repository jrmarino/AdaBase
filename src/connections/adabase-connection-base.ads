--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with CommonText;
with AdaBase.Interfaces.Connection;

package AdaBase.Connection.Base is

   package CT  renames CommonText;
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
   procedure setErrorMode (conn : out Base_Connection; mode : ErrorMode);

   overriding
   function getErrorMode (conn : Base_Connection) return ErrorMode;

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
         prop_max_blob       : BLOB_maximum     := 2 ** 12;  -- 4kb

         info_server         : CT.Text := CT.blank;
         info_server_version : CT.Text := CT.blank;
         info_client         : CT.Text := CT.blank;
         info_client_version : CT.Text := CT.blank;
      end record;

   overriding
   procedure finalize (conn : in out Base_Connection) is null;

end AdaBase.Connection.Base;
