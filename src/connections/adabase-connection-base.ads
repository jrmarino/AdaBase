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
   procedure setCaseMode (conn : out Base_Connection; mode : Case_Modes);

   overriding
   function getCaseMode (conn : Base_Connection) return Case_Modes;

   overriding
   procedure setMaxBlobSize (conn : out Base_Connection;
                             maxsize : BLOB_Maximum);

   overriding
   function maxBlobSize (conn : Base_Connection) return BLOB_Maximum;

   overriding
   function transactionIsolation (conn : Base_Connection)
                                  return Trax_Isolation;

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

   function utf8_encoding (conn : Base_Connection) return Boolean;

private

   type Base_Connection is abstract new Base_Pure and AIC.iConnection with
      record
         prop_auto_commit    : Boolean          := False;
         prop_active         : Boolean          := False;
         prop_trax_isolation : Trax_Isolation   := repeatable_read;
         prop_case_mode      : Case_Modes       := natural_case;
         prop_max_blob       : BLOB_Maximum     := 2 ** 12;  -- 4kb

         encoding_is_utf8    : Boolean := True;
         character_set       : CT.Text := CT.SUS ("UTF8");
         info_server         : CT.Text := CT.blank;
         info_server_version : CT.Text := CT.blank;
         info_client         : CT.Text := CT.blank;
         info_client_version : CT.Text := CT.blank;
      end record;

   overriding
   procedure finalize (conn : in out Base_Connection) is null;

end AdaBase.Connection.Base;
