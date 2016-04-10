--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body AdaBase.Connection.Base is

   ------------------
   --  autoCommit  --
   ------------------
   overriding
   function autoCommit (conn : Base_Connection) return Boolean
   is
   begin
      return conn.prop_auto_commit;
   end autoCommit;


   -------------------
   --  setCaseMode  --
   -------------------
   overriding
   procedure setCaseMode (conn : out Base_Connection; mode : CaseMode)
   is
   begin
      conn.prop_case_mode := mode;
   end setCaseMode;


   ----------------
   --  caseMode  --
   ----------------
   overriding
   function getCaseMode (conn : Base_Connection) return CaseMode
   is
   begin
      return conn.prop_case_mode;
   end getCaseMode;


   --------------------
   --  setErrorMode  --
   --------------------
   overriding
   procedure setErrorMode (conn : out Base_Connection; mode : ErrorMode)
   is
   begin
      conn.prop_error_mode := mode;
   end setErrorMode;


   -----------------
   --  ErrorMode  --
   -----------------
   overriding
   function getErrorMode (conn : Base_Connection) return ErrorMode
   is
   begin
      return conn.prop_error_mode;
   end getErrorMode;


   --------------------
   --  setMaxBlobSize  --
   --------------------
   overriding
   procedure setMaxBlobSize (conn    : out Base_Connection;
                             maxsize :     BLOB_maximum)
   is
   begin
      conn.prop_max_blob := maxsize;
   end setMaxBlobSize;


   -------------------
   --  maxBlobSize  --
   -------------------
   overriding
   function maxBlobSize (conn : Base_Connection) return BLOB_maximum
   is
   begin
      return conn.prop_max_blob;
   end maxBlobSize;


   ----------------------------
   --  transactionIsolation  --
   ----------------------------
   overriding
   function transactionIsolation (conn : Base_Connection)
                                  return TransIsolation
   is
   begin
      return conn.prop_trax_isolation;
   end transactionIsolation;


   -----------------
   --  connected  --
   -----------------
   overriding
   function connected (conn : Base_Connection) return Boolean
   is
   begin
      return conn.prop_active;
   end connected;


   ---------------------
   --  serverVersion  --
   ---------------------
   overriding
   function serverVersion (conn : Base_Connection)
                           return String
   is
   begin
      return USS (conn.info_server_version);
   end serverVersion;


   ------------------
   --  serverInfo  --
   ------------------
   overriding
   function serverInfo (conn : Base_Connection) return String
   is
   begin
      return USS (conn.info_server);
   end serverInfo;


   ----------------------
   --   clientVersion  --
   ----------------------
   overriding
   function clientVersion (conn : Base_Connection)
                           return String
   is
   begin
      return USS (conn.info_client_version);
   end clientVersion;


   ------------------
   --  clientInfo  --
   ------------------
   overriding
   function clientInfo (conn : Base_Connection) return String
   is
   begin
      return USS (conn.info_client);
   end clientInfo;

   ------------------------------------------------------------------------
   --  PRIVATE ROUTINES NOT COVERED BY INTERFACES                        --
   ------------------------------------------------------------------------

   -----------
   --  SUS  --
   -----------
   function SUS (fixed : String) return conntext
   is
   begin
      return SU.To_Unbounded_String (Source => fixed);
   end SUS;


   -----------
   --  USS  --
   -----------
   function USS (loose : conntext) return String
   is
   begin
      return SU.To_String (Source => loose);
   end USS;


end AdaBase.Connection.Base;
