--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Statement;

package AdaBase.Interfaces.Connection is

   type iConnection is interface;

   package AS renames AdaBase.Statement;

   --  Autocommit
   procedure setAutoCommit (conn : out iConnection; auto : Boolean) is null;
   function     autoCommit (conn : iConnection) return Boolean  is abstract;


   --  Column Header Case Mode
   procedure setCaseMode (conn : out iConnection; mode : CaseMode) is null;
   function  getCaseMode (conn : iConnection) return CaseMode is abstract;


   --  Set Compression Mode (if supported)
   procedure setCompressed (conn : out iConnection; compressed : Boolean)
                            is null;
   function     compressed (conn : iConnection) return Boolean is abstract;


   --  Set Buffered Queries (aka prefetch, if supported)
   procedure setUseBuffer (conn : out iConnection; buffered : Boolean)
                           is null;
   function     useBuffer (conn : iConnection) return Boolean is abstract;


   --  Set processing of multiple statements per query (if supported)
   procedure setMultiQuery (conn : out iConnection; multiple : Boolean)
                            is null;
   function     multiquery (conn : iConnection) return Boolean is abstract;


   --  Set maximum size of result that buffer must accommodate (if supported)
   procedure setMaxBlobSize (conn    : out iConnection;
                             maxsize :     BLOB_maximum) is null;
   function     maxBlobSize (conn : iConnection) return BLOB_maximum
                             is abstract;

   --  Set transaction Isolation level
   procedure setTransactionIsolation (conn : out iConnection;
                                      isolation : TransIsolation) is null;
   function     transactionIsolation (conn : iConnection)
                                      return TransIsolation is abstract;

   --  properties
   function serverVersion (conn : iConnection) return String
                           is abstract;
   function serverInfo    (conn : iConnection) return String
                           is abstract;
   function clientVersion (conn : iConnection) return String
                           is abstract;
   function clientInfo    (conn : iConnection) return String
                           is abstract;
   function description   (conn : iConnection) return String
                           is abstract;
   function connected     (conn : iConnection) return Boolean
                           is abstract;

   --  Error information associated with last query
   function SqlState      (conn : iConnection) return TSqlState
                           is abstract;
   function driverMessage (conn : iConnection) return String
                          is abstract;
   function driverCode    (conn : iConnection) return DriverCodes
                           is abstract;

   --  Information associated with previous successful query
   function lastInsertID  (conn : iConnection) return TraxID
                           is abstract;

   function rows_affected_by_execution (conn : iConnection)
                                        return AffectedRows is abstract;

   --  Commands
   procedure commit       (conn : out iConnection) is null;
   procedure rollback     (conn : out iConnection) is null;
   procedure disconnect   (conn : out iConnection) is null;
   procedure execute      (conn : out iConnection; sql : String) is null;

   procedure connect (conn     : out iConnection;
                      database : String;
                      username : String := blankstring;
                      password : String := blankstring;
                      hostname : String := blankstring;
                      socket   : String := blankstring;
                      port     : PosixPort := portless) is null;

end AdaBase.Interfaces.Connection;
