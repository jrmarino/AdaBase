--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Interfaces.Connection;
with AdaBase.Bindings.SQLite;
with AdaBase.Results;
with Ada.Exceptions;

package AdaBase.Connection.Base.SQLite is

   package AIC renames AdaBase.Interfaces.Connection;
   package BND renames AdaBase.Bindings.SQLite;
   package AR  renames AdaBase.Results;
   package EX  renames Ada.Exceptions;

   type SQLite_Connection is new Base_Connection and AIC.iConnection
   with private;
   type SQLite_Connection_Access is access all SQLite_Connection;

   -----------------------------------------
   --  SUBROUTINES REQUIRED BY INTERFACE  --
   -----------------------------------------

   overriding
   procedure connect      (conn     : out SQLite_Connection;
                           database : String;
                           username : String := blankstring;
                           password : String := blankstring;
                           hostname : String := blankstring;
                           socket   : String := blankstring;
                           port     : Posix_Port := portless);

   overriding
   procedure setCompressed (conn : out SQLite_Connection; compressed : Boolean);

   overriding
   function compressed     (conn : SQLite_Connection) return Boolean;

   overriding
   procedure setUseBuffer  (conn : out SQLite_Connection; buffered : Boolean);

   overriding
   function useBuffer      (conn : SQLite_Connection) return Boolean;

   overriding
   procedure setMultiQuery (conn : out SQLite_Connection; multiple : Boolean);

   overriding
   function multiquery     (conn : SQLite_Connection) return Boolean;

   overriding
   procedure setAutoCommit (conn : out SQLite_Connection; auto : Boolean);

   overriding
   function description    (conn : SQLite_Connection) return String;

   overriding
   function SqlState       (conn : SQLite_Connection) return SQL_State;

   overriding
   function driverMessage  (conn : SQLite_Connection) return String;

   overriding
   function driverCode     (conn : SQLite_Connection) return Driver_Codes;

   overriding
   function lastInsertID   (conn : SQLite_Connection) return Trax_ID;

   overriding
   procedure commit        (conn : out SQLite_Connection);

   overriding
   procedure rollback      (conn : out SQLite_Connection);

   overriding
   procedure disconnect    (conn : out SQLite_Connection);

   overriding
   procedure execute       (conn : out SQLite_Connection; sql : String);

   overriding
   function rows_affected_by_execution (conn : SQLite_Connection)
                                        return Affected_Rows;

   overriding
   procedure setTransactionIsolation (conn : out SQLite_Connection;
                                      isolation : Trax_Isolation);

   overriding
   procedure set_character_set (conn : out SQLite_Connection;
                                charset : String);

   -----------------------------------------------
   --  SUBROUTINES PARTICULAR TO SQLITE DRIVER  --
   -----------------------------------------------

   function prepare_statement (conn : out SQLite_Connection;
                               stmt : aliased out BND.sqlite3_stmt_Access;
                               sql  : String) return Boolean;

   function prep_markers_found (conn : SQLite_Connection;
                                stmt : BND.sqlite3_stmt_Access) return Natural;

   function fields_in_result (conn : SQLite_Connection;
                              stmt : BND.sqlite3_stmt_Access) return Natural;

   function retrieve_integer (conn  : SQLite_Connection;
                              stmt  : BND.sqlite3_stmt_Access;
                              index : Natural) return AR.Byte8;

   function retrieve_double  (conn  : SQLite_Connection;
                              stmt  : BND.sqlite3_stmt_Access;
                              index : Natural) return AR.Real18;

   function retrieve_text    (conn  : SQLite_Connection;
                              stmt  : BND.sqlite3_stmt_Access;
                              index : Natural) return AR.Textual;

   function retrieve_blob    (conn  : SQLite_Connection;
                              stmt  : BND.sqlite3_stmt_Access;
                              index : Natural;
                              maxsz : Natural) return String;

   function field_is_null    (conn  : SQLite_Connection;
                              stmt  : BND.sqlite3_stmt_Access;
                              index : Natural) return Boolean;

   function field_database (conn  : SQLite_Connection;
                            stmt  : BND.sqlite3_stmt_Access;
                            index : Natural) return String;

   function field_table    (conn  : SQLite_Connection;
                            stmt  : BND.sqlite3_stmt_Access;
                            index : Natural) return String;

   function field_name     (conn  : SQLite_Connection;
                            stmt  : BND.sqlite3_stmt_Access;
                            index : Natural) return String;

   function field_true_name      (conn  : SQLite_Connection;
                                  stmt  : BND.sqlite3_stmt_Access;
                                  index : Natural) return String;

   procedure get_field_meta_data (conn  : SQLite_Connection;
                                  stmt  : BND.sqlite3_stmt_Access;
                                  database  : String;
                                  table     : String;
                                  column    : String;
                                  data_type : out BND.enum_field_types;
                                  nullable  : out Boolean);

   procedure reset_prep_stmt (conn  : SQLite_Connection;
                              stmt  : BND.sqlite3_stmt_Access);

   function prep_finalize    (conn  : SQLite_Connection;
                              stmt  : BND.sqlite3_stmt_Access) return Boolean;

   function prep_fetch_next  (conn  : SQLite_Connection;
                              stmt  : BND.sqlite3_stmt_Access) return Boolean;

   function marker_is_null    (conn  : SQLite_Connection;
                               stmt  : BND.sqlite3_stmt_Access;
                               index : Natural) return Boolean;

   function marker_is_integer (conn  : SQLite_Connection;
                               stmt  : BND.sqlite3_stmt_Access;
                               index : Natural;
                               value : AR.Byte8) return Boolean;

   function marker_is_double  (conn  : SQLite_Connection;
                               stmt  : BND.sqlite3_stmt_Access;
                               index : Natural;
                               value : AR.Real18) return Boolean;

   function marker_is_text    (conn  : SQLite_Connection;
                               stmt  : BND.sqlite3_stmt_Access;
                               index : Natural;
                               value : String;
                               cstr  : out BND.ICS.chars_ptr) return Boolean;

   function marker_is_blob    (conn  : SQLite_Connection;
                               stmt  : BND.sqlite3_stmt_Access;
                               index : Natural;
                               value : String;
                               chary : out BND.ICS.char_array_access)
                               return Boolean;

   ------------------
   --  EXCEPTIONS  --
   ------------------

   UNSUPPORTED_BY_SQLITE : exception;
   NOT_WHILE_CONNECTED   : exception;
   DISCONNECT_FAILED     : exception;
   CONNECT_FAILED        : exception;
   AUTOCOMMIT_FAIL       : exception;
   TRAX_BEGIN_FAIL       : exception;
   TRAXISOL_FAIL         : exception;
   ROLLBACK_FAIL         : exception;
   COMMIT_FAIL           : exception;
   QUERY_FAIL            : exception;
   METADATA_FAIL         : exception;
   STMT_NOT_VALID        : exception;
   STMT_RESET_FAIL       : exception;
   STMT_FETCH_FAIL       : exception;

private

   type SQLite_Connection is new Base_Connection and AIC.iConnection
     with record
      info_description : String (1 .. 21) := "SQLite3 native driver";
      prop_multiquery  : Boolean := False;
      dummy            : Boolean := False;
      handle           : aliased BND.sqlite3_Access := null;
   end record;

   function PUTF82S (cstr : BND.ICS.chars_ptr) return String;

   procedure begin_transaction (conn : out SQLite_Connection);

   procedure Initialize (conn : in out SQLite_Connection);

   overriding
   procedure finalize (conn : in out SQLite_Connection);

   procedure private_execute (conn : out SQLite_Connection; sql : String);

   function within_transaction (conn : SQLite_Connection) return Boolean;

end AdaBase.Connection.Base.SQLite;
