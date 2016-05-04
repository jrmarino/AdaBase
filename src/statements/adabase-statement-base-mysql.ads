--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Connection.Base.MySQL;
with AdaBase.Bindings.MySQL;
with AdaBase.Results.Sets;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

package AdaBase.Statement.Base.MySQL is

   package ACM renames AdaBase.Connection.Base.MySQL;
   package ABM renames AdaBase.Bindings.MySQL;
   package ARS renames AdaBase.Results.Sets;
   package AC  renames Ada.Containers;

   type MySQL_statement (type_of_statement : stmt_type;
                         log_handler       : ALF.LogFacility_access;
                         mysql_conn        : ACM.MySQL_Connection_Access;
                         initial_sql       : stmttext_access;
                         con_error_mode    : ErrorMode;
                         con_case_mode     : CaseMode;
                         con_max_blob      : BLOB_maximum;
                         con_buffered      : Boolean)
   is limited new Base_Statement and AIS.iStatement with private;
   type MySQL_statement_access is access MySQL_statement;

   overriding
   function column_count (Stmt : MySQL_statement) return Natural;

   overriding
   function last_insert_id (Stmt : MySQL_statement) return TraxID;

   overriding
   function last_sql_state (Stmt : MySQL_statement) return TSqlState;

   overriding
   function last_driver_code (Stmt : MySQL_statement) return DriverCodes;

   overriding
   function last_driver_message (Stmt : MySQL_statement) return String;

   overriding
   procedure discard_rest   (Stmt : out MySQL_statement);

   overriding
   function execute         (Stmt : out MySQL_statement) return Boolean;

   overriding
   function execute         (Stmt : out MySQL_statement; bind_piped : String)
                             return Boolean;

   overriding
   function rows_returned   (Stmt : MySQL_statement) return AffectedRows;

   overriding
   function column_name     (Stmt : MySQL_statement; index : Positive)
                             return String;

   overriding
   function column_table    (Stmt : MySQL_statement; index : Positive)
                             return String;

   overriding
   function column_native_type (Stmt : MySQL_statement; index : Positive)
                                return field_types;

   overriding
   function fetch_next (Stmt    : out MySQL_statement;
                        datarow : out ARS.DataRow_Access) return Boolean;

   overriding
   function fetch_all  (Stmt : out MySQL_statement) return ARS.DataRowSet;

   overriding
   function fetch_bound (Stmt : out MySQL_statement) return Boolean;

   overriding
   procedure fetch_next_set (Stmt         : out MySQL_statement;
                             data_present : out Boolean;
                             data_fetched : out Boolean);

private

   type mysql_canvas;

   procedure initialize (Object : in out MySQL_statement);
   procedure finalize   (Object : in out MySQL_statement);
   procedure internal_post_prep_stmt   (Stmt   : out MySQL_statement);
   procedure internal_direct_post_exec (Stmt   : out MySQL_statement;
                                        newset : Boolean := False);
   procedure process_direct_result     (Stmt : out MySQL_statement);
   procedure scan_column_information   (Stmt : out MySQL_statement);
   procedure clear_column_information  (Stmt : out MySQL_statement);
   procedure construct_bind_slot (Stmt   : MySQL_statement;
                                  struct : out ABM.MYSQL_BIND;
                                  canvas : out mysql_canvas;
                                  marker : Positive);
   function internal_fetch_bound (Stmt : out MySQL_statement) return Boolean;
   function internal_fetch_row   (Stmt : out MySQL_statement)
                                  return ARS.DataRow_Access;

   function internal_ps_fetch_row (Stmt : out MySQL_statement)
                                   return ARS.DataRow_Access;

   function internal_ps_fetch_bound (Stmt : out MySQL_statement)
                                     return Boolean;

   function convert (nv : String) return CAL.Time;
   function convert (nv : String) return AR.settype;
   function bincopy (data : ABM.ICS.char_array_access;
                     datalen, max_size : Natural)
                     return String;
   function bincopy (data : ABM.ICS.char_array_access;
                     datalen, max_size : Natural;
                     hard_limit : Natural := 0)
                     return AR.chain;

   procedure log_problem
     (statement  : MySQL_statement;
      category   : LogCategory;
      message    : String;
      pull_codes : Boolean := False;
      break      : Boolean := False);

   type column_info is record
      table         : CT.Text;
      field_name    : CT.Text;
      field_type    : field_types;
      field_size    : Natural;
      null_possible : Boolean;
      mysql_type    : ABM.enum_field_types;
   end record;

   type fetch_status is (pending, progressing, completed);

   package VColumns is new AC.Vectors (Index_Type   => Positive,
                                       Element_Type => column_info);

   --  mysql_canvas is used by prepared statement execution
   --  The Ada types are converted to C types and stored in this record which
   --  MySQL finds through pointers.

   type mysql_canvas is record
      length        : aliased ABM.IC.unsigned_long := 0;
      is_null       : aliased ABM.my_bool          := 0;
      error         : aliased ABM.my_bool          := 0;
      buffer_uint8  : ABM.IC.unsigned_char         := 0;
      buffer_uint16 : ABM.IC.unsigned_short        := 0;
      buffer_uint32 : ABM.IC.unsigned              := 0;
      buffer_uint64 : ABM.IC.unsigned_long         := 0;
      buffer_int8   : ABM.IC.signed_char           := 0;
      buffer_int16  : ABM.IC.short                 := 0;
      buffer_int32  : ABM.IC.int                   := 0;
      buffer_int64  : ABM.IC.long                  := 0;
      buffer_float  : ABM.IC.C_float               := 0.0;
      buffer_double : ABM.IC.double                := 0.0;
      buffer_binary : ABM.ICS.char_array_access    := null;
      buffer_time   : ABM.MYSQL_TIME;
   end record;
   type mysql_canvases is array (Positive range <>) of aliased mysql_canvas;
   type mysql_canvases_Access is access all mysql_canvases;
   procedure free_canvas is new Ada.Unchecked_Deallocation
     (mysql_canvases, mysql_canvases_Access);
   procedure free_binary is new Ada.Unchecked_Deallocation
     (ABM.IC.char_array, ABM.ICS.char_array_access);

   type MySQL_statement (type_of_statement : stmt_type;
                         log_handler       : ALF.LogFacility_access;
                         mysql_conn        : ACM.MySQL_Connection_Access;
                         initial_sql       : stmttext_access;
                         con_error_mode    : ErrorMode;
                         con_case_mode     : CaseMode;
                         con_max_blob      : BLOB_maximum;
                         con_buffered      : Boolean)
   is limited new Base_Statement and AIS.iStatement with
      record
         delivery       : fetch_status          := completed;
         result_handle  : ABM.MYSQL_RES_Access  := null;
         stmt_handle    : ABM.MYSQL_STMT_Access := null;
         bind_canvas    : mysql_canvases_Access := null;
         num_columns    : Natural               := 0;
         size_of_rowset : TraxID                := 0;
         column_info    : VColumns.Vector;
         sql_final      : access String;
      end record;

end AdaBase.Statement.Base.MySQL;
