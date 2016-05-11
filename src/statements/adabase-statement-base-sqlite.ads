--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Connection.Base.SQLite;
with AdaBase.Bindings.SQLite;
with Ada.Containers.Vectors;

package AdaBase.Statement.Base.SQLite is

   package ACS renames AdaBase.Connection.Base.SQLite;
   package BND renames AdaBase.Bindings.SQLite;
   package ARS renames AdaBase.Results.Sets;
   package AC  renames Ada.Containers;

   type SQLite_statement (type_of_statement : stmt_type;
                          log_handler       : ALF.LogFacility_access;
                          sqlite_conn       : ACS.SQLite_Connection_Access;
                          initial_sql       : SQL_access;
                          con_error_mode    : ErrorMode;
                          con_case_mode     : CaseMode;
                          con_max_blob      : BLOB_maximum)
   is new Base_Statement and AIS.iStatement with private;
   type SQLite_statement_access is access all SQLite_statement;

   overriding
   function column_count (Stmt : SQLite_statement) return Natural;

   overriding
   function last_insert_id (Stmt : SQLite_statement) return TraxID;

   overriding
   function last_sql_state (Stmt : SQLite_statement) return TSqlState;

   overriding
   function last_driver_code (Stmt : SQLite_statement) return DriverCodes;

   overriding
   function last_driver_message (Stmt : SQLite_statement) return String;

   overriding
   procedure discard_rest   (Stmt : out SQLite_statement);

   overriding
   function execute         (Stmt : out SQLite_statement) return Boolean;

   overriding
   function execute         (Stmt : out SQLite_statement; parameters : String;
                             delimiter  : Character := '|') return Boolean;

   overriding
   function rows_returned   (Stmt : SQLite_statement) return AffectedRows;

   overriding
   function column_name     (Stmt : SQLite_statement; index : Positive)
                             return String;

   overriding
   function column_table    (Stmt : SQLite_statement; index : Positive)
                             return String;

   overriding
   function column_native_type (Stmt : SQLite_statement; index : Positive)
                                return field_types;

   overriding
   function fetch_next (Stmt : out SQLite_statement) return ARS.DataRow;

   overriding
   function fetch_all  (Stmt : out SQLite_statement) return ARS.DataRowSet;

   overriding
   function fetch_bound (Stmt : out SQLite_statement) return Boolean;


private

   type column_info is record
      table         : CT.Text;
      field_name    : CT.Text;
      field_type    : field_types;
      null_possible : Boolean;
      sqlite_type   : BND.enum_field_types;
   end record;

   type fetch_status is (pending, progressing, completed);

   package VColumns is new AC.Vectors (Index_Type   => Positive,
                                       Element_Type => column_info);

   type SQLite_statement (type_of_statement : stmt_type;
                          log_handler       : ALF.LogFacility_access;
                          sqlite_conn       : ACS.SQLite_Connection_Access;
                          initial_sql       : SQL_access;
                          con_error_mode    : ErrorMode;
                          con_case_mode     : CaseMode;
                          con_max_blob      : BLOB_maximum)
   is new Base_Statement and AIS.iStatement with
      record
         stmt_handle    : aliased BND.sqlite3_stmt_Access := null;
         delivery       : fetch_status     := completed;
         virgin         : Boolean          := True;
         assign_counter : Natural          := 0;
         num_columns    : Natural          := 0;
         size_of_rowset : TraxID           := 0;
         column_info    : VColumns.Vector;
         sql_final      : SQL_access;
      end record;

   procedure log_problem
     (statement  : SQLite_statement;
      category   : LogCategory;
      message    : String;
      pull_codes : Boolean := False;
      break      : Boolean := False);

   procedure initialize (Object : in out SQLite_statement);
   procedure scan_column_information (Stmt : out SQLite_statement);
   procedure clear_column_information  (Stmt : out SQLite_statement);
   function num_set_items (nv : String) return Natural;


end AdaBase.Statement.Base.SQLite;
