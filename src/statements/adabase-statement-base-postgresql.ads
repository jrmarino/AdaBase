--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Containers;
with AdaBase.Connection.Base.PostgreSQL;
with AdaBase.Bindings.PostgreSQL;

package AdaBase.Statement.Base.PostgreSQL is

   package BND renames AdaBase.Bindings.PostgreSQL;
   package CON renames AdaBase.Connection.Base.PostgreSQL;
   package AC  renames Ada.Containers;

   type PostgreSQL_statement
     (type_of_statement : Stmt_Type;
      log_handler       : ALF.LogFacility_access;
      pgsql_conn        : CON.PostgreSQL_Connection_Access;
      identifier        : Trax_ID;
      initial_sql       : SQL_Access;
      con_error_mode    : Error_Modes;
      con_case_mode     : Case_Modes;
      con_max_blob      : BLOB_Maximum;
      con_buffered      : Boolean)
   is new Base_Statement and AIS.iStatement with private;
   type PostgreSQL_statement_access is access all PostgreSQL_statement;

   overriding
   function column_count (Stmt : PostgreSQL_statement) return Natural;

   overriding
   function last_insert_id (Stmt : PostgreSQL_statement) return Trax_ID;

   overriding
   function last_sql_state (Stmt : PostgreSQL_statement) return SQL_State;

   overriding
   function last_driver_code (Stmt : PostgreSQL_statement) return Driver_Codes;

   overriding
   function last_driver_message (Stmt : PostgreSQL_statement) return String;

   overriding
   procedure discard_rest (Stmt : out PostgreSQL_statement);

   overriding
   function execute   (Stmt : out PostgreSQL_statement) return Boolean;

   overriding
   function execute   (Stmt : out PostgreSQL_statement; parameters : String;
                       delimiter  : Character := '|') return Boolean;

   overriding
   function rows_returned (Stmt : PostgreSQL_statement) return Affected_Rows;

   overriding
   function column_name   (Stmt : PostgreSQL_statement; index : Positive)
                           return String;

   overriding
   function column_table  (Stmt : PostgreSQL_statement; index : Positive)
                           return String;

   overriding
   function column_native_type (Stmt : PostgreSQL_statement; index : Positive)
                                return field_types;

   overriding
   function fetch_next (Stmt : out PostgreSQL_statement) return ARS.Datarow;

   overriding
   function fetch_all  (Stmt : out PostgreSQL_statement)
                        return ARS.Datarow_Set;

   overriding
   function fetch_bound (Stmt : out PostgreSQL_statement) return Boolean;

   overriding
   procedure fetch_next_set (Stmt         : out PostgreSQL_statement;
                             data_present : out Boolean;
                             data_fetched : out Boolean);


private

   type column_info is record
      table         : CT.Text;
      field_name    : CT.Text;
      field_type    : field_types;
      field_size    : Natural;
      binary_format : Boolean;
   end record;

   package VColumns is new AC.Vectors (Index_Type   => Positive,
                                       Element_Type => column_info);

   type PostgreSQL_statement
     (type_of_statement : Stmt_Type;
      log_handler       : ALF.LogFacility_access;
      pgsql_conn        : CON.PostgreSQL_Connection_Access;
      identifier        : Trax_ID;
      initial_sql       : SQL_Access;
      con_error_mode    : Error_Modes;
      con_case_mode     : Case_Modes;
      con_max_blob      : BLOB_Maximum;
      con_buffered      : Boolean)
   is new Base_Statement and AIS.iStatement with
      record
         result_handle  : aliased BND.PGresult_Access := null;
         prepared_stmt  : aliased BND.PGresult_Access := null;
         stmt_allocated : Boolean                     := False;
         insert_prepsql : Boolean                     := False;
         insert_return  : Boolean                     := False;
         assign_counter : Natural                     := 0;
         num_columns    : Natural                     := 0;
         size_of_rowset : Trax_ID                     := 0;
         result_arrow   : Trax_ID                     := 0;
         last_inserted  : Trax_ID                     := 0;
         column_info    : VColumns.Vector;
         sql_final      : SQL_Access;
      end record;

   procedure log_problem
     (statement  : PostgreSQL_statement;
      category   : Log_Category;
      message    : String;
      pull_codes : Boolean := False;
      break      : Boolean := False);

   procedure initialize (Object : in out PostgreSQL_statement);
   procedure Adjust     (Object : in out PostgreSQL_statement);
   procedure finalize   (Object : in out PostgreSQL_statement);
   function reformat_markers (parameterized_sql : String) return String;
   procedure scan_column_information (Stmt : out PostgreSQL_statement;
                                      pgresult : BND.PGresult_Access);

   function assemble_datarow (Stmt : PostgreSQL_statement;
                              row_number : Trax_ID) return ARS.Datarow;

   function show_statement_name (Stmt : PostgreSQL_statement) return String;

   function convert_to_pg_style_binary (nv : AR.Chain) return AR.Textual;
   function bind_text_value (Stmt : PostgreSQL_statement; marker : Positive)
                             return AR.Textual;

end AdaBase.Statement.Base.PostgreSQL;
