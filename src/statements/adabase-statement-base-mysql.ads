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

with AdaBase.Connection.Base.MySQL;
with AdaBase.Bindings.MySQL;
with AdaBase.Results.Sets;
with Ada.Containers.Vectors;

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
   function execute         (Stmt : MySQL_statement) return Boolean;

   overriding
   function execute         (Stmt : MySQL_statement; bind_piped : String)
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
   function fetch_next (Stmt : MySQL_statement) return ARS.DataRow_Access;

   overriding
   function fetch_all  (Stmt : MySQL_statement) return ARS.DataRowSet;

   overriding
   function fetch_bound (Stmt : MySQL_statement) return Boolean;

private

   procedure initialize (Object : in out MySQL_statement);
   procedure internal_execute (Stmt : in out MySQL_statement);
   procedure direct_result (Stmt    : in out MySQL_statement;
                            present : out Boolean);
   procedure scan_column_information (Stmt : in out MySQL_statement);
   function internal_fetch_bound_direct (Stmt : MySQL_statement)
                                         return Boolean;
   function internal_fetch_row_direct (Stmt : MySQL_statement)
                                       return ARS.DataRow_Access;
   function convert (nv : String) return CAL.Time;
   function convert (nv : String) return AR.settype;

   type column_info is record
      table         : stmttext;
      field_name    : stmttext;
      field_type    : field_types;
      field_size    : Natural;
      null_possible : Boolean;
      mysql_type    : ABM.enum_field_types;
   end record;

   type fetch_status is (pending, progressing, completed);

   package VColumns is new AC.Vectors (Index_Type   => Positive,
                                       Element_Type => column_info);

   --  The "cheat" exists to work around Ada2005 restriction of no "out"
   --  parameters for functions.  In some cases we need to adjust the
   --  main object as a result of running the function -- otherwise we
   --  have to switch to a procedure which is not desirable for fetching

   type cheat2005 is record
      delivery      : fetch_status          := completed;
      result_handle : ABM.MYSQL_RES_Access  := null;
   end record;
   type cheat_access is access all cheat2005;

   cheat_ada2005 : aliased cheat2005;

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
         stmt_handle    : ABM.MYSQL_STMT_Access := null;
         cheat          : cheat_access          := cheat_ada2005'Access;
         num_columns    : Natural               := 0;
         size_of_rowset : TraxID                := 0;
         column_info    : VColumns.Vector;
         sql_final      : access String;
      end record;

end AdaBase.Statement.Base.MySQL;
