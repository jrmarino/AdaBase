--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Characters.Handling;

package body AdaBase.Statement.Base.SQLite is

   package ACH renames Ada.Characters.Handling;

   ---------------------
   --  num_set_items  --
   ---------------------
   function num_set_items (nv : String) return Natural
   is
      result : Natural := 0;
   begin
      if not CT.IsBlank (nv) then
         result := 1;
         for x in nv'Range loop
            if nv (x) = ',' then
               result := result + 1;
            end if;
         end loop;
      end if;
      return result;
   end num_set_items;


   -------------------
   --  log_problem  --
   -------------------
   procedure log_problem
     (statement  : SQLite_statement;
      category   : LogCategory;
      message    : String;
      pull_codes : Boolean := False;
      break      : Boolean := False)
   is
      error_msg  : CT.Text     := CT.blank;
      error_code : DriverCodes := 0;
      sqlstate   : TSqlState   := stateless;
   begin
      if pull_codes then
         error_msg  := CT.SUS (statement.last_driver_message);
         error_code := statement.last_driver_code;
         sqlstate   := statement.last_sql_state;
      end if;

      logger_access.all.log_problem
          (driver     => statement.dialect,
           category   => category,
           message    => CT.SUS (message),
           error_msg  => error_msg,
           error_code => error_code,
           sqlstate   => sqlstate,
           break      => break);
   end log_problem;


   --------------------
   --  column_count  --
   --------------------
   overriding
   function column_count (Stmt : SQLite_statement) return Natural is
   begin
      return Stmt.num_columns;
   end column_count;


   -------------------
   --  column_name  --
   -------------------
   overriding
   function column_name (Stmt : SQLite_statement; index : Positive)
                         return String
   is
      maxlen : constant Natural := Natural (Stmt.column_info.Length);
   begin
      if index > maxlen then
         raise INVALID_COLUMN_INDEX with "Max index is" & maxlen'Img &
           " but" & index'Img & " attempted";
      end if;
      return CT.USS (Stmt.column_info.Element (Index => index).field_name);
   end column_name;


   --------------------
   --  column_table  --
   --------------------
   overriding
   function column_table (Stmt : SQLite_statement; index : Positive)
                          return String
   is
      maxlen : constant Natural := Natural (Stmt.column_info.Length);
   begin
      if index > maxlen then
         raise INVALID_COLUMN_INDEX with "Max index is" & maxlen'Img &
           " but" & index'Img & " attempted";
      end if;
      return CT.USS (Stmt.column_info.Element (Index => index).table);
   end column_table;


   ------------------
   --  initialize  --
   ------------------
   overriding
   procedure initialize (Object : in out SQLite_statement)
   is
      use type ACS.SQLite_Connection_Access;
      len : Natural := CT.len (Object.initial_sql.all);
      logcat : LogCategory;
   begin

      if Object.sqlite_conn = null then
         return;
      end if;

      logger_access     := Object.log_handler;
      Object.dialect    := driver_sqlite;
      Object.sql_final  := new String (1 .. len);
      Object.connection := ACB.Base_Connection_Access (Object.sqlite_conn);

      case Object.type_of_statement is
         when direct_statement =>
            Object.sql_final.all := Object.initial_sql.all;
            logcat := statement_execution;
         when prepared_statement =>
            Object.sql_final.all :=
              Object.transform_sql (Object.initial_sql.all);
            logcat := statement_preparation;
      end case;

      if Object.sqlite_conn.prepare_statement (stmt => Object.stmt_handle,
                                               sql  => Object.sql_final.all)
      then
         Object.log_nominal (category => logcat,
                             message  => Object.sql_final.all);
      else
         raise STMT_PREPARATION
           with "Failed to parse a direct SQL query";
      end if;

      if Object.type_of_statement = prepared_statement then
         --  Check that we have as many markers as expected
         declare
            params : Natural :=
              Object.sqlite_conn.prep_markers_found (Object.stmt_handle);
            errmsg : String := "marker mismatch," &
              Object.realmccoy.Length'Img & " expected but" &
              params'Img & " found by SQLite";
         begin
            if params /= Natural (Object.realmccoy.Length) then
               raise ILLEGAL_BIND_SQL with errmsg;
            end if;
         end;
      end if;

      Object.scan_column_information;

   exception
      when HELL : others =>
         Object.log_problem
           (category => statement_preparation,
            message  => ACS.EX.Exception_Message (HELL));
         raise;
   end initialize;


   --------------------------------
   --  clear_column_information  --
   --------------------------------
   procedure clear_column_information  (Stmt : out SQLite_statement) is
   begin
      Stmt.num_columns := 0;
      Stmt.column_info.Clear;
      Stmt.crate.Clear;
      Stmt.headings_map.Clear;
      --  Stmt.reclaim_canvas;
   end clear_column_information;


   -------------------------------
   --  scan_column_information  --
   -------------------------------
   procedure scan_column_information (Stmt : out SQLite_statement)
   is
      function fn (raw : String) return CT.Text;
      function sn (raw : String) return String;
      function fn (raw : String) return CT.Text is
      begin
         case Stmt.con_case_mode is
            when upper_case =>
               return CT.SUS (ACH.To_Upper (raw));
            when lower_case =>
               return CT.SUS (ACH.To_Lower (raw));
            when natural_case =>
               return CT.SUS (raw);
         end case;
      end fn;
      function sn (raw : String) return String is
      begin
         case Stmt.con_case_mode is
            when upper_case =>
               return ACH.To_Upper (raw);
            when lower_case =>
               return ACH.To_Lower (raw);
            when natural_case =>
               return raw;
         end case;
      end sn;

      conn : ACS.SQLite_Connection_Access renames Stmt.sqlite_conn;
   begin
      Stmt.num_columns := conn.fields_in_result (Stmt.stmt_handle);
      for index in Natural range 0 .. Stmt.num_columns - 1 loop
         declare
            info  : column_info;
            brec  : bindrec;
            name  : String := conn.field_name (Stmt.stmt_handle, index);
            table : String := conn.field_table (Stmt.stmt_handle, index);
            dbase : String := conn.field_database (Stmt.stmt_handle, index);
         begin
            brec.v00          := False;   --  placeholder
            info.field_name   := fn (name);
            info.table        := fn (table);

            conn.get_field_meta_data (stmt      => Stmt.stmt_handle,
                                      database  => dbase,
                                      table     => table,
                                      column    => name,
                                      data_type => info.sqlite_type,
                                      nullable  => info.null_possible);

            Stmt.column_info.Append (New_Item => info);
            --  The following pre-populates for bind support
            Stmt.crate.Append (New_Item => brec);
            Stmt.headings_map.Insert (Key      => sn (name),
                                      New_Item => Stmt.crate.Last_Index);
         end;
      end loop;
   end scan_column_information;


   --------------------------
   --  column_native_type  --
   --------------------------
   overriding
   function column_native_type (Stmt : SQLite_statement; index : Positive)
                                return field_types
   is
      maxlen : constant Natural := Natural (Stmt.column_info.Length);
   begin
      if index > maxlen then
         raise INVALID_COLUMN_INDEX with "Max index is" & maxlen'Img &
           " but" & index'Img & " attempted";
      end if;
      --  SQLite is a special implementation where the data size is dynamic
      --  and not constrained to a column definition.  Thus, this function
      --  makes little sense for SQLite.  However, return something, the
      --  biggest possible type.

      case Stmt.column_info.Element (Index => index).sqlite_type is
         when BND.SQLITE_INTEGER => return ft_byte8;
         when BND.SQLITE_TEXT    => return ft_textual;
         when BND.SQLITE_BLOB    => return ft_chain;
         when BND.SQLITE_FLOAT   => return ft_real18;
         when BND.SQLITE_NULL    =>
            raise BINDING_TYPE_MISMATCH
              with "column type advertised as SQLITE_NULL";
      end case;
   end column_native_type;


   ----------------------
   --  last_insert_id  --
   ----------------------
   overriding
   function last_insert_id (Stmt : SQLite_statement) return TraxID
   is
      conn : ACS.SQLite_Connection_Access renames Stmt.sqlite_conn;
   begin
      return conn.lastInsertID;
   end last_insert_id;


   ----------------------
   --  last_sql_state  --
   ----------------------
   overriding
   function last_sql_state (Stmt : SQLite_statement) return TSqlState
   is
      conn : ACS.SQLite_Connection_Access renames Stmt.sqlite_conn;
   begin
      return conn.SqlState;
   end last_sql_state;


   ------------------------
   --  last_driver_code  --
   ------------------------
   overriding
   function last_driver_code (Stmt : SQLite_statement) return DriverCodes
   is
      conn : ACS.SQLite_Connection_Access renames Stmt.sqlite_conn;
   begin
      return conn.driverCode;
   end last_driver_code;


   ---------------------------
   --  last_driver_message  --
   ---------------------------
   overriding
   function last_driver_message (Stmt : SQLite_statement) return String
   is
      conn : ACS.SQLite_Connection_Access renames Stmt.sqlite_conn;
   begin
      return conn.driverMessage;
   end last_driver_message;


   ---------------------
   --  rows_returned  --
   ---------------------
   overriding
   function rows_returned (Stmt : SQLite_statement) return AffectedRows is
   begin
      --  Not supported by SQLite
      return 0;
   end rows_returned;


   --------------------
   --  discard_rest  --
   --------------------
   overriding
   procedure discard_rest (Stmt : out SQLite_statement)
   is
      conn : ACS.SQLite_Connection_Access renames Stmt.sqlite_conn;
   begin
      Stmt.rows_leftover := (Stmt.delivery /= completed);
      conn.reset_prep_stmt (stmt => Stmt.stmt_handle);
      Stmt.delivery := completed;
   end discard_rest;


   ---------------
   --  execute  --
   ---------------
   overriding
   function execute (Stmt : out SQLite_statement) return Boolean
   is
      conn : ACS.SQLite_Connection_Access renames Stmt.sqlite_conn;
   begin
--        if Stmt.virgin then
--           --  The statement has never been stepped and it's ready now
--           return;
--        end if;
--        conn.reset_prep_stmt;

      return True;
   end execute;


end AdaBase.Statement.Base.SQLite;
