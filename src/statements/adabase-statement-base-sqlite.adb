--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Results.Field;
with Ada.Characters.Handling;

package body AdaBase.Statement.Base.SQLite is

   package ARF renames AdaBase.Results.Field;
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
         Object.successful_execution := True;
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
      Object.delivery := progressing;

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

            case info.sqlite_type is
               when BND.SQLITE_INTEGER => info.field_type := ft_byte8;
               when BND.SQLITE_TEXT    => info.field_type := ft_textual;
               when BND.SQLITE_BLOB    => info.field_type := ft_chain;
               when BND.SQLITE_FLOAT   => info.field_type := ft_real18;
               when BND.SQLITE_NULL    => info.field_type := ft_nbyte0;
            end case;

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
      return Stmt.column_info.Element (Index => index).field_type;
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


   ------------------
   --  execute #1  --
   ------------------
   overriding
   function execute (Stmt : out SQLite_statement) return Boolean
   is
      conn : ACS.SQLite_Connection_Access renames Stmt.sqlite_conn;

      num_markers : constant Natural := Natural (Stmt.realmccoy.Length);
      status_successful : Boolean := True;
   begin
      if Stmt.type_of_statement = direct_statement then
         raise INVALID_FOR_DIRECT_QUERY
           with "The execute command is for prepared statements only";
      end if;
      Stmt.successful_execution := False;

      if not Stmt.virgin then
         conn.reset_prep_stmt (Stmt.stmt_handle);
         Stmt.delivery := progressing;
      end if;

      if num_markers > 0 then
         --  TO BE IMPLEMENTED
         null;
      else
         --  No binding required, just execute the prepared statement
         Stmt.log_nominal (category => statement_execution,
                           message => "Exec without bound parameters");

         Stmt.successful_execution := True;
      end if;

      return status_successful;

   end execute;


   ------------------
   --  execute #2  --
   ------------------
   overriding
   function execute (Stmt : out SQLite_statement; parameters : String;
                     delimiter  : Character := '|') return Boolean
   is
      pragma Unreferenced (Stmt);
      --  conn : ACS.SQLite_Connection_Access renames Stmt.sqlite_conn;
   begin
      --  TO BE IMPLEMENTED
      return True;
   end execute;

   ------------------
   --  fetch_next  --
   ------------------
   overriding
   function fetch_next (Stmt : out SQLite_statement) return ARS.DataRow
   is
      conn   : ACS.SQLite_Connection_Access renames Stmt.sqlite_conn;
   begin
      if Stmt.delivery = completed then
         return ARS.Empty_DataRow;
      end if;
      declare
         maxlen : constant Natural := Natural (Stmt.column_info.Length);
         result : ARS.DataRow;
      begin
         Stmt.virgin := False;
         if not conn.prep_fetch_next (Stmt.stmt_handle) then
            Stmt.delivery := completed;
            return ARS.Empty_DataRow;
         end if;
         for F in 1 .. maxlen loop
            declare
               field    : ARF.std_field;
               dvariant : ARF.variant;
               scol     : constant Natural := F - 1;
               last_one : constant Boolean := (F = maxlen);
               heading  : constant String := CT.USS
                          (Stmt.column_info.Element (Index => F).field_name);
               EN       : constant Boolean :=
                          conn.field_is_null (Stmt.stmt_handle, scol);
            begin
               case Stmt.column_info.Element (Index => F).field_type is
                  when ft_nbyte0  =>
                     --  This should never occur though
                     dvariant := (datatype => ft_nbyte0, v00 => False);
                  when ft_byte8   =>
                     dvariant :=
                       (datatype => ft_byte8,
                        v10 => conn.retrieve_integer (Stmt.stmt_handle, scol));
                  when ft_real18  =>
                     dvariant :=
                       (datatype => ft_real18,
                        v12 => conn.retrieve_double (Stmt.stmt_handle, scol));
                  when ft_textual =>
                     dvariant :=
                       (datatype => ft_textual,
                        v13 => conn.retrieve_text (Stmt.stmt_handle, scol));
                  when ft_chain   => null;
                  when others => raise INVALID_FOR_RESULT_SET
                       with "Impossible field type (internal bug??)";
               end case;
               case Stmt.column_info.Element (Index => F).field_type is
                  when ft_chain =>
                     field := ARF.spawn_field
                       (binob => ARC.convert
                          (conn.retrieve_blob
                               (stmt  => Stmt.stmt_handle,
                                index => scol,
                                maxsz => Stmt.con_max_blob)));
                  when ft_nbyte0 | ft_byte8 | ft_real18 | ft_textual =>
                     field := ARF.spawn_field (data => dvariant,
                                               null_data => EN);
                  when others => null;
               end case;
               result.push (heading    => heading,
                            field      => field,
                            last_field => last_one);
            end;
         end loop;
         return result;
      end;
   end fetch_next;


   ------------------
   --  fetch_bound --
   ------------------
   overriding
   function fetch_bound (Stmt : out SQLite_statement) return Boolean
   is
      pragma Unreferenced (Stmt);
   begin
      --  TO BE IMPLEMENTED
      return False;
   end fetch_bound;


   -----------------
   --  fetch_all  --
   -----------------
   overriding
   function fetch_all (Stmt : out SQLite_statement) return ARS.DataRowSet
   is
      subtype rack_range is Positive range 1 .. 1000000;
      type TRack is array (rack_range) of ARS.DataRow_Access;
      nullset      : ARS.DataRowSet (1 .. 0);
   begin
      if Stmt.delivery = completed then
         return nullset;
      end if;
      --  With SQLite, we don't know many rows of data are fetched, ever.
      --  For practical purposes, let's limit a result set to 1 million rows
      --  We'll create an access array and dynamically allocate memory for
      --  each row.  At the end, we'll copy the data to a properly sized
      --  array, free the memory and return the result.

      declare
         rack         : TRack;
         dataset_size : Natural    := 0;
         arrow        : rack_range := rack_range'First;
      begin
         loop
            rack (arrow) := new ARS.DataRow;
            rack (arrow).all := Stmt.fetch_next;
            exit when rack (arrow).data_exhausted;
            dataset_size := dataset_size + 1;
            if arrow = rack_range'Last then
               Stmt.discard_rest;
               exit;
            end if;
            arrow := arrow + 1;
         end loop;
         if dataset_size = 0 then
            --  nothing was fetched
            free_datarow (rack (arrow));
            return nullset;
         end if;

         declare
            returnset : ARS.DataRowSet (1 .. dataset_size);
         begin
            for x in returnset'Range loop
               returnset (x) := rack (x).all;
            end loop;
            for x in rack_range range rack_range'First .. arrow loop
               free_datarow (rack (x));
            end loop;
            return returnset;
         end;
      end;
   end fetch_all;


   --------------
   --  Adjust  --
   --------------
   overriding
   procedure Adjust (Object : in out SQLite_statement) is
   begin
      --  The stmt object goes through this evolution:
      --  A) created in private_prepare()
      --  B) copied to new object in prepare(), A) destroyed
      --  C) copied to new object in program, B) destroyed
      --  We don't want to take any action until C) is destroyed, so add a
      --  reference counter upon each assignment.  When finalize sees a
      --  value of "2", it knows it is the program-level statement and then
      --  it can release memory releases, but not before!
      Object.assign_counter := Object.assign_counter + 1;

      --  Since the finalization is looking for a specific reference
      --  counter, any further assignments would fail finalization, so
      --  just prohibit them outright.
      if Object.assign_counter > 2 then
         raise STMT_PREPARATION
           with "Statement objects cannot be re-assigned.";
      end if;
   end Adjust;


   ----------------
   --  finalize  --
   ----------------
   overriding
   procedure finalize (Object : in out SQLite_statement) is
   begin
      if Object.assign_counter /= 2 then
         return;
      end if;

      begin
         Object.sqlite_conn.prep_finalize (Object.stmt_handle);
      exception
         when others =>
            Object.log_problem
              (category   => statement_preparation,
               message    => "Deallocating statement resources",
               pull_codes => True);
      end;

      free_sql (Object.sql_final);
   end finalize;

end AdaBase.Statement.Base.SQLite;
