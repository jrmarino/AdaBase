--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body AdaBase.Statement.Base.PostgreSQL is

   ------------------------
   --  reformat_markers  --
   ------------------------
   function reformat_markers (parameterized_sql : String) return String
   is
      masked : String := CT.redact_quotes (parameterized_sql);
      cvslen : Natural := masked'Length;
   begin
      for x in masked'Range loop
         if masked (x) = ASCII.Query then
            --  Reserve enough for 9999 markers (limit 1600 on PgSQL)
            --  Trailing whitespace is truncated by the return
            cvslen := cvslen + 4;
         end if;
      end loop;
      declare
         canvas  : String (1 .. cvslen) := (others => ' ');
         polaris : Natural := 0;
         param   : Natural := 0;
      begin
         for x in masked'Range loop
            if masked (x) = ASCII.Query then
               param := param + 1;
               declare
                  marker : String := ASCII.Dollar & CT.int2str (param);
               begin
                  for y in marker'Range loop
                     polaris := polaris + 1;
                     canvas (polaris) := marker (y);
                  end loop;
               end;
            else
               polaris := polaris + 1;
               canvas (polaris) := parameterized_sql (x);
            end if;
         end loop;
         return canvas (1 .. polaris);
      end;
   end reformat_markers;


   --------------------
   --  column_count  --
   --------------------
   overriding
   function column_count (Stmt : PostgreSQL_statement) return Natural
   is
      conn : CON.PostgreSQL_Connection_Access renames Stmt.pgsql_conn;
   begin
      return conn.fields_count (Stmt.stmt_handle);
   end column_count;


   ----------------------
   --  last_insert_id  --
   ----------------------
   overriding
   function last_insert_id (Stmt : PostgreSQL_statement) return Trax_ID
   is
      conn : CON.PostgreSQL_Connection_Access renames Stmt.pgsql_conn;
   begin
      --  return conn.lastInsertID
      --  TO BE IMPLEMENTED (depends on connection implementation)
      return 0;
   end last_insert_id;


   ----------------------
   --  last_sql_state  --
   ----------------------
   overriding
   function last_sql_state (Stmt : PostgreSQL_statement) return SQL_State
   is
      conn : CON.PostgreSQL_Connection_Access renames Stmt.pgsql_conn;
   begin
      return conn.SqlState (Stmt.stmt_handle);
   end last_sql_state;


   ------------------------
   --  last_driver_code  --
   ------------------------
   overriding
   function last_driver_code (Stmt : PostgreSQL_statement) return Driver_Codes
   is
      conn : CON.PostgreSQL_Connection_Access renames Stmt.pgsql_conn;
   begin
      return conn.driverCode (Stmt.stmt_handle);
   end last_driver_code;


   ---------------------------
   --  last_driver_message  --
   ---------------------------
   overriding
   function last_driver_message (Stmt : PostgreSQL_statement) return String
   is
      conn : CON.PostgreSQL_Connection_Access renames Stmt.pgsql_conn;
   begin
      return conn.driverMessage (Stmt.stmt_handle);
   end last_driver_message;


   --------------------
   --  discard_rest  --
   --------------------
   overriding
   procedure discard_rest (Stmt : out PostgreSQL_statement)
   is
      --  When asynchronous command mode becomes supported, this procedure
      --  would free the pgres object and indicate data exhausted somehow.
      --  In the standard buffered mode, we can only simulate it.
   begin
      if Stmt.result_arrow < Stmt.size_of_rowset then
         Stmt.result_arrow := Stmt.size_of_rowset;
         Stmt.rows_leftover := True;
      end if;
   end discard_rest;


   ------------------
   --  execute #1  --
   ------------------
   overriding
   function execute (Stmt : out PostgreSQL_statement) return Boolean
   is
      conn : CON.PostgreSQL_Connection_Access renames Stmt.pgsql_conn;
      num_markers : constant Natural := Natural (Stmt.realmccoy.Length);
      status_successful : Boolean := True;
   begin
      if Stmt.type_of_statement = direct_statement then
         raise INVALID_FOR_DIRECT_QUERY
           with "The execute command is for prepared statements only";
      end if;
      Stmt.successful_execution := False;
      Stmt.rows_leftover := False;

      if num_markers > 0 then
         --  IMPLEMENT
         null;
      else
         --  No binding required, just execute the prepared statement
         Stmt.log_nominal (category => statement_execution,
                           message => "Exec without bound parameters");

--           begin
--           if Stmt.mysql_conn.prep_execute (Stmt.stmt_handle) then
--              Stmt.successful_execution := True;
--           else
--              Stmt.log_problem (category => statement_execution,
--                                message => "failed to exec prep stmt",
--                                pull_codes => True);
--              status_successful := False;
--           end if;
      end if;

      --  TO BE IMPLEMENTED
      return False;
   end execute;


   ------------------
   --  execute #2  --
   ------------------
   overriding
   function execute (Stmt : out PostgreSQL_statement; parameters : String;
                     delimiter  : Character := '|') return Boolean
   is
      function parameters_given return Natural;
      num_markers : constant Natural := Natural (Stmt.realmccoy.Length);

      function parameters_given return Natural
      is
         result : Natural := 1;
      begin
         for x in parameters'Range loop
            if parameters (x) = delimiter then
               result := result + 1;
            end if;
         end loop;
         return result;
      end parameters_given;
   begin
      if Stmt.type_of_statement = direct_statement then
         raise INVALID_FOR_DIRECT_QUERY
           with "The execute command is for prepared statements only";
      end if;

      if num_markers /= parameters_given then
         raise STMT_PREPARATION
           with "Parameter number mismatch, " & num_markers'Img &
           " expected, but" & parameters_given'Img & " provided.";
      end if;

      declare
         index : Natural := 1;
         arrow : Natural := parameters'First;
         scans : Boolean := False;
         start : Natural := 1;
         stop  : Natural := 0;
      begin
         for x in parameters'Range loop
            if parameters (x) = delimiter then
               if not scans then
                  Stmt.auto_assign (index, "");
               else
                  Stmt.auto_assign (index, parameters (start .. stop));
                  scans := False;
               end if;
               index := index + 1;
            else
               stop := x;
               if not scans then
                  start := x;
                  scans := True;
               end if;
            end if;
         end loop;
         if not scans then
            Stmt.auto_assign (index, "");
         else
            Stmt.auto_assign (index, parameters (start .. stop));
         end if;
      end;

      return Stmt.execute;
   end execute;


   ---------------------
   --  rows_returned  --
   ---------------------
   overriding
   function rows_returned (Stmt : PostgreSQL_statement) return Affected_Rows
   is
      conn : CON.PostgreSQL_Connection_Access renames Stmt.pgsql_conn;
   begin
      return conn.rows_impacted (Stmt.stmt_handle);
   end rows_returned;


   -------------------
   --  column_name  --
   -------------------
   overriding
   function column_name (Stmt : PostgreSQL_statement; index : Positive)
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
   function column_table  (Stmt : PostgreSQL_statement; index : Positive)
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


   --------------------------
   --  column_native_type  --
   --------------------------
   overriding
   function column_native_type (Stmt : PostgreSQL_statement; index : Positive)
                                return field_types
   is
      conn : CON.PostgreSQL_Connection_Access renames Stmt.pgsql_conn;
      pndx : Natural := index - 1;
   begin
      return conn.field_type (Stmt.stmt_handle, pndx);
   end column_native_type;


   ------------------
   --  fetch_next  --
   ------------------
   overriding
   function fetch_next (Stmt : out PostgreSQL_statement) return ARS.Datarow is
   begin
      if Stmt.result_arrow >= Stmt.size_of_rowset then
         return ARS.Empty_Datarow;
      end if;
      Stmt.result_arrow := Stmt.result_arrow + 1;
      return Stmt.assemble_datarow (row_number => Stmt.result_arrow);
   end fetch_next;


   -----------------
   --  fetch_all  --
   -----------------
   overriding
   function fetch_all (Stmt : out PostgreSQL_statement) return ARS.Datarow_Set
   is
      maxrows : Natural := Natural (Stmt.rows_returned);
      tmpset  : ARS.Datarow_Set (1 .. maxrows + 1);
      nullset : ARS.Datarow_Set (1 .. 0);
      index   : Natural := 1;
      row     : ARS.Datarow;
   begin
      if Stmt.result_arrow >= Stmt.size_of_rowset then
         return nullset;
      end if;

      declare
         remaining_rows : Trax_ID := Stmt.size_of_rowset - Stmt.result_arrow;
         return_set     : ARS.Datarow_Set (1 .. Natural (remaining_rows));
      begin
         for index in return_set'Range loop
            Stmt.result_arrow := Stmt.result_arrow + 1;
            return_set (index) := Stmt.assemble_datarow (Stmt.result_arrow);
         end loop;
         return return_set;
      end;
   end fetch_all;


   -------------------
   --  fetch_bound  --
   -------------------
   overriding
   function fetch_bound (Stmt : out PostgreSQL_statement) return Boolean
   is
      function null_value (column : Natural) return Boolean;
      function string_equivalent (column : Natural; binary : Boolean)
                                  return String;

      conn   : CON.PostgreSQL_Connection_Access renames Stmt.pgsql_conn;

      function string_equivalent (column : Natural; binary : Boolean)
                                  return String
      is
         --  PostgreSQL result set is zero-indexed
         row_num : constant Natural := Natural (Stmt.result_arrow) - 1;
         col_num : constant Natural := column - 1;
      begin
         if binary then
            return conn.field_binary (Stmt.stmt_handle, row_num, col_num,
                                      Stmt.con_max_blob);
         else
            return conn.field_string (Stmt.stmt_handle, row_num, col_num);
         end if;
      end string_equivalent;

      function null_value (column : Natural) return Boolean
      is
         --  PostgreSQL result set is zero-indexed
         row_num : constant Natural := Natural (Stmt.result_arrow) - 1;
         col_num : constant Natural := column - 1;
      begin
         return conn.field_is_null (Stmt.stmt_handle, row_num, col_num);
      end null_value;

   begin
      if Stmt.result_arrow >= Stmt.size_of_rowset then
         return False;
      end if;
      Stmt.result_arrow := Stmt.result_arrow + 1;

      declare
         maxlen : constant Natural := Stmt.num_columns;
      begin
         for F in 1 .. maxlen loop
            declare
               dossier  : bindrec renames Stmt.crate.Element (F);
               colinfo  : column_info renames Stmt.column_info.Element (F);
               Tout     : constant field_types := dossier.output_type;
               Tnative  : constant field_types := colinfo.field_type;
               isnull   : constant Boolean := null_value (F);
               errmsg   : constant String  := "native type : " &
                          field_types'Image (Tnative) & " binding type : " &
                          field_types'Image (Tout);
               smallerr : constant String  := "Native unsigned type : " &
                          field_types'Image (Tnative) & " is too small for " &
                          field_types'Image (Tout) & " binding type";
               ST       : constant String  :=
                          string_equivalent (F, colinfo.binary_format);
            begin
               if not dossier.bound then
                  goto continue;
               end if;

               --  Because PostgreSQL does not support unsigned integer
               --  types, allow binding NByteX binding to ByteX types, but
               --  remain strict on other type mismatches.

               case Tout is
                  when ft_nbyte1 =>
                     case Tnative is
                        when ft_byte1 | ft_byte2 | ft_byte3 | ft_byte4 |
                             ft_byte8 | ft_nbyte2 | ft_nbyte3 | ft_nbyte4 |
                             ft_nbyte8 =>
                           null;  -- Fall through (all could fail to convert)
                        when ft_nbyte1 =>
                           null;  -- guaranteed to convert
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_nbyte2 =>
                     case Tnative is
                        when ft_byte2 | ft_byte3 | ft_byte4 | ft_byte8 |
                             ft_nbyte3 | ft_nbyte4 | ft_nbyte8 =>
                           null;  -- Fall through (all could fail to convert)
                        when ft_nbyte1 | ft_nbyte2 =>
                           null;  -- guaranteed to convert
                        when ft_byte1 =>
                           raise BINDING_TYPE_MISMATCH with smallerr;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_nbyte3 =>
                     case Tnative is
                        when ft_byte3 | ft_byte4 | ft_byte8 | ft_nbyte4 |
                             ft_nbyte8 =>
                           null;  -- Fall through (all could fail to convert)
                        when ft_nbyte1 | ft_nbyte2 | ft_nbyte3 =>
                           null;  -- guaranteed to convert
                        when ft_byte1 | ft_byte2 =>
                           raise BINDING_TYPE_MISMATCH with smallerr;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_nbyte4 =>
                     case Tnative is
                        when ft_byte4 | ft_byte8 | ft_nbyte8 =>
                           null;  -- Fall through (all could fail to convert)
                        when ft_nbyte1 | ft_nbyte2 | ft_nbyte3 | ft_nbyte4 =>
                           null;  -- guaranteed to convert
                        when ft_byte1 | ft_byte2 | ft_byte3 =>
                           raise BINDING_TYPE_MISMATCH with smallerr;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_nbyte8 =>
                     case Tnative is
                        when ft_byte8 =>
                           null;  -- Fall through (could fail to convert)
                        when ft_nbyte1 | ft_nbyte2 | ft_nbyte3 | ft_nbyte4 |
                             ft_nbyte8 =>
                           null;  -- guaranteed to convert
                        when ft_byte1 | ft_byte2 | ft_byte3 | ft_byte4 =>
                           raise BINDING_TYPE_MISMATCH with smallerr;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when others =>
                     if Tnative /= Tout then
                        raise BINDING_TYPE_MISMATCH with errmsg;
                     end if;
               end case;

               case Tout is
                  when ft_nbyte0    => dossier.a00.all := (ST = "1");
                  when ft_nbyte1    => dossier.a01.all := convert (ST);
                  when ft_nbyte2    => dossier.a02.all := convert (ST);
                  when ft_nbyte3    => dossier.a03.all := convert (ST);
                  when ft_nbyte4    => dossier.a04.all := convert (ST);
                  when ft_nbyte8    => dossier.a05.all := convert (ST);
                  when ft_byte1     => dossier.a06.all := convert (ST);
                  when ft_byte2     => dossier.a07.all := convert (ST);
                  when ft_byte3     => dossier.a08.all := convert (ST);
                  when ft_byte4     => dossier.a09.all := convert (ST);
                  when ft_byte8     => dossier.a10.all := convert (ST);
                  when ft_real9     => dossier.a11.all := convert (ST);
                  when ft_real18    => dossier.a12.all := convert (ST);
                  when ft_textual   => dossier.a13.all := CT.SUS (ST);
                  when ft_widetext  => dossier.a14.all := convert (ST);
                  when ft_supertext => dossier.a15.all := convert (ST);
                  when ft_enumtype  => dossier.a18.all := ARC.convert (ST);
                  when ft_timestamp =>
                     begin
                        dossier.a16.all := ARC.convert (ST);
                     exception
                        when CAL.Time_Error =>
                           dossier.a16.all := CAL.Time_Of
                             (Year  => CAL.Year_Number'First,
                              Month => CAL.Month_Number'First,
                              Day   => CAL.Day_Number'First);
                     end;
                  when ft_chain =>
                     declare
                        FL    : Natural := dossier.a17.all'Length;
                        DVLEN : Natural := ST'Length;
                     begin
                        if DVLEN > FL then
                           raise BINDING_SIZE_MISMATCH with "native size : " &
                             DVLEN'Img & " greater than binding size : " &
                             FL'Img;
                        end if;
                        dossier.a17.all := ARC.convert (ST, FL);
                     end;
                  when ft_settype =>
                     declare
                        FL    : Natural := dossier.a19.all'Length;
                        items : constant Natural := CT.num_set_items (ST);
                     begin
                        if items > FL then
                           raise BINDING_SIZE_MISMATCH with
                             "native size : " & items'Img &
                             " greater than binding size : " & FL'Img;
                        end if;
                        dossier.a19.all := ARC.convert (ST, FL);
                     end;
               end case;
            end;
            <<continue>>
         end loop;
      end;

      return True;
   end fetch_bound;


   ----------------------
   --  fetch_next_set  --
   ----------------------
   overriding
   procedure fetch_next_set (Stmt         : out PostgreSQL_statement;
                             data_present : out Boolean;
                             data_fetched : out Boolean) is
   begin
      --  TO BE IMPLEMENTED
      null;
   end fetch_next_set;


   ------------------
   --  initialize  --
   ------------------
   overriding
   procedure initialize (Object : in out PostgreSQL_statement)
   is
      use type CON.PostgreSQL_Connection_Access;
      conn   : CON.PostgreSQL_Connection_Access renames Object.pgsql_conn;
      logcat : Log_Category;
      stmt_name   : String := Object.show_statement_name;
      hold_result : aliased BND.PGresult_Access;
   begin

      if conn = null then
         return;
      end if;

      logger_access     := Object.log_handler;
      Object.dialect    := driver_postgresql;
      Object.connection := ACB.Base_Connection_Access (conn);

      case Object.type_of_statement is
         when direct_statement =>
            Object.sql_final := new String'(CT.trim_sql
                                            (Object.initial_sql.all));
            logcat := statement_execution;
         when prepared_statement =>
            Object.sql_final :=
              new String'(reformat_markers (Object.transform_sql
                          (Object.initial_sql.all)));
            logcat := statement_preparation;
      end case;

      if Object.type_of_statement = prepared_statement then
         if conn.prepare_statement (stmt => hold_result,
                                    name => stmt_name,
                                    sql  => Object.sql_final.all)
         then
            Object.stmt_allocated := True;
            Object.log_nominal (category => logcat,
                                message  => Object.sql_final.all);
         else
            Object.log_problem
              (category => statement_preparation,
               message  => "Failed to prepare SQL query: '" &
                            Object.sql_final.all & "'",
               pull_codes => True);
            return;
         end if;

         --  Check that we have as many markers as expected
         declare
            params : Natural := conn.markers_found (Object.stmt_handle);
            errmsg : String := "marker mismatch," &
              Object.realmccoy.Length'Img & " expected but" &
              params'Img & " found by PostgreSQL";
         begin
            if params /= Natural (Object.realmccoy.Length) then
               Object.log_problem
                 (category => statement_preparation,
                  message  => errmsg);
               return;
            end if;
         end;

         --  Get column metadata
         if conn.prepare_metadata (meta => Object.stmt_handle,
                                   name => stmt_name)
         then
            Object.scan_column_information;
            conn.discard_pgresult (Object.stmt_handle);
            Object.stmt_handle := hold_result;
            Object.size_of_rowset := 0;
            Object.result_arrow   := 0;
         else
            Object.log_problem
              (category => statement_preparation,
               message  => "Failed to acquire prep statement metadata (" &
                            stmt_name & ")",
               pull_codes => True);
         end if;

      else
         if conn.direct_stmt_exec (stmt => Object.stmt_handle,
                                   sql => Object.sql_final.all)
         then
            Object.log_nominal (category => logcat,
                                message  => Object.sql_final.all);
            Object.successful_execution := True;
            Object.result_arrow := 0;
            Object.size_of_rowset := conn.rows_in_result (Object.stmt_handle);

            Object.scan_column_information;
         else
            Object.log_problem
              (category => statement_execution,
               message  => "Failed to execute a direct SQL query");
            return;
         end if;
      end if;

   exception
      when HELL : others =>
         Object.log_problem
           (category => statement_preparation,
            message  => CON.EX.Exception_Message (HELL));
   end initialize;


   -------------------------------
   --  scan_column_information  --
   -------------------------------
   procedure scan_column_information (Stmt : out PostgreSQL_statement)
   is
      function fn (raw : String) return CT.Text;
      function sn (raw : String) return String;
      function fn (raw : String) return CT.Text is
      begin
         return CT.SUS (sn (raw));
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

      conn : CON.PostgreSQL_Connection_Access renames Stmt.pgsql_conn;
   begin
      Stmt.num_columns := conn.fields_count (Stmt.stmt_handle);
      for index in Natural range 0 .. Stmt.num_columns - 1 loop
         declare
            info  : column_info;
            brec  : bindrec;
            name  : String := conn.field_name (Stmt.stmt_handle, index);
            table : String := conn.field_table (Stmt.stmt_handle, index);
         begin
            brec.v00           := False;   --  placeholder
            info.field_name    := fn (name);
            info.table         := fn (table);
            info.field_type    := conn.field_type (Stmt.stmt_handle, index);
            info.binary_format := conn.field_data_is_binary (Stmt.stmt_handle,
                                                             index);

            --  IMPLEMENT
            info.null_possible := False;


            Stmt.column_info.Append (New_Item => info);
            --  The following pre-populates for bind support
            Stmt.crate.Append (New_Item => brec);
            Stmt.headings_map.Insert (Key      => sn (name),
                                      New_Item => Stmt.crate.Last_Index);
         end;
      end loop;
   end scan_column_information;


   -------------------
   --  log_problem  --
   -------------------
   procedure log_problem
     (statement  : PostgreSQL_statement;
      category   : Log_Category;
      message    : String;
      pull_codes : Boolean := False;
      break      : Boolean := False)
   is
      error_msg  : CT.Text      := CT.blank;
      error_code : Driver_Codes := 0;
      sqlstate   : SQL_State    := stateless;
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


   --------------
   --  Adjust  --
   --------------
   overriding
   procedure Adjust (Object : in out PostgreSQL_statement) is
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
   procedure finalize (Object : in out PostgreSQL_statement)
   is
      use type BND.PGresult_Access;
      conn : CON.PostgreSQL_Connection_Access renames Object.pgsql_conn;
   begin
      if Object.assign_counter /= 2 then
         return;
      end if;

      if Object.stmt_handle /= null then
         conn.discard_pgresult (Object.stmt_handle);
      end if;

      if Object.stmt_allocated then
         if not conn.destroy_statement (Object.show_statement_name) then
            Object.log_problem
              (category   => statement_preparation,
               message    => "Deallocating statement resources",
               pull_codes => True);
         end if;
      end if;

      if Object.sql_final /= null then
         free_sql (Object.sql_final);
      end if;
   end finalize;


   ------------------------
   --  assembly_datarow  --
   ------------------------
   function assemble_datarow (Stmt : PostgreSQL_statement;
                              row_number : Trax_ID) return ARS.Datarow
   is
      function null_value (column : Natural) return Boolean;
      function string_equivalent (column : Natural; binary : Boolean)
                                  return String;
      result : ARS.Datarow;
      conn   : CON.PostgreSQL_Connection_Access renames Stmt.pgsql_conn;
      maxlen : constant Natural := Natural (Stmt.column_info.Length);

      function string_equivalent (column : Natural; binary : Boolean)
                                  return String
      is
         --  PostgreSQL result set is zero-indexed
         row_num : constant Natural := Natural (row_number) - 1;
         col_num : constant Natural := column - 1;
      begin
         if binary then
            return conn.field_binary (Stmt.stmt_handle, row_num, col_num,
                                      Stmt.con_max_blob);
         else
            return conn.field_string (Stmt.stmt_handle, row_num, col_num);
         end if;
      end string_equivalent;

      function null_value (column : Natural) return Boolean
      is
         --  PostgreSQL result set is zero-indexed
         row_num : constant Natural := Natural (row_number) - 1;
         col_num : constant Natural := column - 1;
      begin
         return conn.field_is_null (Stmt.stmt_handle, row_num, col_num);
      end null_value;

   begin
      for F in 1 .. maxlen loop
         declare
            colinfo : column_info renames Stmt.column_info.Element (F);
            field    : ARF.Std_Field;
            dvariant : ARF.Variant;
            last_one : constant Boolean := (F = maxlen);
            isnull   : constant Boolean := null_value (F);
            heading  : constant String  := CT.USS (colinfo.field_name);
            ST       : constant String  :=
                       string_equivalent (F, colinfo.binary_format);
         begin
            case colinfo.field_type is
               when ft_nbyte0 =>
                  dvariant := (datatype => ft_nbyte0, v00 => ST = "1");
               when ft_nbyte1 =>
                  dvariant := (datatype => ft_nbyte1, v01 => convert (ST));
               when ft_nbyte2 =>
                  dvariant := (datatype => ft_nbyte2, v02 => convert (ST));
               when ft_nbyte3 =>
                  dvariant := (datatype => ft_nbyte3, v03 => convert (ST));
               when ft_nbyte4 =>
                  dvariant := (datatype => ft_nbyte4, v04 => convert (ST));
               when ft_nbyte8 =>
                  dvariant := (datatype => ft_nbyte8, v05 => convert (ST));
               when ft_byte1  =>
                  dvariant := (datatype => ft_byte1, v06 => convert (ST));
               when ft_byte2  =>
                  dvariant := (datatype => ft_byte2, v07 => convert (ST));
               when ft_byte3  =>
                  dvariant := (datatype => ft_byte3, v08 => convert (ST));
               when ft_byte4  =>
                  dvariant := (datatype => ft_byte4, v09 => convert (ST));
               when ft_byte8  =>
                  dvariant := (datatype => ft_byte8, v10 => convert (ST));
               when ft_real9  =>
                  dvariant := (datatype => ft_real9, v11 => convert (ST));
               when ft_real18 =>
                  dvariant := (datatype => ft_real18, v12 => convert (ST));
               when ft_textual =>
                  dvariant := (datatype => ft_textual, v13 => CT.SUS (ST));
               when ft_widetext =>
                  dvariant := (datatype => ft_widetext, v14 => convert (ST));
               when ft_supertext =>
                  dvariant := (datatype => ft_supertext, v15 => convert (ST));
               when ft_timestamp =>
                  begin
                     dvariant := (datatype => ft_timestamp,
                                  v16 => ARC.convert (ST));
                  exception
                     when CAL.Time_Error =>
                        dvariant := (datatype => ft_textual,
                                     v13 => CT.SUS (ST));
                  end;
               when ft_enumtype =>
                  dvariant := (datatype => ft_enumtype,
                               V18 => ARC.convert (CT.SUS (ST)));
               when others =>
                  null;

            end case;
            case colinfo.field_type is
               when ft_chain =>
                  field := ARF.spawn_field (binob => ARC.convert (ST));
               when ft_settype =>
                  field := ARF.spawn_field (enumset => ST);
               when others =>
                  field := ARF.spawn_field (data => dvariant,
                                            null_data => isnull);
            end case;

            result.push (heading    => heading,
                         field      => field,
                         last_field => last_one);
         end;
      end loop;
      return result;
   end assemble_datarow;


   ---------------------------
   --  show_statement_name  --
   ---------------------------
   function show_statement_name (Stmt : PostgreSQL_statement) return String is
   begin
      return "AdaBase_" & CT.trim (Stmt.identifier'Img);
   end show_statement_name;


end AdaBase.Statement.Base.PostgreSQL;
