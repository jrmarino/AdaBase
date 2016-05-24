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
   function column_count (Stmt : PostgreSQL_statement) return Natural is
   begin
      return Stmt.num_columns;
   end column_count;


   ----------------------
   --  last_insert_id  --
   ----------------------
   overriding
   function last_insert_id (Stmt : PostgreSQL_statement) return Trax_ID
   is
      conn : CON.PostgreSQL_Connection_Access renames Stmt.pgsql_conn;
   begin
      if Stmt.insert_return then
         return Stmt.last_inserted;
      else
         return conn.select_last_val;
      end if;
   end last_insert_id;


   ----------------------
   --  last_sql_state  --
   ----------------------
   overriding
   function last_sql_state (Stmt : PostgreSQL_statement) return SQL_State
   is
      conn : CON.PostgreSQL_Connection_Access renames Stmt.pgsql_conn;
   begin
      return conn.SqlState (Stmt.result_handle);
   end last_sql_state;


   ------------------------
   --  last_driver_code  --
   ------------------------
   overriding
   function last_driver_code (Stmt : PostgreSQL_statement) return Driver_Codes
   is
      conn : CON.PostgreSQL_Connection_Access renames Stmt.pgsql_conn;
   begin
      return conn.driverCode (Stmt.result_handle);
   end last_driver_code;


   ---------------------------
   --  last_driver_message  --
   ---------------------------
   overriding
   function last_driver_message (Stmt : PostgreSQL_statement) return String
   is
      conn : CON.PostgreSQL_Connection_Access renames Stmt.pgsql_conn;
   begin
      return conn.driverMessage (Stmt.result_handle);
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
      markers : constant Natural := Natural (Stmt.realmccoy.Length);
      status_successful : Boolean := True;
   begin
      if Stmt.type_of_statement = direct_statement then
         raise INVALID_FOR_DIRECT_QUERY
           with "The execute command is for prepared statements only";
      end if;

      Stmt.result_arrow := 0;
      Stmt.last_inserted := 0;
      Stmt.size_of_rowset := 0;
      Stmt.rows_leftover := False;
      Stmt.successful_execution := False;
      conn.discard_pgresult (Stmt.result_handle);

      if markers > 0 then
         --  Check to make sure all prepared markers are bound
         for sx in Natural range 1 .. markers loop
            if not Stmt.realmccoy.Element (sx).bound then
               raise STMT_PREPARATION
                 with "Prep Stmt column" & sx'Img & " unbound";
            end if;
         end loop;

         --  Now bind the actual values to the markers
         declare
            canvas : CON.parameter_block (1 .. markers);
            msg : String := "Exec with" & markers'Img & " bound parameters";
         begin
            for x in canvas'Range loop
               canvas (x).payload := Stmt.bind_text_value (x);
               canvas (x).is_null := Stmt.realmccoy.Element (x).null_data;
               canvas (x).binary  := Stmt.realmccoy.Element (x).output_type =
                                     ft_chain;
            end loop;
            Stmt.log_nominal (statement_execution, msg);

            Stmt.result_handle :=  conn.execute_prepared_stmt
              (name => Stmt.show_statement_name,
               data => canvas);
         end;

      else
         --  No binding required, just execute the prepared statement
         Stmt.log_nominal (category => statement_execution,
                           message => "Exec without bound parameters");

         Stmt.result_handle := conn.execute_prepared_stmt
           (name => Stmt.show_statement_name);
      end if;

      case conn.examine_result (Stmt.result_handle) is
         when CON.executed =>
            Stmt.successful_execution := True;
         when CON.returned_data =>
            Stmt.successful_execution := True;
            Stmt.insert_return := Stmt.insert_prepsql;
         when CON.failed =>
            Stmt.successful_execution := False;
      end case;

      if Stmt.successful_execution then
         if not Stmt.insert_return then
            Stmt.size_of_rowset := conn.rows_in_result (Stmt.result_handle);
         end if;

         if Stmt.insert_return then
            Stmt.last_inserted := conn.returned_id (Stmt.result_handle);
         end if;
      end if;

      return Stmt.successful_execution;
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
      return conn.rows_impacted (Stmt.result_handle);
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
      maxlen : constant Natural := Natural (Stmt.column_info.Length);
   begin
      if index > maxlen then
         raise INVALID_COLUMN_INDEX with "Max index is" & maxlen'Img &
           " but" & index'Img & " attempted";
      end if;
      return Stmt.column_info.Element (Index => index).field_type;
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
            return conn.field_binary (Stmt.result_handle, row_num, col_num,
                                      Stmt.con_max_blob);
         else
            return conn.field_string (Stmt.result_handle, row_num, col_num);
         end if;
      end string_equivalent;

      function null_value (column : Natural) return Boolean
      is
         --  PostgreSQL result set is zero-indexed
         row_num : constant Natural := Natural (Stmt.result_arrow) - 1;
         col_num : constant Natural := column - 1;
      begin
         return conn.field_is_null (Stmt.result_handle, row_num, col_num);
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
      params : Natural;
      stmt_name   : String := Object.show_statement_name;
      hold_result : aliased BND.PGresult_Access;
   begin

      if conn = null then
         return;
      end if;

      logger_access         := Object.log_handler;
      Object.dialect        := driver_postgresql;
      Object.connection     := ACB.Base_Connection_Access (conn);
      Object.insert_prepsql := False;

      --------------------------------
      --  Set SQL and log category  --
      --------------------------------
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

      --------------------------------------------------------
      --  Detect INSERT commands (for INSERT .. RETURNING)  --
      --------------------------------------------------------
      declare
         sql : String := Object.initial_sql.all;
      begin
         if sql'Length > 12 and then
           ACH.To_Upper (sql (sql'First .. sql'First + 6)) = "INSERT "
         then
            Object.insert_prepsql := True;
         end if;
      end;

      if Object.type_of_statement = prepared_statement then
         -----------------------------------
         --  Prepared Statement handling  --
         -----------------------------------
         if conn.prepare_statement (stmt => Object.prepared_stmt,
                                    name => stmt_name,
                                    sql  => Object.sql_final.all)
         then
            Object.stmt_allocated := True;
            Object.log_nominal (category => logcat,
                                message  => Object.sql_final.all);
         else
            Object.log_problem (statement_preparation,
                                conn.driverMessage (Object.prepared_stmt));
            Object.log_problem
              (category => statement_preparation,
               message  => "Failed to prepare SQL query: '" &
                            Object.sql_final.all & "'",
               break    => True);
            return;
         end if;

         ---------------------------------------
         --  Get column metadata (prep stmt)  --
         ---------------------------------------
         if conn.prepare_metadata (meta => hold_result,
                                   name => stmt_name)
         then
            Object.scan_column_information (hold_result);
            params := conn.markers_found (hold_result);
            conn.discard_pgresult (hold_result);
         else
            Object.log_problem (statement_preparation,
                                conn.driverMessage (hold_result));
            Object.log_problem
              (category => statement_preparation,
               message  => "Failed to acquire prep statement metadata (" &
                            stmt_name & ")",
               break    => True);
            return;
         end if;

         ------------------------------------------------------
         --  Check that we have as many markers as expected  --
         ------------------------------------------------------
         declare
            errmsg : String := "marker mismatch," &
              Object.realmccoy.Length'Img & " expected but" &
              params'Img & " found by PostgreSQL";
         begin
            if params /= Natural (Object.realmccoy.Length) then
               Object.log_problem
                 (category => statement_preparation,
                  message  => errmsg,
                  break    => True);
               return;
            end if;
         end;

      else
         ---------------------------------
         --  Direct statement handling  --
         ---------------------------------
         if conn.direct_stmt_exec (stmt => Object.result_handle,
                                   sql => Object.sql_final.all)
         then
            Object.log_nominal (category => logcat,
                                message  => Object.sql_final.all);

            case conn.examine_result (Object.result_handle) is
               when CON.executed =>
                  Object.successful_execution := True;
               when CON.returned_data =>
                  Object.successful_execution := True;
                  Object.insert_return := Object.insert_prepsql;
               when CON.failed =>
                  Object.successful_execution := False;
            end case;

            if not Object.insert_return then
               Object.size_of_rowset :=
                 conn.rows_in_result (Object.result_handle);
            end if;

            if Object.insert_return then
               Object.last_inserted := conn.returned_id (Object.result_handle);
            end if;

            Object.scan_column_information (Object.result_handle);
         else
            Object.log_problem
              (category => statement_execution,
               message  => "Failed to execute a direct SQL query",
               break    => True);
            return;
         end if;
      end if;
   end initialize;


   -------------------------------
   --  scan_column_information  --
   -------------------------------
   procedure scan_column_information (Stmt : out PostgreSQL_statement;
                                      pgresult : BND.PGresult_Access)
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
      Stmt.num_columns := conn.fields_count (pgresult);
      for index in Natural range 0 .. Stmt.num_columns - 1 loop
         declare
            info  : column_info;
            brec  : bindrec;
            name  : String := conn.field_name (pgresult, index);
            table : String := conn.field_table (pgresult, index);
         begin
            brec.v00           := False;   --  placeholder
            info.field_name    := fn (name);
            info.table         := fn (table);
            info.field_type    := conn.field_type (pgresult, index);
            info.binary_format :=
              conn.field_data_is_binary (pgresult, index);
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
      conn : CON.PostgreSQL_Connection_Access renames Object.pgsql_conn;
   begin
      if Object.assign_counter /= 2 then
         return;
      end if;

      conn.discard_pgresult (Object.result_handle);

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
            return conn.field_binary (Stmt.result_handle, row_num, col_num,
                                      Stmt.con_max_blob);
         else
            return conn.field_string (Stmt.result_handle, row_num, col_num);
         end if;
      end string_equivalent;

      function null_value (column : Natural) return Boolean
      is
         --  PostgreSQL result set is zero-indexed
         row_num : constant Natural := Natural (row_number) - 1;
         col_num : constant Natural := column - 1;
      begin
         return conn.field_is_null (Stmt.result_handle, row_num, col_num);
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
            if isnull then
               field := ARF.spawn_null_field (colinfo.field_type);
            else
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
            end if;

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


   -----------------------
   --  bind_text_value  --
   -----------------------
   function bind_text_value (Stmt : PostgreSQL_statement; marker : Positive)
                             return AR.Textual
   is
      zone    : bindrec renames Stmt.realmccoy.Element (marker);
      vartype : constant field_types := zone.output_type;

      use type AR.NByte0_Access;
      use type AR.NByte1_Access;
      use type AR.NByte2_Access;
      use type AR.NByte3_Access;
      use type AR.NByte4_Access;
      use type AR.NByte8_Access;
      use type AR.Byte1_Access;
      use type AR.Byte2_Access;
      use type AR.Byte3_Access;
      use type AR.Byte4_Access;
      use type AR.Byte8_Access;
      use type AR.Real9_Access;
      use type AR.Real18_Access;
      use type AR.Str1_Access;
      use type AR.Str2_Access;
      use type AR.Str4_Access;
      use type AR.Time_Access;
      use type AR.Enum_Access;
      use type AR.Chain_Access;
      use type AR.Settype_Access;

      hold : AR.Textual;
   begin
      case vartype is
         when ft_nbyte0 =>
            if zone.a00 = null then
               hold := ARC.convert (zone.v00);
            else
               hold := ARC.convert (zone.a00.all);
            end if;
         when ft_nbyte1 =>
            if zone.a01 = null then
               hold := ARC.convert (zone.v01);
            else
               hold := ARC.convert (zone.a01.all);
            end if;
         when ft_nbyte2 =>
            if zone.a02 = null then
               hold := ARC.convert (zone.v02);
            else
               hold := ARC.convert (zone.a02.all);
            end if;
         when ft_nbyte3 =>
            if zone.a03 = null then
               hold := ARC.convert (zone.v03);
            else
               hold := ARC.convert (zone.a03.all);
            end if;
         when ft_nbyte4 =>
            if zone.a04 = null then
               hold := ARC.convert (zone.v04);
            else
               hold := ARC.convert (zone.a04.all);
            end if;
         when ft_nbyte8 =>
            if zone.a05 = null then
               hold := ARC.convert (zone.v05);
            else
               hold := ARC.convert (zone.a05.all);
            end if;
         when ft_byte1 =>
            if zone.a06 = null then
               hold := ARC.convert (zone.v06);
            else
               hold := ARC.convert (zone.a06.all);
            end if;
         when ft_byte2 =>
            if zone.a07 = null then
               hold := ARC.convert (zone.v07);
            else
               hold := ARC.convert (zone.a07.all);
            end if;
         when ft_byte3 =>
            if zone.a08 = null then
               hold := ARC.convert (zone.v08);
            else
               hold := ARC.convert (zone.a08.all);
            end if;
         when ft_byte4 =>
            if zone.a09 = null then
               hold := ARC.convert (zone.v09);
            else
               hold := ARC.convert (zone.a09.all);
            end if;
         when ft_byte8 =>
            if zone.a10 = null then
               hold := ARC.convert (zone.v10);
            else
               hold := ARC.convert (zone.a10.all);
            end if;
         when ft_real9 =>
            if zone.a11 = null then
               hold := ARC.convert (zone.v11);
            else
               hold := ARC.convert (zone.a11.all);
            end if;
         when ft_real18 =>
            if zone.a12 = null then
               hold := ARC.convert (zone.v12);
            else
               hold := ARC.convert (zone.a12.all);
            end if;
         when ft_textual =>
            if zone.a13 = null then
               hold := zone.v13;
            else
               hold := zone.a13.all;
            end if;
         when ft_widetext =>
            if zone.a14 = null then
               hold := ARC.convert (zone.v14);
            else
               hold := ARC.convert (zone.a14.all);
            end if;
         when ft_supertext =>
            if zone.a15 = null then
               hold := ARC.convert (zone.v15);
            else
               hold := ARC.convert (zone.a15.all);
            end if;
         when ft_timestamp =>
            if zone.a16 = null then
               hold := ARC.convert (zone.v16);
            else
               hold := ARC.convert (zone.a16.all);
            end if;
         when ft_chain =>
            if zone.a17 = null then
               hold := zone.v17;
            else
               hold := ARC.convert (zone.a17.all);
            end if;
         when ft_enumtype =>
            if zone.a18 = null then
               hold := ARC.convert (zone.v18);
            else
               hold := ARC.convert (zone.a18.all);
            end if;
         when ft_settype =>
            if zone.a19 = null then
               hold := zone.v19;
            else
               hold := ARC.convert (zone.a19.all);
            end if;
      end case;
      return hold;
   end bind_text_value;

end AdaBase.Statement.Base.PostgreSQL;
