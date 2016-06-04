--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body AdaBase.Statement.Base.SQLite is

   -------------------
   --  log_problem  --
   -------------------
   procedure log_problem
     (statement  : SQLite_statement;
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
      conn   : ACS.SQLite_Connection_Access renames Object.sqlite_conn;
      logcat : Log_Category;
   begin

      if conn = null then
         return;
      end if;

      logger_access     := Object.log_handler;
      Object.dialect    := driver_sqlite;
      Object.connection := ACB.Base_Connection_Access (conn);

      case Object.type_of_statement is
         when direct_statement =>
            Object.sql_final := new String'(CT.trim_sql
                                            (Object.initial_sql.all));
            logcat := statement_execution;
         when prepared_statement =>
            Object.sql_final := new String'(Object.transform_sql
                                            (Object.initial_sql.all));
            logcat := statement_preparation;
      end case;

      if conn.prepare_statement (stmt => Object.stmt_handle,
                                 sql  => Object.sql_final.all)
      then
         Object.successful_execution := True;
         Object.log_nominal (category => logcat,
                             message  => Object.sql_final.all);
      else
         Object.log_problem
              (category => statement_preparation,
               message  => "Failed to parse SQL query: '" &
                            Object.sql_final.all & "'",
               pull_codes => True);
         return;
      end if;

      if Object.type_of_statement = prepared_statement then
         --  Check that we have as many markers as expected
         declare
            params : Natural := conn.prep_markers_found (Object.stmt_handle);
            errmsg : String := "marker mismatch," &
              Object.realmccoy.Length'Img & " expected but" &
              params'Img & " found by SQLite";
         begin
            if params /= Natural (Object.realmccoy.Length) then
               Object.log_problem
                 (category => statement_preparation,
                  message  => errmsg);
               return;
            end if;
         end;
      else
         if not Object.private_execute then
            Object.log_problem
              (category => statement_preparation,
               message  => "Failed to execute a direct SQL query");
            return;
         end if;
      end if;

      Object.scan_column_information;

   exception
      when HELL : others =>
         Object.log_problem
           (category => statement_preparation,
            message  => ACS.EX.Exception_Message (HELL));
   end initialize;


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
            tname : String := conn.field_true_name (Stmt.stmt_handle, index);
            table : String := conn.field_table (Stmt.stmt_handle, index);
            dbase : String := conn.field_database (Stmt.stmt_handle, index);
         begin
            brec.v00          := False;   --  placeholder
            info.field_name   := fn (name);
            info.table        := fn (table);

            conn.get_field_meta_data (stmt      => Stmt.stmt_handle,
                                      database  => dbase,
                                      table     => table,
                                      column    => tname,
                                      data_type => info.sqlite_type,
                                      nullable  => info.null_possible);

            case info.sqlite_type is
               when BND.SQLITE_INTEGER => info.field_type := ft_byte8;
               when BND.SQLITE_TEXT    => info.field_type := ft_utf8;
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
   function last_insert_id (Stmt : SQLite_statement) return Trax_ID
   is
      conn : ACS.SQLite_Connection_Access renames Stmt.sqlite_conn;
   begin
      return conn.lastInsertID;
   end last_insert_id;


   ----------------------
   --  last_sql_state  --
   ----------------------
   overriding
   function last_sql_state (Stmt : SQLite_statement) return SQL_State
   is
      conn : ACS.SQLite_Connection_Access renames Stmt.sqlite_conn;
   begin
      return conn.SqlState;
   end last_sql_state;


   ------------------------
   --  last_driver_code  --
   ------------------------
   overriding
   function last_driver_code (Stmt : SQLite_statement) return Driver_Codes
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
   function rows_returned (Stmt : SQLite_statement) return Affected_Rows is
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
      Stmt.rows_leftover := (Stmt.step_result = data_pulled);
      conn.reset_prep_stmt (stmt => Stmt.stmt_handle);
      Stmt.step_result := unset;
   end discard_rest;


   -----------------------
   --  private_execute  --
   -----------------------
   function private_execute (Stmt : out SQLite_statement) return Boolean
   is
      conn : ACS.SQLite_Connection_Access renames Stmt.sqlite_conn;
   begin
      if conn.prep_fetch_next (Stmt.stmt_handle) then
         Stmt.step_result := data_pulled;
      else
         Stmt.step_result := progam_complete;
         Stmt.impacted := conn.rows_affected_by_execution;
      end if;
      return True;
   exception
      when ACS.STMT_FETCH_FAIL =>
         Stmt.step_result := error_seen;
         return False;
   end private_execute;


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

      conn.reset_prep_stmt (Stmt.stmt_handle);
      Stmt.reclaim_canvas;
      Stmt.step_result := unset;
      Stmt.rows_leftover := False;

      if num_markers > 0 then
         --  Check to make sure all prepared markers are bound
         for sx in Natural range 1 .. num_markers loop
            if not Stmt.realmccoy.Element (sx).bound then
               raise STMT_PREPARATION
                 with "Prep Stmt column" & sx'Img & " unbound";
            end if;
         end loop;

         --  Now bind the actual values to the markers
         begin
            for sx in Natural range 1 .. num_markers loop
               Stmt.bind_canvas.Append (Stmt.construct_bind_slot (sx));
            end loop;
            Stmt.log_nominal (category => statement_execution,
                              message => "Exec with" & num_markers'Img &
                                " bound parameters");
         exception
            when CBS : others =>
               Stmt.log_problem (category => statement_execution,
                                 message  => ACS.EX.Exception_Message (CBS));
               return False;
         end;

      else
         --  No binding required, just execute the prepared statement
         Stmt.log_nominal (category => statement_execution,
                           message => "Exec without bound parameters");
      end if;

      begin
         if conn.prep_fetch_next (Stmt.stmt_handle) then
            Stmt.step_result := data_pulled;
         else
            Stmt.step_result := progam_complete;
            Stmt.impacted := conn.rows_affected_by_execution;
         end if;
         Stmt.successful_execution := True;
      exception
         when ACS.STMT_FETCH_FAIL =>
            Stmt.step_result := error_seen;
            status_successful := False;
      end;

      return status_successful;

   end execute;


   ------------------
   --  execute #2  --
   ------------------
   overriding
   function execute (Stmt : out SQLite_statement; parameters : String;
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

   ------------------
   --  fetch_next  --
   ------------------
   overriding
   function fetch_next (Stmt : out SQLite_statement) return ARS.Datarow
   is
      conn   : ACS.SQLite_Connection_Access renames Stmt.sqlite_conn;
   begin
      if Stmt.step_result /= data_pulled then
         return ARS.Empty_Datarow;
      end if;
      declare
         maxlen : constant Natural := Natural (Stmt.column_info.Length);
         result : ARS.Datarow;
      begin

         for F in 1 .. maxlen loop
            declare
               colinfo  : column_info renames Stmt.column_info.Element (F);
               field    : ARF.Std_Field;
               dvariant : ARF.Variant;
               scol     : constant Natural := F - 1;
               last_one : constant Boolean := (F = maxlen);
               heading  : constant String := CT.USS (colinfo.field_name);
               isnull   : constant Boolean :=
                          conn.field_is_null (Stmt.stmt_handle, scol);
            begin
               if isnull then
                  field := ARF.spawn_null_field (colinfo.field_type);
               else
                  case colinfo.field_type is
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
                  when ft_utf8 =>
                     declare
                        datatext : AR.Textual :=
                          conn.retrieve_text (Stmt.stmt_handle, scol);
                     begin
                        if seems_like_bit_string (datatext) then
                           dvariant := (datatype => ft_bits,
                                        v20 => datatext);
                        else
                           dvariant := (datatype => ft_utf8,
                                        v21 => datatext);
                        end if;
                     end;
                  when ft_chain   => null;
                  when others => raise INVALID_FOR_RESULT_SET
                       with "Impossible field type (internal bug??)";
                  end case;
                  case colinfo.field_type is
                  when ft_chain =>
                     field := ARF.spawn_field
                       (binob => ARC.convert
                          (conn.retrieve_blob
                               (stmt  => Stmt.stmt_handle,
                                index => scol,
                                maxsz => Stmt.con_max_blob)));
                  when ft_nbyte0 | ft_byte8 | ft_real18 | ft_utf8 =>
                     field := ARF.spawn_field (data => dvariant,
                                               null_data => isnull);
                  when others => null;
                  end case;
               end if;
               result.push (heading    => heading,
                            field      => field,
                            last_field => last_one);
            end;
         end loop;
         begin
            if conn.prep_fetch_next (Stmt.stmt_handle) then
               Stmt.step_result := data_pulled;
            else
               Stmt.step_result := progam_complete;
            end if;
         exception
            when ACS.STMT_FETCH_FAIL =>
               Stmt.step_result := error_seen;
         end;
         return result;
      end;
   end fetch_next;


   ------------------
   --  fetch_bound --
   ------------------
   overriding
   function fetch_bound (Stmt : out SQLite_statement) return Boolean
   is
      conn   : ACS.SQLite_Connection_Access renames Stmt.sqlite_conn;
   begin
      if Stmt.step_result /= data_pulled then
         return False;
      end if;

      declare
         maxlen : constant Natural := Stmt.num_columns;
      begin
         for F in 1 .. maxlen loop
            declare
               dossier  : bindrec renames Stmt.crate.Element (F);
               colinfo  : column_info renames Stmt.column_info.Element (F);
               Tout     : constant field_types := dossier.output_type;
               Tnative  : constant field_types := colinfo.field_type;
               dvariant : ARF.Variant;
               scol     : constant Natural := F - 1;
            begin
               if not dossier.bound then
                  goto continue;
               end if;

               case Tnative is
                  when ft_byte8   =>
                     dvariant :=
                       (datatype => ft_byte8,
                        v10 => conn.retrieve_integer (Stmt.stmt_handle, scol));
                  when ft_real18  =>
                     dvariant :=
                       (datatype => ft_real18,
                        v12 => conn.retrieve_double (Stmt.stmt_handle, scol));
                  when ft_utf8 =>
                     dvariant :=
                       (datatype => ft_utf8,
                        v21 => conn.retrieve_text (Stmt.stmt_handle, scol));
                  when ft_chain =>
                     declare
                        bin : String :=
                          conn.retrieve_blob
                            (stmt  => Stmt.stmt_handle,
                             index => scol,
                             maxsz => Stmt.con_max_blob);
                     begin
                        dvariant := (datatype => ft_chain,
                                     v17 => CT.SUS (bin));
                     end;
                  when others => raise INVALID_FOR_RESULT_SET
                       with "Impossible field type (internal bug??)";
               end case;

               if Tnative = ft_byte8 and then
                 (Tout = ft_nbyte0 or else
                  Tout = ft_nbyte1 or else
                  Tout = ft_nbyte2 or else
                  Tout = ft_nbyte3 or else
                  Tout = ft_nbyte4 or else
                  Tout = ft_nbyte8 or else
                  Tout = ft_byte1 or else
                  Tout = ft_byte2 or else
                  Tout = ft_byte3 or else
                  Tout = ft_byte4 or else
                  Tout = ft_byte8)
               then
                  case Tout is
                     when ft_nbyte0 =>
                        dossier.a00.all := ARC.convert (dvariant.v10);
                     when ft_nbyte1 =>
                        dossier.a01.all := ARC.convert (dvariant.v10);
                     when ft_nbyte2 =>
                        dossier.a02.all := ARC.convert (dvariant.v10);
                     when ft_nbyte3 =>
                        dossier.a03.all := ARC.convert (dvariant.v10);
                     when ft_nbyte4 =>
                        dossier.a04.all := ARC.convert (dvariant.v10);
                     when ft_nbyte8 =>
                        dossier.a05.all := ARC.convert (dvariant.v10);
                     when ft_byte1 =>
                        dossier.a06.all := ARC.convert (dvariant.v10);
                     when ft_byte2 =>
                        dossier.a07.all := ARC.convert (dvariant.v10);
                     when ft_byte3 =>
                        dossier.a08.all := ARC.convert (dvariant.v10);
                     when ft_byte4 =>
                        dossier.a09.all := ARC.convert (dvariant.v10);
                     when ft_byte8 =>
                        dossier.a10.all := dvariant.v10;
                     when others => null;
                  end case;
               elsif Tnative = ft_real18 and then
                 (Tout = ft_real9 or else
                  Tout = ft_real18)
               then
                  if Tout = ft_real18 then
                     dossier.a12.all := dvariant.v12;
                  else
                     dossier.a11.all := ARC.convert (dvariant.v12);
                  end if;
               elsif Tnative = ft_utf8 and then
                 (Tout = ft_textual or else
                  Tout = ft_widetext or else
                  Tout = ft_supertext or else
                  Tout = ft_timestamp or else
                  Tout = ft_enumtype or else
                  Tout = ft_settype or else
                  Tout = ft_utf8 or else
                  Tout = ft_bits)
               then
                  declare
                     STU : String := ARC.convert (dvariant.v21);
                     STA : String := ARC.cvu2str (CT.USS (dvariant.v21));
                  begin
                     case Tout is
                     when ft_textual   => dossier.a13.all := CT.SUS (STA);
                     when ft_widetext  => dossier.a14.all := convert (STA);
                     when ft_supertext => dossier.a15.all := convert (STA);
                     when ft_timestamp => dossier.a16.all := ARC.convert (STU);
                     when ft_enumtype  => dossier.a18.all := ARC.convert (STU);
                     when ft_utf8      => dossier.a21.all := STU;
                     when ft_settype =>
                        declare
                           FL    : Natural := dossier.a19.all'Length;
                           items : constant Natural := CT.num_set_items (STU);
                        begin
                           if items > FL then
                              raise BINDING_SIZE_MISMATCH with
                                "native size : " & items'Img &
                                " greater than binding size : " & FL'Img;
                           end if;
                           dossier.a19.all := ARC.convert (STU, FL);
                        end;
                     when ft_bits =>
                        declare
                           FL    : Natural := dossier.a20.all'Length;
                           DVLEN : Natural := STA'Length;
                        begin
                           if DVLEN > FL then
                              raise BINDING_SIZE_MISMATCH with "native size : " &
                                DVLEN'Img & " greater than binding size : " &
                                FL'Img;
                           end if;
                           dossier.a20.all := ARC.convert (STA, FL);
                        end;
                     when others => null;
                     end case;
                  end;
               elsif Tnative = ft_chain and then Tout = ft_chain then
                  declare
                     ST    : String := ARC.convert (dvariant.v17);
                     FL    : Natural := dossier.a17.all'Length;
                     DVLEN : Natural := ST'Length;
                  begin
                     if DVLEN > FL then
                        raise BINDING_SIZE_MISMATCH with "native size : " &
                          DVLEN'Img & " greater than binding size : " & FL'Img;
                     end if;
                     dossier.a17.all := ARC.convert (ST, FL);
                  end;
               else
                  raise BINDING_TYPE_MISMATCH with "native type " &
                    field_types'Image (Tnative) &
                    " is incompatible with binding type " &
                    field_types'Image (Tout);
               end if;
            end;
            <<continue>>
         end loop;
      end;

      begin
         if conn.prep_fetch_next (Stmt.stmt_handle) then
            Stmt.step_result := data_pulled;
         else
            Stmt.step_result := progam_complete;
         end if;
      exception
         when ACS.STMT_FETCH_FAIL =>
            Stmt.step_result := error_seen;
      end;

      return True;
   end fetch_bound;


   -----------------
   --  fetch_all  --
   -----------------
   overriding
   function fetch_all (Stmt : out SQLite_statement) return ARS.Datarow_Set
   is
      subtype rack_range is Positive range 1 .. 20000;
      dataset_size : Natural    := 0;
      arrow        : rack_range := rack_range'First;
      rack         : ARS.Datarow_Set (rack_range);
      nullset      : constant ARS.Datarow_Set (1 .. 0) :=
                     (others => ARS.Empty_Datarow);
   begin
      if Stmt.step_result /= data_pulled then
         return nullset;
      end if;
      --  With SQLite, we don't know many rows of data are fetched, ever.
      --  For practical purposes, let's limit a result set to 20k rows
      --  Rather than dynamically allocating rows and having to worry about
      --  copying them to a fixed array, let's just allocate a 20k set and
      --  return the part we need.  That should be more efficient considering
      --  trade-offs.
      --
      --  Note that this was originally intended to be a 100k row result set.
      --  but gcc 5.3 is core dumping with "illegal" instruction if the
      --  rack_range is >= 25000.  Maybe a problem with containers?  But it
      --  happened with a 100k array of access to datarows too!

      loop
         rack (arrow) := Stmt.fetch_next;
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
         return nullset;
      end if;

      return rack (1 .. dataset_size);
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
   procedure finalize (Object : in out SQLite_statement)
   is
      use type BND.sqlite3_stmt_Access;
   begin
      if Object.assign_counter /= 2 then
         return;
      end if;

      if Object.stmt_handle /= null then
         if not Object.sqlite_conn.prep_finalize (Object.stmt_handle) then
            Object.log_problem
              (category   => statement_preparation,
               message    => "Deallocating statement resources",
               pull_codes => True);
         end if;
      end if;

      if Object.sql_final /= null then
         free_sql (Object.sql_final);
      end if;
      Object.reclaim_canvas;
   end finalize;


   ---------------------------
   --  construct_bind_slot  --
   ---------------------------
   function construct_bind_slot (Stmt : SQLite_statement; marker : Positive)
                                 return sqlite_canvas
   is
      zone    : bindrec renames Stmt.realmccoy.Element (marker);
      conn    : ACS.SQLite_Connection_Access renames Stmt.sqlite_conn;

      vartype : constant field_types := zone.output_type;
      okay    : Boolean := True;
      product : sqlite_canvas;

      BT      : BND.ICS.chars_ptr         renames product.buffer_text;
      BB      : BND.ICS.char_array_access renames product.buffer_binary;

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
      use type AR.Bits_Access;
      use type AR.S_UTF8_Access;
      use type AR.Geometry_Access;
   begin
      if zone.null_data then
         if not conn.marker_is_null (Stmt.stmt_handle, marker) then
            raise STMT_EXECUTION
              with "failed to bind NULL marker" & marker'Img;
         end if;
      else
         case vartype is
            when ft_nbyte0 | ft_nbyte1 | ft_nbyte2 | ft_nbyte3 | ft_nbyte4 |
                 ft_nbyte8 | ft_byte1  | ft_byte2  | ft_byte3  | ft_byte4  |
                 ft_byte8 =>
               declare
                  hold : AR.Byte8;
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
                           hold := zone.v10;
                        else
                           hold := zone.a10.all;
                        end if;
                     when others => hold := 0;
                  end case;
                  okay := conn.marker_is_integer (Stmt.stmt_handle,
                                                  marker, hold);
               end;
            when ft_real9 | ft_real18 =>
               declare
                  hold : AR.Real18;
               begin
                  if vartype = ft_real18 then
                     if zone.a12 = null then
                        hold := zone.v12;
                     else
                        hold := zone.a12.all;
                     end if;
                  else
                     if zone.a11 = null then
                        hold := ARC.convert (zone.v11);
                     else
                        hold := ARC.convert (zone.a11.all);
                     end if;
                  end if;
                  okay := conn.marker_is_double (Stmt.stmt_handle,
                                                 marker, hold);
               end;
            when ft_textual =>
               if zone.a13 = null then
                  okay := conn.marker_is_text (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.v13), BT);
               else
                  okay := conn.marker_is_text (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.a13.all), BT);
               end if;
            when ft_widetext =>
               if zone.a14 = null then
                  okay := conn.marker_is_text (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.v14), BT);
               else
                  okay := conn.marker_is_text (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.a14.all), BT);
               end if;
           when ft_supertext =>
               if zone.a15 = null then
                  okay := conn.marker_is_text (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.v15), BT);
               else
                  okay := conn.marker_is_text (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.a15.all), BT);
               end if;
            when ft_timestamp =>
               if zone.a16 = null then
                  okay := conn.marker_is_text (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.v16), BT);
               else
                  okay := conn.marker_is_text (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.a16.all), BT);
               end if;
            when ft_enumtype =>
               if zone.a18 = null then
                  okay := conn.marker_is_text (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.v18), BT);
               else
                  okay := conn.marker_is_text (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.a18.all), BT);
               end if;
            when ft_settype =>
               if zone.a19 = null then
                  okay := conn.marker_is_text (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.v19), BT);
               else
                  okay := conn.marker_is_text (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.a19.all), BT);
               end if;
            when ft_chain =>
               if zone.a17 = null then
                  okay := conn.marker_is_blob (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.v17), BB);
               else
                  okay := conn.marker_is_blob (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.a17.all), BB);
               end if;
            when ft_bits =>
               if zone.a20 = null then
                  okay := conn.marker_is_blob (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.v20), BB);
               else
                  okay := conn.marker_is_blob (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.a20.all), BB);
               end if;
            when ft_utf8 =>
               if zone.a21 = null then
                  okay := conn.marker_is_text (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.v21), BT);
               else
                  okay := conn.marker_is_text (Stmt.stmt_handle, marker,
                                               zone.a21.all, BT);
               end if;
            when ft_geometry =>
               if zone.a22 = null then
                  okay := conn.marker_is_blob (Stmt.stmt_handle, marker,
                                               ARC.convert (zone.v22), BB);
               else
                  okay := conn.marker_is_blob
                    (Stmt.stmt_handle, marker,
                     WKB.Construct_WKB (zone.a22.all), BB);
               end if;

         end case;
         if not okay then
            Stmt.log_problem (category   => statement_execution,
                              message    => "failed to bind " & vartype'Img &
                                            " type to marker " & marker'Img,
                              pull_codes => True,
                              break      => True);
         end if;
      end if;
      return product;
   end construct_bind_slot;


   ----------------------
   --  reclaim_canvas  --
   ----------------------
   procedure reclaim_canvas (Stmt : out SQLite_statement)
   is
      use type BND.ICS.char_array_access;
      use type BND.ICS.chars_ptr;
   begin
      for x in Positive range 1 .. Natural (Stmt.bind_canvas.Length) loop
         declare
            SC : sqlite_canvas renames Stmt.bind_canvas.Element (x);
            BT : BND.ICS.chars_ptr         := SC.buffer_text;
            BB : BND.ICS.char_array_access := SC.buffer_binary;
         begin
            if BT /= BND.ICS.Null_Ptr then
               BND.ICS.Free (BT);
            end if;
            if BB /= null then
               free_binary (BB);
            end if;
         end;
      end loop;
      Stmt.bind_canvas.Clear;
   end reclaim_canvas;


   ----------------------
   --  fetch_next_set  --
   ----------------------
   overriding
   procedure fetch_next_set (Stmt         : out SQLite_statement;
                             data_present : out Boolean;
                             data_fetched : out Boolean)
   is
      pragma Unreferenced (Stmt);

      --  Stored precedures are not supported on SQLite
      --  There's nothting that would generate multiple result sets
      --  with a single query.
   begin
      data_fetched := False;
      data_present := False;
   end fetch_next_set;


   ------------------
   --  bit_string  --
   ------------------
   function seems_like_bit_string (candidate : CT.Text) return Boolean
   is
      canstr : String := CT.USS (candidate);
   begin
      if canstr'Length > 64 then
         return False;
      end if;
      for x in canstr'Range loop
         if canstr (x) /= '0' and then canstr (x) /= '1' then
            return False;
         end if;
      end loop;
      return True;
   end seems_like_bit_string;


end AdaBase.Statement.Base.SQLite;
