--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Exceptions;
with Ada.Calendar.Time_Zones;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;

package body AdaBase.Statement.Base.MySQL is

   package EX  renames Ada.Exceptions;
   package CTZ renames Ada.Calendar.Time_Zones;

   --------------------
   --  discard_rest  --
   --------------------
   overriding
   procedure discard_rest (Stmt : out MySQL_statement)
   is
      use type ABM.MYSQL_RES_Access;
      use type ABM.MYSQL_STMT_Access;
   begin
      case Stmt.type_of_statement is
      when direct_statement =>
         if Stmt.result_handle /= null then
            Stmt.rows_leftover := True;
            Stmt.mysql_conn.free_result (Stmt.result_handle);
            Stmt.clear_column_information;
         end if;
      when prepared_statement =>
         if Stmt.stmt_handle /= null then
            Stmt.rows_leftover := True;
            Stmt.mysql_conn.prep_free_result (Stmt.stmt_handle);
         end if;
      end case;
      Stmt.delivery := completed;
   end discard_rest;


   --------------------
   --  column_count  --
   --------------------
   overriding
   function column_count (Stmt : MySQL_statement) return Natural is
   begin
      return Stmt.num_columns;
   end column_count;


   ---------------------------
   --  last_driver_message  --
   ---------------------------
   overriding
   function last_driver_message (Stmt : MySQL_statement) return String is
   begin
      case Stmt.type_of_statement is
         when direct_statement   =>
            return Stmt.mysql_conn.driverMessage;
         when prepared_statement =>
            return Stmt.mysql_conn.prep_DriverMessage
              (Stmt.stmt_handle);
      end case;

   end last_driver_message;


   ----------------------
   --  last_insert_id  --
   ----------------------
   overriding
   function last_insert_id (Stmt : MySQL_statement) return Trax_ID is
   begin
      case Stmt.type_of_statement is
         when direct_statement   =>
            return Stmt.mysql_conn.lastInsertID;
         when prepared_statement =>
            return Stmt.mysql_conn.prep_LastInsertID
              (Stmt.stmt_handle);
      end case;
   end last_insert_id;


   ----------------------
   --  last_sql_state  --
   ----------------------
   overriding
   function last_sql_state (Stmt : MySQL_statement) return SQL_State is
   begin
      case Stmt.type_of_statement is
         when direct_statement   =>
            return Stmt.mysql_conn.SqlState;
         when prepared_statement =>
            return Stmt.mysql_conn.prep_SqlState
              (Stmt.stmt_handle);
      end case;
   end last_sql_state;


   ------------------------
   --  last_driver_code  --
   ------------------------
   overriding
   function last_driver_code (Stmt : MySQL_statement) return Driver_Codes is
   begin
      case Stmt.type_of_statement is
         when direct_statement   =>
            return Stmt.mysql_conn.driverCode;
         when prepared_statement =>
            return Stmt.mysql_conn.prep_DriverCode
              (Stmt.stmt_handle);
      end case;
   end last_driver_code;


   ----------------------------
   --  execute  (version 1)  --
   ----------------------------
   overriding
   function execute (Stmt : out MySQL_statement) return Boolean
   is
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
         --  Check to make sure all prepared markers are bound
         for sx in Natural range 1 .. num_markers loop
            if not Stmt.realmccoy.Element (sx).bound then
               raise STMT_PREPARATION
                 with "Prep Stmt column" & sx'Img & " unbound";
            end if;
         end loop;
         declare
            slots : ABM.MYSQL_BIND_Array (1 .. num_markers);
            vault : mysql_canvases (1 .. num_markers);
         begin
            for sx in slots'Range loop
               Stmt.construct_bind_slot (struct => slots (sx),
                                         canvas => vault (sx),
                                         marker => sx);
            end loop;

            if not Stmt.mysql_conn.prep_bind_parameters
              (Stmt.stmt_handle, slots)
            then
               Stmt.log_problem (category => statement_preparation,
                                 message => "failed to bind parameters",
                                 pull_codes => True);
               status_successful := False;
            end if;

            if status_successful then
               Stmt.log_nominal (category => statement_execution,
                                 message => "Exec with" & num_markers'Img &
                                   " bound parameters");
               if Stmt.mysql_conn.prep_execute (Stmt.stmt_handle) then
                  Stmt.successful_execution := True;
               else
                  Stmt.log_problem (category => statement_execution,
                                    message => "failed to exec prep stmt",
                                    pull_codes => True);
                  status_successful := False;
               end if;
            end if;

            --  Recover dynamically allocated data
            for sx in slots'Range loop
               free_binary (vault (sx).buffer_binary);
            end loop;
         end;
      else
         --  No binding required, just execute the prepared statement
         Stmt.log_nominal (category => statement_execution,
                           message => "Exec without bound parameters");
         if Stmt.mysql_conn.prep_execute (Stmt.stmt_handle) then
            Stmt.successful_execution := True;
         else
            Stmt.log_problem (category => statement_execution,
                              message => "failed to exec prep stmt",
                              pull_codes => True);
            status_successful := False;
         end if;
      end if;

      Stmt.internal_post_prep_stmt;

      return status_successful;
   end execute;


   ----------------------------
   --  execute  (version 2)  --
   ----------------------------
   overriding
   function execute (Stmt : out MySQL_statement; parameters : String;
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
   --  initialize  --
   ------------------
   overriding
   procedure initialize (Object : in out MySQL_statement)
   is
      use type ACM.MySQL_Connection_Access;
   begin
      if Object.mysql_conn = null then
         return;
      end if;

      logger_access     := Object.log_handler;
      Object.dialect    := driver_mysql;
      Object.connection := ACB.Base_Connection_Access (Object.mysql_conn);
      case Object.type_of_statement is
         when direct_statement =>
            Object.sql_final := new String'(CT.trim_sql
                                            (Object.initial_sql.all));
            Object.internal_direct_post_exec;
         when prepared_statement =>
            declare
               use type ABM.MYSQL_RES_Access;
            begin
               Object.sql_final := new String'(Object.transform_sql
                                               (Object.initial_sql.all));
               Object.mysql_conn.initialize_and_prepare_statement
                 (stmt => Object.stmt_handle, sql => Object.sql_final.all);
               declare
                  params : Natural := Object.mysql_conn.prep_markers_found
                    (stmt => Object.stmt_handle);
                  errmsg : String := "marker mismatch," &
                    Object.realmccoy.Length'Img & " expected but" &
                    params'Img & " found by MySQL";
               begin
                  if params /= Natural (Object.realmccoy.Length) then
                     raise ILLEGAL_BIND_SQL with errmsg;
                  end if;
                  Object.log_nominal (category => statement_preparation,
                                      message => Object.sql_final.all);
               end;
               Object.result_handle := Object.mysql_conn.prep_result_metadata
                 (Object.stmt_handle);
               --  Direct statements always produce result sets, but prepared
               --  statements very well may not. The next procedure ends early
               --  after clearing column data if there is results.
               Object.scan_column_information;
               if Object.result_handle /= null then
                  Object.mysql_conn.free_result (Object.result_handle);
               end if;
            exception
               when HELL : others =>
                  Object.log_problem (category => statement_preparation,
                                      message  => EX.Exception_Message (HELL));
                  raise;
            end;
      end case;
   end initialize;


   --------------
   --  Adjust  --
   --------------
   overriding
   procedure Adjust (Object : in out MySQL_statement) is
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
   procedure finalize (Object : in out MySQL_statement) is
   begin
      if Object.assign_counter /= 2 then
         return;
      end if;

      if Object.type_of_statement = prepared_statement then
         if not Object.mysql_conn.prep_close_statement (Object.stmt_handle)
         then
            Object.log_problem (category   => statement_preparation,
                                message    => "Deallocating statement memory",
                                pull_codes => True);
         end if;
      end if;
      free_sql (Object.sql_final);
      Object.reclaim_canvas;
   end finalize;


   ---------------------
   --  direct_result  --
   ---------------------
   procedure process_direct_result (Stmt : out MySQL_statement)
   is
      use type ABM.MYSQL_RES_Access;
   begin
      case Stmt.con_buffered is
         when True => Stmt.mysql_conn.store_result
              (result_handle => Stmt.result_handle);
         when False => Stmt.mysql_conn.use_result
              (result_handle => Stmt.result_handle);
      end case;
      Stmt.result_present := (Stmt.result_handle /= null);
   end process_direct_result;


   ---------------------
   --  rows_returned  --
   ---------------------
   overriding
   function rows_returned (Stmt : MySQL_statement) return Affected_Rows is
   begin
      if not Stmt.successful_execution then
         raise PRIOR_EXECUTION_FAILED
           with "Has query been executed yet?";
      end if;
      if Stmt.result_present then
         if Stmt.con_buffered then
            return Stmt.size_of_rowset;
         else
            raise INVALID_FOR_RESULT_SET
              with "Row set size is not known (Use query buffers to fix)";
         end if;
      else
         raise INVALID_FOR_RESULT_SET
           with "Result set not found; use rows_affected";
      end if;
   end rows_returned;


   ----------------------
   --  reclaim_canvas  --
   ----------------------
   procedure reclaim_canvas (Stmt : out MySQL_statement)
   is
      use type ABM.ICS.char_array_access;
   begin
      if Stmt.bind_canvas /= null then
         for sx in Stmt.bind_canvas.all'Range loop
            if Stmt.bind_canvas (sx).buffer_binary /= null then
               free_binary (Stmt.bind_canvas (sx).buffer_binary);
            end if;
         end loop;
         free_canvas (Stmt.bind_canvas);
      end if;
   end reclaim_canvas;


   --------------------------------
   --  clear_column_information  --
   --------------------------------
   procedure clear_column_information (Stmt : out MySQL_statement) is
   begin
      Stmt.num_columns := 0;
      Stmt.column_info.Clear;
      Stmt.crate.Clear;
      Stmt.headings_map.Clear;
      Stmt.reclaim_canvas;
   end clear_column_information;


   -------------------------------
   --  scan_column_information  --
   -------------------------------
   procedure scan_column_information (Stmt : out MySQL_statement)
   is
      use type ABM.MYSQL_FIELD_Access;
      use type ABM.MYSQL_RES_Access;
      field : ABM.MYSQL_FIELD_Access;
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
   begin
      Stmt.clear_column_information;
      if Stmt.result_handle = null then
         return;
      end if;
      Stmt.num_columns := Stmt.mysql_conn.fields_in_result
                          (Stmt.result_handle);
      loop
         field := Stmt.mysql_conn.fetch_field
           (result_handle => Stmt.result_handle);
         exit when field = null;
         declare
            info : column_info;
            brec : bindrec;
         begin
            brec.v00         := False;   --  placeholder
            info.field_name  := fn (Stmt.mysql_conn.field_name_field (field));
            info.table       := fn (Stmt.mysql_conn.field_name_table (field));
            info.mysql_type  := field.field_type;
            info.null_possible := Stmt.mysql_conn.field_allows_null (field);
            Stmt.mysql_conn.field_data_type (field    => field,
                                             std_type => info.field_type,
                                             size     => info.field_size);
            if info.field_size > Stmt.con_max_blob then
               info.field_size := Stmt.con_max_blob;
            end if;
            Stmt.column_info.Append (New_Item => info);
            --  The following pre-populates for bind support
            Stmt.crate.Append (New_Item => brec);
            Stmt.headings_map.Insert
              (Key => sn (Stmt.mysql_conn.field_name_field (field)),
               New_Item => Stmt.crate.Last_Index);
         end;
      end loop;
   end scan_column_information;


   -------------------
   --  column_name  --
   -------------------
   overriding
   function column_name (Stmt : MySQL_statement; index : Positive)
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
   function column_table (Stmt : MySQL_statement; index : Positive)
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
   function column_native_type (Stmt : MySQL_statement; index : Positive)
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
   function fetch_next (Stmt : out MySQL_statement) return ARS.Datarow is
   begin
      if Stmt.delivery = completed then
         return ARS.Empty_Datarow;
      end if;
      case Stmt.type_of_statement is
         when prepared_statement =>
            return Stmt.internal_ps_fetch_row;
         when direct_statement =>
            return Stmt.internal_fetch_row;
      end case;
   end fetch_next;


   -------------------
   --  fetch_bound  --
   -------------------
   overriding
   function fetch_bound (Stmt : out MySQL_statement) return Boolean is
   begin
      if Stmt.delivery = completed then
         return False;
      end if;
      case Stmt.type_of_statement is
         when prepared_statement =>
            return Stmt.internal_ps_fetch_bound;
         when direct_statement =>
            return Stmt.internal_fetch_bound;
      end case;
   end fetch_bound;


   -----------------
   --  fetch_all  --
   -----------------
   overriding
   function fetch_all (Stmt : out MySQL_statement) return ARS.Datarow_Set
   is
      maxrows : Natural := Natural (Stmt.rows_returned);
      tmpset  : ARS.Datarow_Set (1 .. maxrows + 1);
      nullset : ARS.Datarow_Set (1 .. 0);
      index   : Natural := 1;
      row     : ARS.Datarow;
   begin
      if (Stmt.delivery = completed) or else (maxrows = 0) then
         return nullset;
      end if;
      --  It is possible that one or more rows was individually fetched
      --  before the entire set was fetched.  Let's consider this legal so
      --  use a repeat loop to check each row and return a partial set
      --  if necessary.
      loop
         tmpset (index) := Stmt.fetch_next;
         exit when tmpset (index).data_exhausted;
         index := index + 1;
         exit when index > maxrows + 1;  --  should never happen
      end loop;
      if index = 1 then
         return nullset;   --  nothing was fetched
      end if;
      return tmpset (1 .. index - 1);
   end fetch_all;


   ----------------------
   --  fetch_next_set  --
   ----------------------
   overriding
   procedure fetch_next_set (Stmt         : out MySQL_statement;
                             data_present : out Boolean;
                             data_fetched : out Boolean)
   is
     use type ABM.MYSQL_RES_Access;
   begin
      data_fetched := False;
      if Stmt.result_handle /= null then
         Stmt.mysql_conn.free_result (Stmt.result_handle);
      end if;
      data_present := Stmt.mysql_conn.fetch_next_set;
      if not data_present then
         return;
      end if;
      declare
      begin
         Stmt.process_direct_result;
      exception
         when others =>
            Stmt.log_nominal (category => statement_execution,
                              message  => "Result set missing from: "
                                          & Stmt.sql_final.all);
            return;
      end;
      Stmt.internal_direct_post_exec (newset => True);
      data_fetched := True;
   end fetch_next_set;


   --------------------------
   --  internal_fetch_row  --
   --------------------------
   function internal_fetch_row (Stmt : out MySQL_statement)
                                return ARS.Datarow
   is
      use type ABM.ICS.chars_ptr;
      use type ABM.MYSQL_ROW_access;
      rptr : ABM.MYSQL_ROW_access :=
        Stmt.mysql_conn.fetch_row (Stmt.result_handle);
   begin
      if rptr = null then
         Stmt.delivery := completed;
         Stmt.mysql_conn.free_result (Stmt.result_handle);
         Stmt.clear_column_information;
         return ARS.Empty_Datarow;
      end if;
      Stmt.delivery := progressing;

      declare
         maxlen : constant Natural := Natural (Stmt.column_info.Length);
         bufmax : constant ABM.IC.size_t := ABM.IC.size_t (Stmt.con_max_blob);
         subtype data_buffer is ABM.IC.char_array (1 .. bufmax);
         type db_access is access all data_buffer;
         type rowtype is array (1 .. maxlen) of db_access;
         type rowtype_access is access all rowtype;

         row    : rowtype_access;
         result : ARS.Datarow;

         field_lengths : constant ACM.fldlen := Stmt.mysql_conn.fetch_lengths
           (result_handle => Stmt.result_handle,
            num_columns   => maxlen);

         function convert is new Ada.Unchecked_Conversion
           (Source => ABM.MYSQL_ROW_access, Target => rowtype_access);

         function db_convert (dba : db_access; size : Natural) return String;
         function db_convert (dba : db_access; size : Natural) return String
         is
            max : Natural := size;
         begin
            if max > Stmt.con_max_blob then
               max := Stmt.con_max_blob;
            end if;
            declare
               result : String (1 .. max);
            begin
               for x in result'Range loop
                  result (x) := Character (dba.all (ABM.IC.size_t (x)));
               end loop;
               return result;
            end;
         end db_convert;
      begin
         row := convert (rptr);
         for F in 1 .. maxlen loop
            declare
               colinfo  : column_info renames Stmt.column_info.Element (F);
               field    : ARF.Std_Field;
               last_one : constant Boolean := (F = maxlen);
               heading  : constant String := CT.USS (colinfo.field_name);
               isnull   : constant Boolean := (row (F) = null);
               sz : constant Natural := field_lengths (F);
               ST : constant String  := db_convert (row (F), sz);
               dvariant : ARF.Variant;
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
                     dvariant := (datatype => ft_widetext,
                                  v14 => convert (ST));
                  when ft_supertext =>
                     dvariant := (datatype => ft_supertext,
                                  v15 => convert (ST));
                  when ft_timestamp =>
                     begin
                        dvariant := (datatype => ft_timestamp,
                                     v16 => ARC.convert (ST));
                     exception
                        when AR.CONVERSION_FAILED =>
                           dvariant := (datatype => ft_textual,
                                        v13 => CT.SUS (ST));
                     end;
                  when ft_enumtype =>
                     dvariant := (datatype => ft_enumtype,
                                  V18 => ARC.convert (CT.SUS (ST)));
                  when ft_chain =>   null;
                  when ft_settype => null;
                  when ft_bits =>    null;
                  end case;
                  case colinfo.field_type is
                  when ft_chain =>
                     field := ARF.spawn_field (binob => ARC.convert (ST));
                  when ft_bits =>
                     field := ARF.spawn_bits_field
                       (convert_to_bitstring (ST, colinfo.field_size * 8));
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
      end;

   end internal_fetch_row;


   ------------------
   --  bincopy #1  --
   ------------------
   function bincopy (data : ABM.ICS.char_array_access;
                     datalen, max_size : Natural) return String
   is
      reslen : Natural := datalen;
   begin
      if reslen > max_size then
         reslen := max_size;
      end if;
      declare
         result : String (1 .. reslen) := (others => '_');
      begin
         for x in result'Range loop
            result (x) := Character (data.all (ABM.IC.size_t (x)));
         end loop;
         return result;
      end;
   end bincopy;


   ------------------
   --  bincopy #2  --
   ------------------
   function bincopy (data : ABM.ICS.char_array_access;
                     datalen, max_size : Natural;
                     hard_limit : Natural := 0) return AR.Chain
   is
      reslen   : Natural := datalen;
      chainlen : Natural := data.all'Length;
   begin
      if reslen > max_size then
         reslen := max_size;
      end if;
      if hard_limit > 0 then
         chainlen := hard_limit;
      else
         chainlen := reslen;
      end if;
      declare
         result : AR.Chain (1 .. chainlen) := (others => 0);
         jimmy : Character;
      begin
         for x in Natural range 1 .. reslen loop
            jimmy := Character (data.all (ABM.IC.size_t (x)));
            result (x) := AR.NByte1 (Character'Pos (jimmy));
         end loop;
         return result;
      end;
   end bincopy;


   -----------------------------
   --  internal_ps_fetch_row  --
   -----------------------------
   function internal_ps_fetch_row (Stmt : out MySQL_statement)
                                   return ARS.Datarow
   is
      use type ABM.ICS.chars_ptr;
      use type ABM.MYSQL_ROW_access;
      use type ACM.fetch_status;
      status : ACM.fetch_status;
   begin
      status := Stmt.mysql_conn.prep_fetch_bound (Stmt.stmt_handle);
      if status = ACM.spent then
         Stmt.delivery := completed;
         Stmt.clear_column_information;
      elsif status = ACM.truncated then
         Stmt.log_nominal (category => statement_execution,
                           message  => "data truncated");
         Stmt.delivery := progressing;
      elsif status = ACM.error then
         Stmt.log_problem (category => statement_execution,
                           message  => "prep statement fetch error",
                           pull_codes => True);
         Stmt.delivery := completed;
      else
         Stmt.delivery := progressing;
      end if;
      if Stmt.delivery = completed then
         return ARS.Empty_Datarow;
      end if;

      declare
         maxlen : constant Natural := Stmt.num_columns;
         result : ARS.Datarow;
      begin
         for F in 1 .. maxlen loop
            declare
               use type ABM.enum_field_types;
               cv       : mysql_canvas renames Stmt.bind_canvas (F);
               colinfo  : column_info renames Stmt.column_info.Element (F);
               dvariant : ARF.Variant;
               field    : ARF.Std_Field;
               last_one : constant Boolean := (F = maxlen);
               datalen  : constant Natural := Natural (cv.length);
               heading  : constant String := CT.USS (colinfo.field_name);
               isnull   : constant Boolean := (Natural (cv.is_null) = 1);
               mtype    : ABM.enum_field_types := colinfo.mysql_type;
            begin
               if isnull then
                  field := ARF.spawn_null_field (colinfo.field_type);
               else
                  case colinfo.field_type is
                  when ft_nbyte0 =>
                     dvariant := (datatype => ft_nbyte0,
                                  v00 => Natural (cv.buffer_uint8) = 1);
                  when ft_nbyte1 =>
                     dvariant := (datatype => ft_nbyte1,
                                  v01 => AR.NByte1 (cv.buffer_uint8));
                  when ft_nbyte2 =>
                     dvariant := (datatype => ft_nbyte2,
                                  v02 => AR.NByte2 (cv.buffer_uint16));
                  when ft_nbyte3 =>
                     dvariant := (datatype => ft_nbyte3,
                                  v03 => AR.NByte3 (cv.buffer_uint32));
                  when ft_nbyte4 =>
                     dvariant := (datatype => ft_nbyte4,
                                  v04 => AR.NByte4 (cv.buffer_uint32));
                  when ft_nbyte8 =>
                     dvariant := (datatype => ft_nbyte8,
                                  v05 => AR.NByte8 (cv.buffer_uint64));
                  when ft_byte1  =>
                     dvariant := (datatype => ft_byte1,
                                  v06 => AR.Byte1 (cv.buffer_int8));
                  when ft_byte2  =>
                     dvariant := (datatype => ft_byte2,
                                  v07 => AR.Byte2 (cv.buffer_int16));
                  when ft_byte3  =>
                     dvariant := (datatype => ft_byte3,
                                  v08 => AR.Byte3 (cv.buffer_int32));
                  when ft_byte4  =>
                     dvariant := (datatype => ft_byte4,
                                  v09 => AR.Byte4 (cv.buffer_int32));
                  when ft_byte8  =>
                     dvariant := (datatype => ft_byte8,
                                  v10 => AR.Byte8 (cv.buffer_int64));
                  when ft_real9  =>
                     if mtype = ABM.MYSQL_TYPE_NEWDECIMAL or else
                       mtype = ABM.MYSQL_TYPE_DECIMAL
                     then
                        dvariant := (datatype => ft_real9,
                                     v11 => convert (bincopy (cv.buffer_binary,
                                       datalen, Stmt.con_max_blob)));
                     else
                        dvariant := (datatype => ft_real9,
                                     v11 => AR.Real9 (cv.buffer_float));
                     end if;
                  when ft_real18 =>
                     if mtype = ABM.MYSQL_TYPE_NEWDECIMAL or else
                       mtype = ABM.MYSQL_TYPE_DECIMAL
                     then
                        dvariant := (datatype => ft_real18,
                                     v12 => convert (bincopy (cv.buffer_binary,
                                       datalen, Stmt.con_max_blob)));
                     else
                        dvariant := (datatype => ft_real18,
                                     v12 => AR.Real18 (cv.buffer_double));
                     end if;
                  when ft_textual =>
                     dvariant := (datatype => ft_textual,
                                  v13 => CT.SUS (bincopy (cv.buffer_binary,
                                    datalen, Stmt.con_max_blob)));
                  when ft_widetext =>
                     dvariant := (datatype => ft_widetext,
                                  v14 => convert (bincopy (cv.buffer_binary,
                                    datalen, Stmt.con_max_blob)));
                  when ft_supertext =>
                     dvariant := (datatype => ft_supertext,
                                  v15 => convert (bincopy (cv.buffer_binary,
                                    datalen, Stmt.con_max_blob)));
                  when ft_timestamp =>
                     declare
                        year  : Natural := Natural (cv.buffer_time.year);
                        month : Natural := Natural (cv.buffer_time.month);
                        day   : Natural := Natural (cv.buffer_time.day);
                     begin
                        if year < CAL.Year_Number'First or else
                          year > CAL.Year_Number'Last
                        then
                           year := CAL.Year_Number'First;
                        end if;
                        if month < CAL.Month_Number'First or else
                          month > CAL.Month_Number'Last
                        then
                           month := CAL.Month_Number'First;
                        end if;
                        if day < CAL.Day_Number'First or else
                          day > CAL.Day_Number'Last
                        then
                           day := CAL.Day_Number'First;
                        end if;
                        dvariant :=
                          (datatype => ft_timestamp,
                           v16 => CFM.Time_Of
                             (Year => year,
                              Month => month,
                              Day => day,
                              Hour => Natural (cv.buffer_time.hour),
                              Minute => Natural (cv.buffer_time.minute),
                              Second => Natural (cv.buffer_time.second),
                              Sub_Second => CFM.Second_Duration (Natural
                                (cv.buffer_time.second_part) / 1000000))
                          );
                     end;
                  when ft_enumtype =>
                     dvariant := (datatype => ft_enumtype, v18 => ARC.convert
                                  (CT.SUS (bincopy (cv.buffer_binary, datalen,
                                     Stmt.con_max_blob))));
                  when ft_settype => null;
                  when ft_chain   => null;
                  when ft_bits    => null;
                  end case;
                  case colinfo.field_type is
                  when ft_chain =>
                     field := ARF.spawn_field
                       (binob => bincopy (cv.buffer_binary, datalen,
                        Stmt.con_max_blob));
                  when ft_bits =>
                     field := ARF.spawn_bits_field
                       (convert_to_bitstring
                          (bincopy (cv.buffer_binary, datalen,
                           Stmt.con_max_blob), datalen * 8));
                  when ft_settype =>
                     field := ARF.spawn_field
                       (enumset => bincopy (cv.buffer_binary, datalen,
                        Stmt.con_max_blob));
                  when others =>
                     field := ARF.spawn_field
                       (data => dvariant,
                        null_data => isnull);
                  end case;
               end if;

               result.push (heading    => heading,
                            field      => field,
                            last_field => last_one);
            end;
         end loop;
         return result;
      end;

   end internal_ps_fetch_row;


   -------------------------------
   --  internal_ps_fetch_bound  --
   -------------------------------
   function internal_ps_fetch_bound (Stmt : out MySQL_statement)
                                     return Boolean
   is
      use type ABM.ICS.chars_ptr;
      use type ACM.fetch_status;
      status : ACM.fetch_status;
   begin
      status := Stmt.mysql_conn.prep_fetch_bound (Stmt.stmt_handle);
      if status = ACM.spent then
         Stmt.delivery := completed;
         Stmt.clear_column_information;
      elsif status = ACM.truncated then
         Stmt.log_nominal (category => statement_execution,
                           message  => "data truncated");
         Stmt.delivery := progressing;
      elsif status = ACM.error then
         Stmt.log_problem (category => statement_execution,
                           message  => "prep statement fetch error",
                           pull_codes => True);
         Stmt.delivery := completed;
      else
         Stmt.delivery := progressing;
      end if;
      if Stmt.delivery = completed then
         return False;
      end if;

      declare
         maxlen : constant Natural := Stmt.num_columns;
      begin
         for F in 1 .. maxlen loop
            if not Stmt.crate.Element (Index => F).bound then
               goto continue;
            end if;

            declare
               use type ABM.enum_field_types;
               cv      : mysql_canvas renames Stmt.bind_canvas (F);
               colinfo : column_info renames Stmt.column_info.Element (F);
               param   : bindrec renames Stmt.crate.Element (F);
               datalen : constant Natural := Natural (cv.length);
               Tout    : constant field_types := param.output_type;
               Tnative : constant field_types := colinfo.field_type;
               mtype   : ABM.enum_field_types := colinfo.mysql_type;
               errmsg  : constant String  := "native type : " &
                         field_types'Image (Tnative) & " binding type : " &
                         field_types'Image (Tout);
            begin
               --  Derivation of implementation taken from PostgreSQL
               --  Only guaranteed successful converstions allowed though

               case Tout is
                  when ft_nbyte2 =>
                     case Tnative is
                        when ft_nbyte1 | ft_nbyte2 =>
                           null;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_nbyte3 =>
                     case Tnative is
                        when ft_nbyte1 | ft_nbyte2 | ft_nbyte3 =>
                           null;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_nbyte4 =>
                     case Tnative is
                        when ft_nbyte1 | ft_nbyte2 | ft_nbyte3 | ft_nbyte4 =>
                           null;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_nbyte8 =>
                     case Tnative is
                        when ft_nbyte1 | ft_nbyte2 | ft_nbyte3 | ft_nbyte4 |
                             ft_nbyte8 =>
                           null;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_byte2 =>
                     case Tnative is
                        when ft_byte1 | ft_byte2 =>
                           null;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_byte3 =>
                     case Tnative is
                        when ft_byte1 | ft_byte2 | ft_byte3 =>
                           null;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_byte4 =>
                     case Tnative is
                        when ft_byte1 | ft_byte2 | ft_byte3 | ft_byte4 =>
                           null;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_byte8 =>
                     case Tnative is
                        when ft_byte1 | ft_byte2 | ft_byte3 | ft_byte4 |
                           ft_byte8 =>
                           null;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_real18 =>
                     case Tnative is
                        when ft_real9 | ft_real18 =>
                           null;  -- guaranteed to convert without loss
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when others =>
                     if Tnative /= Tout then
                        raise BINDING_TYPE_MISMATCH with errmsg;
                     end if;
               end case;

               case Tout is
                  when ft_nbyte0 => param.a00.all := (Natural (cv.buffer_uint8) = 1);
                  when ft_nbyte1 => param.a01.all := AR.NByte1 (cv.buffer_uint8);
                  when ft_nbyte2 => param.a02.all := AR.NByte2 (cv.buffer_uint16);
                  when ft_nbyte3 => param.a03.all := AR.NByte3 (cv.buffer_uint32);
                  when ft_nbyte4 => param.a04.all := AR.NByte4 (cv.buffer_uint32);
                  when ft_nbyte8 => param.a05.all := AR.NByte8 (cv.buffer_uint64);
                  when ft_byte1  => param.a06.all := AR.Byte1 (cv.buffer_int8);
                  when ft_byte2  => param.a07.all := AR.Byte2 (cv.buffer_int16);
                  when ft_byte3  => param.a08.all := AR.Byte3 (cv.buffer_int32);
                  when ft_byte4  => param.a09.all := AR.Byte4 (cv.buffer_int32);
                  when ft_byte8  => param.a10.all := AR.Byte8 (cv.buffer_int64);
                  when ft_real9  =>
                     if mtype = ABM.MYSQL_TYPE_NEWDECIMAL or else
                       mtype = ABM.MYSQL_TYPE_DECIMAL
                     then
                        param.a11.all := convert (bincopy (cv.buffer_binary,
                                              datalen, Stmt.con_max_blob));
                     else
                        param.a11.all := AR.Real9 (cv.buffer_float);
                     end if;
                  when ft_real18 =>
                     if mtype = ABM.MYSQL_TYPE_NEWDECIMAL or else
                       mtype = ABM.MYSQL_TYPE_DECIMAL
                     then
                        param.a12.all := convert (bincopy (cv.buffer_binary,
                                              datalen, Stmt.con_max_blob));
                     else
                        param.a12.all := AR.Real18 (cv.buffer_double);
                     end if;
                  when ft_textual =>
                     param.a13.all :=
                       CT.SUS (bincopy (cv.buffer_binary, datalen,
                               Stmt.con_max_blob));
                  when ft_widetext => param.a14.all :=
                       convert (bincopy (cv.buffer_binary, datalen,
                                Stmt.con_max_blob));
                  when ft_supertext => param.a15.all :=
                       convert (bincopy (cv.buffer_binary, datalen,
                                Stmt.con_max_blob));
                  when ft_timestamp =>
                     declare
                        year  : Natural := Natural (cv.buffer_time.year);
                        month : Natural := Natural (cv.buffer_time.month);
                        day   : Natural := Natural (cv.buffer_time.day);
                     begin
                        if year < CAL.Year_Number'First or else
                          year > CAL.Year_Number'Last
                        then
                           year := CAL.Year_Number'First;
                        end if;
                        if month < CAL.Month_Number'First or else
                          month > CAL.Month_Number'Last
                        then
                           month := CAL.Month_Number'First;
                        end if;
                        if day < CAL.Day_Number'First or else
                          day > CAL.Day_Number'Last
                        then
                           day := CAL.Day_Number'First;
                        end if;
                        param.a16.all := CFM.Time_Of
                          (Year => year,
                           Month => month,
                           Day => day,
                           Hour => Natural (cv.buffer_time.hour),
                           Minute => Natural (cv.buffer_time.minute),
                           Second => Natural (cv.buffer_time.second),
                           Sub_Second => CFM.Second_Duration (Natural
                             (cv.buffer_time.second_part) / 1000000));
                     end;
                  when ft_chain =>
                     if param.a17.all'Length < datalen then
                           raise BINDING_SIZE_MISMATCH with "native size : " &
                             param.a17.all'Length'Img &
                             " less than binding size : " & datalen'Img;
                     end if;
                     param.a17.all := bincopy
                       (cv.buffer_binary, datalen, Stmt.con_max_blob,
                        param.a17.all'Length);
                  when ft_bits =>
                     declare
                        strval : String := bincopy (cv.buffer_binary, datalen,
                                                    Stmt.con_max_blob);
                        FL    : Natural := param.a20.all'Length;
                        DVLEN : Natural := strval'Length * 8;
                     begin
                        if FL < DVLEN then
                           raise BINDING_SIZE_MISMATCH with "native size : " &
                             FL'Img & " less than binding size : " & DVLEN'Img;
                        end if;

                        param.a20.all :=
                          ARC.convert (convert_to_bitstring (strval, FL));
                     end;
                  when ft_enumtype =>
                     param.a18.all :=
                       ARC.convert (CT.SUS (bincopy (cv.buffer_binary, datalen,
                                Stmt.con_max_blob)));
                  when ft_settype =>
                     declare
                        setstr : constant String := bincopy
                          (cv.buffer_binary, datalen, Stmt.con_max_blob);
                        num_items : constant Natural := num_set_items (setstr);
                     begin
                        if param.a19.all'Length < num_items
                        then
                           raise BINDING_SIZE_MISMATCH with "native size : " &
                             param.a19.all'Length'Img &
                             " less than binding size : " & num_items'Img;
                        end if;
                        param.a19.all := ARC.convert (setstr,
                                                      param.a19.all'Length);
                     end;
               end case;
            end;
            <<continue>>
            null;
         end loop;
         return True;
      end;

   end internal_ps_fetch_bound;


   -----------------------------------
   --  internal_fetch_bound_direct  --
   -----------------------------------
   function internal_fetch_bound (Stmt : out MySQL_statement) return Boolean
   is
      use type ABM.ICS.chars_ptr;
      use type ABM.MYSQL_ROW_access;
      rptr : ABM.MYSQL_ROW_access :=
        Stmt.mysql_conn.fetch_row (Stmt.result_handle);
   begin
      if rptr = null then
         Stmt.delivery := completed;
         Stmt.mysql_conn.free_result (Stmt.result_handle);
         Stmt.clear_column_information;
         return False;
      end if;
      Stmt.delivery := progressing;

      declare
         maxlen : constant Natural := Natural (Stmt.column_info.Length);
         bufmax : constant ABM.IC.size_t := ABM.IC.size_t (Stmt.con_max_blob);
         subtype data_buffer is ABM.IC.char_array (1 .. bufmax);
         type db_access is access all data_buffer;
         type rowtype is array (1 .. maxlen) of db_access;
         type rowtype_access is access all rowtype;

         row : rowtype_access;
         field_lengths : constant ACM.fldlen := Stmt.mysql_conn.fetch_lengths
           (result_handle => Stmt.result_handle,
            num_columns   => maxlen);

         function Convert is new Ada.Unchecked_Conversion
           (Source => ABM.MYSQL_ROW_access, Target => rowtype_access);

         function db_convert (dba : db_access; size : Natural) return String;
         function db_convert (dba : db_access; size : Natural) return String
         is
            max : Natural := size;
         begin
            if max > Stmt.con_max_blob then
               max := Stmt.con_max_blob;
            end if;
            declare
               result : String (1 .. max);
            begin
               for x in result'Range loop
                  result (x) := Character (dba.all (ABM.IC.size_t (x)));
               end loop;
               return result;
            end;
         end db_convert;
      begin
         row := Convert (rptr);
         for F in 1 .. maxlen loop
            declare
               use type ABM.enum_field_types;
               dossier  : bindrec renames Stmt.crate.Element (F);
               colinfo  : column_info renames Stmt.column_info.Element (F);
               mtype    : ABM.enum_field_types := colinfo.mysql_type;
               isnull   : constant Boolean := (row (F) = null);
               sz       : constant Natural := field_lengths (F);
               ST       : constant String  := db_convert (row (F), sz);
               Tout     : constant field_types := dossier.output_type;
               Tnative  : constant field_types := colinfo.field_type;
               errmsg   : constant String  := "native type : " &
                          field_types'Image (Tnative) & " binding type : " &
                          field_types'Image (Tout);
            begin
               if not dossier.bound then
                  goto continue;
               end if;

               if isnull or else CT.IsBlank (ST) then
                  set_as_null (dossier);
                  goto continue;
               end if;

               --  Derivation of implementation taken from PostgreSQL
               --  Only guaranteed successful converstions allowed though

               case Tout is
                  when ft_nbyte2 =>
                     case Tnative is
                        when ft_nbyte1 | ft_nbyte2 =>
                           null;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_nbyte3 =>
                     case Tnative is
                        when ft_nbyte1 | ft_nbyte2 | ft_nbyte3 =>
                           null;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_nbyte4 =>
                     case Tnative is
                        when ft_nbyte1 | ft_nbyte2 | ft_nbyte3 | ft_nbyte4 =>
                           null;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_nbyte8 =>
                     case Tnative is
                        when ft_nbyte1 | ft_nbyte2 | ft_nbyte3 | ft_nbyte4 |
                             ft_nbyte8 =>
                           null;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_byte2 =>
                     case Tnative is
                        when ft_byte1 | ft_byte2 =>
                           null;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_byte3 =>
                     case Tnative is
                        when ft_byte1 | ft_byte2 | ft_byte3 =>
                           null;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_byte4 =>
                     case Tnative is
                        when ft_byte1 | ft_byte2 | ft_byte3 | ft_byte4 =>
                           null;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_byte8 =>
                     case Tnative is
                        when ft_byte1 | ft_byte2 | ft_byte3 | ft_byte4 |
                           ft_byte8 =>
                           null;
                        when others =>
                           raise BINDING_TYPE_MISMATCH with errmsg;
                     end case;
                  when ft_real18 =>
                     case Tnative is
                        when ft_real9 | ft_real18 =>
                           null;  -- guaranteed to convert without loss
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
                  when ft_widetext  => dossier.a14.all := convert (ST);
                  when ft_supertext => dossier.a15.all := convert (ST);
                  when ft_enumtype  => dossier.a18.all := ARC.convert (ST);
                  when ft_textual   =>
                     dossier.a13.all := CT.SUS (ST);
                  when ft_timestamp =>
                     begin
                        dossier.a16.all := ARC.convert (ST);
                     exception
                        when AR.CONVERSION_FAILED =>
                           dossier.a16.all := AR.PARAM_IS_TIMESTAMP;
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
                  when ft_bits =>
                     declare
                        FL    : Natural := dossier.a20.all'Length;
                        DVLEN : Natural := ST'Length * 8;
                     begin
                        if DVLEN > FL then
                           raise BINDING_SIZE_MISMATCH with "native size : " &
                             DVLEN'Img & " greater than binding size : " &
                             FL'Img;
                        end if;
                        dossier.a20.all :=
                          ARC.convert (convert_to_bitstring (ST, FL));
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
         return True;
      end;
   end internal_fetch_bound;


   ----------------------------------
   --  internal_direct_post_exec   --
   ----------------------------------
   procedure internal_direct_post_exec (Stmt : out MySQL_statement;
                                        newset : Boolean := False) is
   begin
      Stmt.successful_execution := False;
      Stmt.size_of_rowset := 0;
      if newset then
         Stmt.log_nominal (category => statement_execution,
                           message => "Fetch next rowset from: "
                                       & Stmt.sql_final.all);
      else
         Stmt.connection.execute (sql => Stmt.sql_final.all);
         Stmt.log_nominal (category => statement_execution,
                           message => Stmt.sql_final.all);
         Stmt.process_direct_result;
      end if;
      Stmt.successful_execution := True;
      if Stmt.result_present then
         Stmt.scan_column_information;
         if Stmt.con_buffered then
            Stmt.size_of_rowset := Stmt.mysql_conn.rows_in_result
                                     (Stmt.result_handle);
         end if;
         Stmt.delivery := pending;
      else
         declare
            returned_cols : Natural;
         begin
            returned_cols := Stmt.mysql_conn.field_count;
            if returned_cols = 0 then
               Stmt.impacted := Stmt.mysql_conn.rows_affected_by_execution;
            else
               raise ACM.RESULT_FAIL with "Columns returned without result";
            end if;
         end;
         Stmt.delivery := completed;
      end if;

   exception
      when ACM.QUERY_FAIL =>
         Stmt.log_problem (category   => statement_execution,
                           message    => Stmt.sql_final.all,
                           pull_codes => True);
      when RES : ACM.RESULT_FAIL =>
         Stmt.log_problem (category   => statement_execution,
                           message    => EX.Exception_Message (X => RES),
                           pull_codes => True);
   end internal_direct_post_exec;


   -------------------------------
   --  internal_post_prep_stmt  --
   -------------------------------
   procedure internal_post_prep_stmt (Stmt : out MySQL_statement)
   is
      use type mysql_canvases_Access;
   begin
      Stmt.delivery := completed;  --  default for early returns
      if Stmt.num_columns = 0 then
         Stmt.result_present := False;
         Stmt.impacted := Stmt.mysql_conn.prep_rows_affected_by_execution
                           (Stmt.stmt_handle);
         return;
      end if;
      Stmt.result_present := True;

      if Stmt.bind_canvas /= null then
         raise STMT_PREPARATION with
           "Previous bind canvas present (expected to be null)";
      end if;
      Stmt.bind_canvas := new mysql_canvases (1 .. Stmt.num_columns);

      declare
         slots : ABM.MYSQL_BIND_Array (1 .. Stmt.num_columns);
         ft    : field_types;
         fsize : Natural;
      begin
         for sx in slots'Range loop
            slots (sx).is_null     := Stmt.bind_canvas (sx).is_null'Access;
            slots (sx).length      := Stmt.bind_canvas (sx).length'Access;
            slots (sx).error       := Stmt.bind_canvas (sx).error'Access;
            slots (sx).buffer_type := Stmt.column_info.Element (sx).mysql_type;
            ft := Stmt.column_info.Element (sx).field_type;
            case slots (sx).buffer_type is
               when ABM.MYSQL_TYPE_DOUBLE =>
                  slots (sx).buffer :=
                    Stmt.bind_canvas (sx).buffer_double'Address;
               when ABM.MYSQL_TYPE_FLOAT =>
                  slots (sx).buffer :=
                    Stmt.bind_canvas (sx).buffer_float'Address;
               when ABM.MYSQL_TYPE_NEWDECIMAL | ABM.MYSQL_TYPE_DECIMAL =>
                  --  Don't set buffer_type to FLOAT or DOUBLE.  MySQL will
                  --  automatically convert it, but precision will be lost.
                  --  Ask for a string and let's convert that ourselves.
                  slots (sx).buffer_type := ABM.MYSQL_TYPE_NEWDECIMAL;
                  fsize := Stmt.column_info.Element (sx).field_size;
                  slots (sx).buffer_length := ABM.IC.unsigned_long (fsize);
                  Stmt.bind_canvas (sx).buffer_binary := new ABM.IC.char_array
                    (1 .. ABM.IC.size_t (fsize));
                  slots (sx).buffer :=
                    Stmt.bind_canvas (sx).buffer_binary.all'Address;
               when ABM.MYSQL_TYPE_TINY =>
                  if ft = ft_nbyte0 or else ft = ft_nbyte1 then
                     slots (sx).is_unsigned := 1;
                     slots (sx).buffer :=
                       Stmt.bind_canvas (sx).buffer_uint8'Address;
                  else
                     slots (sx).buffer :=
                       Stmt.bind_canvas (sx).buffer_int8'Address;
                  end if;
               when ABM.MYSQL_TYPE_SHORT | ABM.MYSQL_TYPE_YEAR =>
                  if ft = ft_nbyte2 then
                     slots (sx).is_unsigned := 1;
                     slots (sx).buffer :=
                       Stmt.bind_canvas (sx).buffer_uint16'Address;
                  else
                     slots (sx).buffer :=
                       Stmt.bind_canvas (sx).buffer_int16'Address;
                  end if;
               when ABM.MYSQL_TYPE_INT24 | ABM.MYSQL_TYPE_LONG =>
                  if ft = ft_nbyte3 or else ft = ft_nbyte4 then
                     slots (sx).is_unsigned := 1;
                     slots (sx).buffer :=
                       Stmt.bind_canvas (sx).buffer_uint32'Address;
                  else
                     slots (sx).buffer :=
                       Stmt.bind_canvas (sx).buffer_int32'Address;
                  end if;
               when ABM.MYSQL_TYPE_LONGLONG =>
                  if ft = ft_nbyte8 then
                     slots (sx).is_unsigned := 1;
                     slots (sx).buffer :=
                       Stmt.bind_canvas (sx).buffer_uint64'Address;
                  else
                     slots (sx).buffer :=
                       Stmt.bind_canvas (sx).buffer_int64'Address;
                  end if;
               when ABM.MYSQL_TYPE_DATE | ABM.MYSQL_TYPE_TIMESTAMP |
                    ABM.MYSQL_TYPE_TIME | ABM.MYSQL_TYPE_DATETIME =>
                  slots (sx).buffer :=
                    Stmt.bind_canvas (sx).buffer_time'Address;
               when ABM.MYSQL_TYPE_BIT | ABM.MYSQL_TYPE_TINY_BLOB |
                    ABM.MYSQL_TYPE_MEDIUM_BLOB | ABM.MYSQL_TYPE_LONG_BLOB |
                    ABM.MYSQL_TYPE_BLOB | ABM.MYSQL_TYPE_STRING |
                    ABM.MYSQL_TYPE_VAR_STRING =>
                  fsize := Stmt.column_info.Element (sx).field_size;
                  slots (sx).buffer_length := ABM.IC.unsigned_long (fsize);
                  Stmt.bind_canvas (sx).buffer_binary := new ABM.IC.char_array
                    (1 .. ABM.IC.size_t (fsize));
                  slots (sx).buffer :=
                    Stmt.bind_canvas (sx).buffer_binary.all'Address;
               when ABM.MYSQL_TYPE_NULL | ABM.MYSQL_TYPE_NEWDATE |
                    ABM.MYSQL_TYPE_VARCHAR | ABM.MYSQL_TYPE_GEOMETRY |
                    ABM.MYSQL_TYPE_ENUM | ABM.MYSQL_TYPE_SET =>
                  raise STMT_PREPARATION with
                    "Unsupported MySQL type for result binding attempted";
            end case;
         end loop;

         if not Stmt.mysql_conn.prep_bind_result (Stmt.stmt_handle, slots)
         then
            Stmt.log_problem (category => statement_preparation,
                              message => "failed to bind result structures",
                              pull_codes => True);
            return;
         end if;
      end;

      if Stmt.con_buffered then
         Stmt.mysql_conn.prep_store_result (Stmt.stmt_handle);
         Stmt.size_of_rowset := Stmt.mysql_conn.prep_rows_in_result
                                (Stmt.stmt_handle);
      end if;
      Stmt.delivery := pending;

   end internal_post_prep_stmt;


   ---------------------------
   --  construct_bind_slot  --
   ---------------------------
   procedure construct_bind_slot (Stmt   : MySQL_statement;
                                  struct : out ABM.MYSQL_BIND;
                                  canvas : out mysql_canvas;
                                  marker : Positive)
   is
      procedure set_binary_buffer (Str : String);

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
      use type AR.Bits_Access;

      procedure set_binary_buffer (Str : String)
      is
         len : constant ABM.IC.size_t := ABM.IC.size_t (Str'Length);
      begin
         canvas.buffer_binary := new ABM.IC.char_array (1 .. len);
         canvas.buffer_binary.all := ABM.IC.To_C (Str, False);
         canvas.length := ABM.IC.unsigned_long (len);

         struct.buffer        := canvas.buffer_binary.all'Address;
         struct.buffer_length := ABM.IC.unsigned_long (len);
         struct.length        := canvas.length'Unchecked_Access;
         struct.is_null       := canvas.is_null'Unchecked_Access;
      end set_binary_buffer;

   begin
      case vartype is
         when ft_nbyte0 | ft_nbyte1 | ft_nbyte2 | ft_nbyte3 | ft_nbyte4 |
              ft_nbyte8 => struct.is_unsigned := 1;
         when others => null;
      end case;
      case vartype is
         when ft_nbyte0 =>     struct.buffer_type := ABM.MYSQL_TYPE_TINY;
         when ft_nbyte1 =>     struct.buffer_type := ABM.MYSQL_TYPE_TINY;
         when ft_nbyte2 =>     struct.buffer_type := ABM.MYSQL_TYPE_SHORT;
         when ft_nbyte3 =>     struct.buffer_type := ABM.MYSQL_TYPE_LONG;
         when ft_nbyte4 =>     struct.buffer_type := ABM.MYSQL_TYPE_LONG;
         when ft_nbyte8 =>     struct.buffer_type := ABM.MYSQL_TYPE_LONGLONG;
         when ft_byte1 =>      struct.buffer_type := ABM.MYSQL_TYPE_TINY;
         when ft_byte2 =>      struct.buffer_type := ABM.MYSQL_TYPE_SHORT;
         when ft_byte3 =>      struct.buffer_type := ABM.MYSQL_TYPE_LONG;
         when ft_byte4 =>      struct.buffer_type := ABM.MYSQL_TYPE_LONG;
         when ft_byte8 =>      struct.buffer_type := ABM.MYSQL_TYPE_LONGLONG;
         when ft_real9 =>      struct.buffer_type := ABM.MYSQL_TYPE_FLOAT;
         when ft_real18 =>     struct.buffer_type := ABM.MYSQL_TYPE_DOUBLE;
         when ft_textual =>    struct.buffer_type := ABM.MYSQL_TYPE_STRING;
         when ft_widetext =>   struct.buffer_type := ABM.MYSQL_TYPE_STRING;
         when ft_supertext =>  struct.buffer_type := ABM.MYSQL_TYPE_STRING;
         when ft_timestamp =>  struct.buffer_type := ABM.MYSQL_TYPE_DATETIME;
         when ft_chain =>      struct.buffer_type := ABM.MYSQL_TYPE_BLOB;
         when ft_enumtype =>   struct.buffer_type := ABM.MYSQL_TYPE_STRING;
         when ft_settype =>    struct.buffer_type := ABM.MYSQL_TYPE_STRING;
         when ft_bits =>       struct.buffer_type := ABM.MYSQL_TYPE_STRING;
      end case;
      if zone.null_data then
         canvas.is_null := 1;
         struct.buffer_type := ABM.MYSQL_TYPE_NULL;
      else
         case vartype is
         when ft_nbyte0 =>
            struct.buffer := canvas.buffer_uint8'Address;
            if zone.a00 = null then
               if zone.v00 then
                  canvas.buffer_uint8 := 1;
               end if;
            else
               if zone.a00.all then
                  canvas.buffer_uint8 := 1;
               end if;
            end if;
         when ft_nbyte1 =>
            struct.buffer := canvas.buffer_uint8'Address;
            if zone.a01 = null then
               canvas.buffer_uint8 := ABM.IC.unsigned_char (zone.v01);
            else
               canvas.buffer_uint8 := ABM.IC.unsigned_char (zone.a01.all);
            end if;
         when ft_nbyte2 =>
            struct.buffer := canvas.buffer_uint16'Address;
            if zone.a02 = null then
               canvas.buffer_uint16 := ABM.IC.unsigned_short (zone.v02);
            else
               canvas.buffer_uint16 := ABM.IC.unsigned_short (zone.a02.all);
            end if;
         when ft_nbyte3 =>
            struct.buffer := canvas.buffer_uint32'Address;
            --  ABM.MYSQL_TYPE_INT24 not for input, use next biggest
            if zone.a03 = null then
               canvas.buffer_uint32 := ABM.IC.unsigned (zone.v03);
            else
               canvas.buffer_uint32 := ABM.IC.unsigned (zone.a03.all);
            end if;
         when ft_nbyte4 =>
            struct.buffer := canvas.buffer_uint32'Address;
            if zone.a04 = null then
               canvas.buffer_uint32 := ABM.IC.unsigned (zone.v04);
            else
               canvas.buffer_uint32 := ABM.IC.unsigned (zone.a04.all);
            end if;
         when ft_nbyte8 =>
            struct.buffer := canvas.buffer_uint64'Address;
            if zone.a05 = null then
               canvas.buffer_uint64 := ABM.IC.unsigned_long (zone.v05);
            else
               canvas.buffer_uint64 := ABM.IC.unsigned_long (zone.a05.all);
            end if;
         when ft_byte1 =>
            struct.buffer := canvas.buffer_int8'Address;
            if zone.a06 = null then
               canvas.buffer_int8 := ABM.IC.signed_char (zone.v06);
            else
               canvas.buffer_int8 := ABM.IC.signed_char (zone.a06.all);
            end if;
         when ft_byte2 =>
            struct.buffer := canvas.buffer_int16'Address;
            if zone.a07 = null then
               canvas.buffer_int16 := ABM.IC.short (zone.v07);
            else
               canvas.buffer_int16 := ABM.IC.short (zone.a07.all);
            end if;
         when ft_byte3 =>
            struct.buffer := canvas.buffer_int32'Address;
            --  ABM.MYSQL_TYPE_INT24 not for input, use next biggest
            if zone.a08 = null then
               canvas.buffer_int32 := ABM.IC.int (zone.v08);
            else
               canvas.buffer_int32 := ABM.IC.int (zone.a08.all);
            end if;
         when ft_byte4 =>
            struct.buffer := canvas.buffer_int32'Address;
            if zone.a09 = null then
               canvas.buffer_int32 := ABM.IC.int (zone.v09);
            else
               canvas.buffer_int32 := ABM.IC.int (zone.a09.all);
            end if;
         when ft_byte8 =>
            struct.buffer := canvas.buffer_int64'Address;
            if zone.a10 = null then
               canvas.buffer_int64 := ABM.IC.long (zone.v10);
            else
               canvas.buffer_int64 := ABM.IC.long (zone.a10.all);
            end if;
         when ft_real9 =>
            struct.buffer := canvas.buffer_float'Address;
            if zone.a11 = null then
               canvas.buffer_float := ABM.IC.C_float (zone.v11);
            else
               canvas.buffer_float := ABM.IC.C_float (zone.a11.all);
            end if;
         when ft_real18 =>
            struct.buffer := canvas.buffer_double'Address;
            if zone.a12 = null then
               canvas.buffer_double := ABM.IC.double (zone.v12);
            else
               canvas.buffer_double := ABM.IC.double (zone.a12.all);
            end if;
         when ft_textual =>
            if zone.a13 = null then
               set_binary_buffer (ARC.convert (zone.v13));
            else
               set_binary_buffer (ARC.convert (zone.a13.all));
            end if;
         when ft_widetext =>
            if zone.a14 = null then
               set_binary_buffer (ARC.convert (zone.v14));
            else
               set_binary_buffer (ARC.convert (zone.a14.all));
            end if;
         when ft_supertext =>
            if zone.a15 = null then
               set_binary_buffer (ARC.convert (zone.v15));
            else
               set_binary_buffer (ARC.convert (zone.a15.all));
            end if;
         when ft_timestamp =>
            struct.buffer := canvas.buffer_time'Address;
            declare
               hack : CAL.Time;
            begin
               if zone.a16 = null then
                  hack := zone.v16;
               else
                  hack := zone.a16.all;
               end if;
               --  Negative time not supported
               canvas.buffer_time.year   := ABM.IC.unsigned (CFM.Year (hack));
               canvas.buffer_time.month  := ABM.IC.unsigned (CFM.Month (hack));
               canvas.buffer_time.day    := ABM.IC.unsigned (CFM.Day (hack));
               canvas.buffer_time.hour   := ABM.IC.unsigned (CFM.Hour (hack));
               canvas.buffer_time.minute := ABM.IC.unsigned
                                            (CFM.Minute (hack));
               canvas.buffer_time.second := ABM.IC.unsigned
                                            (CFM.Second (hack));
               canvas.buffer_time.second_part :=
                 ABM.IC.unsigned_long (CFM.Sub_Second (hack) * 1000000);
            end;
         when ft_chain =>
            if zone.a17 = null then
               set_binary_buffer (CT.USS (zone.v17));
            else
               set_binary_buffer (ARC.convert (zone.a17.all));
            end if;
         when ft_enumtype =>
            if zone.a18 = null then
               set_binary_buffer (ARC.convert (zone.v18.enumeration));
            else
               set_binary_buffer (ARC.convert (zone.a18.all.enumeration));
            end if;
         when ft_settype =>
            if zone.a19 = null then
               set_binary_buffer (CT.USS (zone.v19));
            else
               set_binary_buffer (ARC.convert (zone.a19.all));
            end if;
            when ft_bits =>
            if zone.a20 = null then
               set_binary_buffer (CT.USS (zone.v20));
            else
               set_binary_buffer (ARC.convert (zone.a20.all));
            end if;
         end case;
      end if;

   end construct_bind_slot;


   -------------------
   --  log_problem  --
   -------------------
   procedure log_problem
     (statement  : MySQL_statement;
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


   ----------------------------
   --  convert_to_bitstring  --
   ----------------------------
   function convert_to_bitstring (nv : String; width : Natural) return String
   is
      use type AR.NByte1;
      result : String (1 .. width) := (others => '0');
      marker : Natural;
      lode   : AR.NByte1;
      mask   : constant array (0 .. 7) of AR.NByte1 := (2 ** 0, 2 ** 1,
                                                        2 ** 2, 2 ** 3,
                                                        2 ** 4, 2 ** 5,
                                                        2 ** 6, 2 ** 7);
   begin
      --  We can't seem to get the true size, e.g 12 bits shows as 2,
      --  for two bytes, which could represent up to 16 bits.  Thus, we
      --  return a variable width in multiples of 8.  MySQL doesn't mind
      --  leading zeros.

      marker := result'Last;

      for x in reverse nv'Range loop
         lode := AR.NByte1 (Character'Pos (nv (x)));
         for position in mask'Range loop
            if (lode and mask (position)) > 0 then
               result (marker) := '1';
            end if;
            exit when marker = result'First;
            marker := marker - 1;
         end loop;
      end loop;
      return result;

   end convert_to_bitstring;


end AdaBase.Statement.Base.MySQL;
