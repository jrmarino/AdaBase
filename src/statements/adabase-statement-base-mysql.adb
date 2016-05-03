--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Exceptions;
with Ada.Characters.Handling;
with AdaBase.Results.Field;
with Ada.Unchecked_Conversion;

package body AdaBase.Statement.Base.MySQL is

   package EX  renames Ada.Exceptions;
   package ARF renames AdaBase.Results.Field;
   package ACH renames Ada.Characters.Handling;

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
         end if;
      when prepared_statement =>
         if Stmt.stmt_handle /= null then
            Stmt.rows_leftover := True;
            Stmt.mysql_conn.prep_free_result (Stmt.stmt_handle);
         end if;
      end case;
      Stmt.clear_column_information;
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
   function last_insert_id (Stmt : MySQL_statement) return TraxID is
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
   function last_sql_state (Stmt : MySQL_statement) return TSqlState is
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
   function last_driver_code (Stmt : MySQL_statement) return DriverCodes is
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
           with "execute is for prepared statements";
      end if;
      Stmt.successful_execution := False;
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
   function execute (Stmt : out MySQL_statement; bind_piped : String)
                     return Boolean
   is
   begin
      if Stmt.type_of_statement = direct_statement then
         raise INVALID_FOR_DIRECT_QUERY
           with "execute is for prepared statements";
      end if;
      --  TODO : IMPLEMENT
      --  Change, use strings directly (no double conversion)
      raise ILLEGAL_BIND_SQL with "to be implemented";
      return Stmt.execute;
   end execute;


   ------------------
   --  initialize  --
   ------------------
   overriding
   procedure initialize (Object : in out MySQL_statement)
   is
      use type ACM.MySQL_Connection_Access;
      len : Natural := CT.len (Object.initial_sql.all);
   begin
      if Object.mysql_conn = null then
         return;
      end if;

      logger_access     := Object.log_handler;
      Object.dialect    := driver_mysql;
      Object.sql_final  := new String (1 .. len);
      Object.connection := ACB.Base_Connection_Access (Object.mysql_conn);
      case Object.type_of_statement is
         when direct_statement =>
            Object.sql_final.all := CT.USS (Object.initial_sql.all);
            Object.internal_direct_post_exec;
         when prepared_statement =>
            declare
               use type ABM.MYSQL_RES_Access;
            begin
               Object.transform_sql (sql => CT.USS (Object.initial_sql.all),
                                     new_sql => Object.sql_final.all);
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


   ----------------
   --  finalize  --
   ----------------
   overriding
   procedure finalize (Object : in out MySQL_statement) is
   begin
      if Object.type_of_statement = prepared_statement then
         if not Object.mysql_conn.prep_close_statement (Object.stmt_handle)
         then
            Object.log_problem (category   => statement_preparation,
                                message    => "Deallocating statement memory",
                                pull_codes => True);
         end if;
      end if;
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
   function rows_returned (Stmt : MySQL_statement) return AffectedRows is
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


   --------------------------------
   --  clear_column_information  --
   --------------------------------
   procedure clear_column_information (Stmt : out MySQL_statement) is
   begin
      Stmt.num_columns := 0;
      Stmt.column_info.Clear;
      Stmt.crate.Clear;
      Stmt.headings_map.Clear;
      if Stmt.bind_canvas /= null then
         for sx in Stmt.bind_canvas.all'Range loop
            free_binary (Stmt.bind_canvas (sx).buffer_binary);
         end loop;
         free_canvas (Stmt.bind_canvas);
      end if;
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
   function fetch_next (Stmt    : out MySQL_statement;
                        datarow : out ARS.DataRow_Access) return Boolean
   is
      use type ARS.DataRow_Access;
   begin
      datarow := null;
      if Stmt.delivery = completed then
         return False;
      end if;
      case Stmt.type_of_statement is
         when prepared_statement =>
            datarow := Stmt.internal_ps_fetch_row;
         when direct_statement =>
            datarow := Stmt.internal_fetch_row;
      end case;
      return (datarow /= null);
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
   function fetch_all (Stmt : out MySQL_statement) return ARS.DataRowSet
   is
      maxrows : Natural := Natural (Stmt.rows_returned);
      tmpset  : ARS.DataRowSet (1 .. maxrows + 1) := (others => null);
      nullset : ARS.DataRowSet (1 .. 0);
      index   : Natural := 1;
   begin
      if (Stmt.delivery = completed) or else (maxrows = 0) then
         return nullset;
      end if;
      --  It is possible that one or more rows was individually fetched
      --  before the entire set was fetched.  Let's consider this legal so
      --  use a repeat loop to check each row and return a partial set
      --  if necessary.
      loop
         exit when not Stmt.fetch_next (tmpset (index));
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


   ---------------------------
   --  convert to Ada Time  --
   ---------------------------
   function convert (nv : String) return CAL.Time
   is
      len    : constant Natural  := nv'Length;
      NF     : constant Natural  := nv'First;
      year   : CAL.Year_Number   := CAL.Year_Number'First;
      month  : CAL.Month_Number  := CAL.Month_Number'First;
      day    : CAL.Day_Number    := CAL.Day_Number'First;
      hour   : CFM.Hour_Number   := CFM.Hour_Number'First;
      minute : CFM.Minute_Number := CFM.Minute_Number'First;
      second : CFM.Second_Number := CFM.Second_Number'First;
      HHMMSS : String (1 .. 8);
   begin
      --  Possible formats
      --  DATE      [10]: YYYY-MM-DD
      --  DATETIME  [19]: YYYY-MM-DD HH:MM:SS
      --  TIMESTAMP [19]: YYYY-MM-DD HH:MM:SS
      --  TIME      [08]: HH:MM:SS

      --  Handle HH:MM:SS formats
      case len is
         when  8 => HHMMSS := nv (NF .. NF + 7);
         when 19 => HHMMSS := nv (NF + 11 .. NF + 18);
         when others => null;
      end case;
      case len is
         when 8 | 19 =>
            hour   := CFM.Hour_Number   (Integer'Value (HHMMSS (1 .. 2)));
            minute := CFM.Minute_Number (Integer'Value (HHMMSS (4 .. 5)));
            second := CFM.Second_Number (Integer'Value (HHMMSS (7 .. 8)));
         when others => null;
      end case;

      --  Handle date formats
      case len is
         when 10 | 19 =>
            year  := CAL.Year_Number  (Integer'Value (nv (NF .. NF + 3)));
            month := CAL.Month_Number (Integer'Value (nv (NF + 5 .. NF + 6)));
            day   := CAL.Day_Number   (Integer'Value (nv (NF + 8 .. NF + 9)));
         when others => null;
      end case;

      --  If this raises an exception, it probable means the date < 1901 or
      --  greater than 2099.  Turn this into a string time in that case.
      return CFM.Time_Of (Year   => year,
                          Month  => month,
                          Day    => day,
                          Hour   => hour,
                          Minute => minute,
                          Second => second);
   end convert;


   ----------------------------------
   --  convert string to enumtype  --
   ----------------------------------
   function convert (nv : String) return AR.settype
   is
      num_enums : Natural := 1;
      nv_len    : Natural := nv'Length;
   begin
      for x in nv'Range loop
         if nv (x) = ',' then
            num_enums := num_enums + 1;
         end if;
      end loop;
      declare
         result : AR.settype (1 .. num_enums);
         cursor : Natural  := 1;
         curend : Natural  := 0;
         index  : Positive := 1;
      begin
         for x in nv'Range loop
            if nv (x) = ',' then
               result (index).enumeration := CT.SUS (nv (cursor .. curend));
               result (index).index := 0;  -- not supported on MySQL
               index := index + 1;
               cursor := x + 1;
            end if;
            curend := curend + 1;
         end loop;
         result (index).enumeration := CT.SUS (nv (cursor .. curend));
         result (index).index := 0;
         return result;
      end;
   end convert;


   --------------------------
   --  internal_fetch_row  --
   --------------------------
   function internal_fetch_row (Stmt : out MySQL_statement)
                                return ARS.DataRow_Access
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
         return null;
      end if;
      Stmt.delivery := progressing;

      declare
         maxlen : constant Natural := Natural (Stmt.column_info.Length);
         type rowtype is array (1 .. maxlen) of ABM.ICS.chars_ptr;
         type rowtype_access is access all rowtype;

         row    : rowtype_access;
         result : ARS.DataRow_Access := new ARS.DataRow;

         field_lengths : constant ACM.fldlen := Stmt.mysql_conn.fetch_lengths
           (result_handle => Stmt.result_handle,
            num_columns   => maxlen);

         function Convert is new Ada.Unchecked_Conversion
           (Source => ABM.MYSQL_ROW_access, Target => rowtype_access);
      begin
         row := Convert (rptr);
         for F in 1 .. maxlen loop
            declare
               field    : ARF.field_access;
               last_one : constant Boolean := (F = maxlen);
               heading  : constant String := CT.USS
                 (Stmt.column_info.Element (Index => F).field_name);
               sz : constant Natural := field_lengths (F);
               EN : constant Boolean := row (F) = ABM.ICS.Null_Ptr;
               ST : constant String  := ABM.ICS.Value
                 (Item => row (F), Length => ABM.IC.size_t (sz));
               dvariant : ARF.variant;
            begin
               case Stmt.column_info.Element (Index => F).field_type is
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
                     dvariant := (datatype => ft_textual,
                                  v13 => convert (ST, Stmt.con_max_blob));
                  when ft_widetext =>
                     dvariant := (datatype => ft_widetext,
                                  v14 => convert (ST, Stmt.con_max_blob));
                  when ft_supertext =>
                     dvariant := (datatype => ft_supertext,
                                  v15 => convert (ST, Stmt.con_max_blob));
                  when ft_timestamp =>
                     begin
                        dvariant := (datatype => ft_timestamp,
                                     v16 => convert (ST));
                     exception
                        when CAL.Time_Error =>
                           dvariant := (datatype => ft_textual, v13 =>
                                           convert (ST, Stmt.con_max_blob));
                     end;
                  when ft_enumtype =>
                     --  It seems that mysql doesn't give up the enum index
                     --  easily.  Set to "0" for all members
                     dvariant := (datatype => ft_enumtype,
                                  V18 => (enumeration =>
                                             convert (ST, Stmt.con_max_blob),
                                            index => 0));
                  when others =>
                     null;

               end case;
               case Stmt.column_info.Element (Index => F).field_type is
                  when ft_chain =>
                     field := ARF.spawn_field
                       (binob => convert (ST, Stmt.con_max_blob));
                  when ft_settype =>
                     field := ARF.spawn_field (enumset => convert (ST));
                  when others =>
                     field := ARF.spawn_field (data => dvariant,
                                               null_data => EN);
               end case;

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
                     datalen, max_size : Natural) return AR.chain
   is
      reslen : Natural := datalen;
   begin
      if reslen > max_size then
         reslen := max_size;
      end if;
      declare
         result : AR.chain (1 .. reslen) := (others => 0);
         jimmy : Character;
      begin
         for x in Natural range 1 .. reslen loop
            jimmy := Character (data.all (ABM.IC.size_t (x)));
            result (x) := AR.nbyte1 (Character'Pos (jimmy));
         end loop;
         return result;
      end;
   end bincopy;


   -----------------------------
   --  internal_ps_fetch_row  --
   -----------------------------
   function internal_ps_fetch_row (Stmt : out MySQL_statement)
                                   return ARS.DataRow_Access
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
         return null;
      end if;

      declare
         maxlen : constant Natural := Stmt.num_columns;
         result : ARS.DataRow_Access := new ARS.DataRow;
      begin
         for F in 1 .. maxlen loop
            declare
               cv       : mysql_canvas renames Stmt.bind_canvas (F);
               dvariant : ARF.variant;
               field    : ARF.field_access;
               last_one : constant Boolean := (F = maxlen);
               datalen  : constant Natural := Natural (cv.length);
               heading  : constant String := CT.USS
                 (Stmt.column_info.Element (Index => F).field_name);
            begin
               case Stmt.column_info.Element (Index => F).field_type is
                  when ft_nbyte0 =>
                     dvariant := (datatype => ft_nbyte0,
                                  v00 => Natural (cv.buffer_uint8) = 1);
                  when ft_nbyte1 =>
                     dvariant := (datatype => ft_nbyte1,
                                  v01 => AR.nbyte1 (cv.buffer_uint8));
                  when ft_nbyte2 =>
                     dvariant := (datatype => ft_nbyte2,
                                  v02 => AR.nbyte2 (cv.buffer_uint16));
                  when ft_nbyte3 =>
                     dvariant := (datatype => ft_nbyte3,
                                  v03 => AR.nbyte3 (cv.buffer_uint32));
                  when ft_nbyte4 =>
                     dvariant := (datatype => ft_nbyte4,
                                  v04 => AR.nbyte4 (cv.buffer_uint32));
                  when ft_nbyte8 =>
                     dvariant := (datatype => ft_nbyte8,
                                  v05 => AR.nbyte8 (cv.buffer_uint64));
                  when ft_byte1  =>
                     dvariant := (datatype => ft_byte1,
                                  v06 => AR.byte1 (cv.buffer_int8));
                  when ft_byte2  =>
                     dvariant := (datatype => ft_byte2,
                                  v07 => AR.byte2 (cv.buffer_int16));
                  when ft_byte3  =>
                     dvariant := (datatype => ft_byte3,
                                  v08 => AR.byte3 (cv.buffer_int32));
                  when ft_byte4  =>
                     dvariant := (datatype => ft_byte4,
                                  v09 => AR.byte4 (cv.buffer_int32));
                  when ft_byte8  =>
                     dvariant := (datatype => ft_byte8,
                                  v10 => AR.byte8 (cv.buffer_int64));
                  when ft_real9  =>
                     dvariant := (datatype => ft_real9,
                                  v11 => AR.real9 (cv.buffer_float));
                  when ft_real18 =>
                     dvariant := (datatype => ft_real18,
                                  v12 => AR.real18 (cv.buffer_double));
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
                        dvariant := (datatype => ft_timestamp,
                                  v16 => CFM.Time_Of
                                    (Year => year,
                                     Month => month,
                                     Day => day,
                                     Hour => Natural (cv.buffer_time.hour),
                                     Minute => Natural (cv.buffer_time.minute),
                                     Second => Natural (cv.buffer_time.second),
                                     Sub_Second => CFM.Second_Duration
                                       (Natural (cv.buffer_time.second_part)
                                        / 1000000))
                                 );
                     end;
                  when ft_enumtype =>
                     dvariant := (datatype => ft_enumtype,
                                  v18 => (enumeration => CT.SUS
                                          (bincopy (cv.buffer_binary,
                                             datalen, Stmt.con_max_blob)),
                                          index => 0));
                  when ft_settype => null;
                  when ft_chain => null;
               end case;
               case Stmt.column_info.Element (Index => F).field_type is
                  when ft_chain =>
                     field := ARF.spawn_field
                       (binob => bincopy (cv.buffer_binary, datalen,
                        Stmt.con_max_blob));
                  when ft_settype =>
                     field := ARF.spawn_field
                       (enumset => convert (bincopy (cv.buffer_binary, datalen,
                        Stmt.con_max_blob)));
                  when others =>
                     field := ARF.spawn_field
                       (data => dvariant,
                        null_data => Natural (cv.is_null) = 1);
               end case;

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
               cv      : mysql_canvas renames Stmt.bind_canvas (F);
               datalen : constant Natural := Natural (cv.length);
               Tout    : constant field_types :=
                         Stmt.crate.Element (Index => F).output_type;
               Tnative : constant field_types :=
                         Stmt.column_info.Element (Index => F).field_type;
            begin
               if Tnative /= Tout then
                  raise BINDING_TYPE_MISMATCH with "native type : " &
                    field_types'Image (Tnative) & " binding type : " &
                    field_types'Image (Tout);
               end if;
               case Tnative is
                  when ft_nbyte0 => Stmt.crate.Element (F).a00.all :=
                       (Natural (cv.buffer_uint8) = 1);
                  when ft_nbyte1 => Stmt.crate.Element (F).a01.all :=
                       AR.nbyte1 (cv.buffer_uint8);
                  when ft_nbyte2 => Stmt.crate.Element (F).a02.all :=
                       AR.nbyte2 (cv.buffer_uint16);
                  when ft_nbyte3 => Stmt.crate.Element (F).a03.all :=
                       AR.nbyte3 (cv.buffer_uint32);
                  when ft_nbyte4 => Stmt.crate.Element (F).a04.all :=
                       AR.nbyte4 (cv.buffer_uint32);
                  when ft_nbyte8 => Stmt.crate.Element (F).a05.all :=
                       AR.nbyte8 (cv.buffer_uint64);
                  when ft_byte1 => Stmt.crate.Element (F).a06.all :=
                       AR.byte1 (cv.buffer_int8);
                  when ft_byte2 => Stmt.crate.Element (F).a07.all :=
                       AR.byte2 (cv.buffer_int16);
                  when ft_byte3 => Stmt.crate.Element (F).a08.all :=
                       AR.byte3 (cv.buffer_int32);
                  when ft_byte4 => Stmt.crate.Element (F).a09.all :=
                       AR.byte4 (cv.buffer_int32);
                  when ft_byte8 => Stmt.crate.Element (F).a10.all :=
                       AR.byte8 (cv.buffer_int64);
                  when ft_real9  => Stmt.crate.Element (F).a11.all :=
                       AR.real9 (cv.buffer_float);
                  when ft_real18 => Stmt.crate.Element (F).a12.all :=
                       AR.real18 (cv.buffer_double);
                  when ft_textual => Stmt.crate.Element (F).a13.all :=
                       CT.SUS (bincopy (cv.buffer_binary, datalen,
                               Stmt.con_max_blob));
                  when ft_widetext => Stmt.crate.Element (F).a14.all :=
                       convert (bincopy (cv.buffer_binary, datalen,
                                Stmt.con_max_blob));
                  when ft_supertext => Stmt.crate.Element (F).a15.all :=
                       convert (bincopy (cv.buffer_binary, datalen,
                                Stmt.con_max_blob));
                  when ft_timestamp => Stmt.crate.Element (F).a16.all :=
                       CFM.Time_Of
                         (Year => Natural (cv.buffer_time.year),
                          Month => Natural (cv.buffer_time.month),
                          Day => Natural (cv.buffer_time.day),
                          Hour => Natural (cv.buffer_time.hour),
                          Minute => Natural (cv.buffer_time.minute),
                          Second => Natural (cv.buffer_time.second),
                          Sub_Second => CFM.Second_Duration
                            (Natural (cv.buffer_time.second_part) / 1000000));
                  when ft_chain =>
                     if Stmt.crate.Element (F).a17.all'Length /= datalen then
                           raise BINDING_SIZE_MISMATCH with "native size : " &
                             Stmt.crate.Element (F).a17.all'Length'Img &
                             " binding size : " & datalen'Img;
                     end if;
                     Stmt.crate.Element (F).a17.all :=
                       bincopy (cv.buffer_binary, datalen, Stmt.con_max_blob);
                  when ft_enumtype =>
                     Stmt.crate.Element (F).a18.all.index := 0;
                     Stmt.crate.Element (F).a18.all.enumeration :=
                       CT.SUS (bincopy (cv.buffer_binary, datalen,
                               Stmt.con_max_blob));
                  when ft_settype =>
                     Stmt.crate.Element (F).a19.all := convert
                       (bincopy (cv.buffer_binary, datalen,
                        Stmt.con_max_blob));
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
         type rowtype is array (1 .. maxlen) of ABM.ICS.chars_ptr;
         type rowtype_access is access all rowtype;

         row : rowtype_access;
         field_lengths : constant ACM.fldlen := Stmt.mysql_conn.fetch_lengths
           (result_handle => Stmt.result_handle,
            num_columns   => maxlen);

         function Convert is new Ada.Unchecked_Conversion
           (Source => ABM.MYSQL_ROW_access, Target => rowtype_access);
      begin
         row := Convert (rptr);
         for F in 1 .. maxlen loop
            if Stmt.crate.Element (Index => F).bound then
               declare
                  sz : constant Natural := field_lengths (F);
                  EN : constant Boolean := row (F) = ABM.ICS.Null_Ptr;
                  ST : constant String  := ABM.ICS.Value
                    (Item => row (F), Length => ABM.IC.size_t (sz));

                  Tout : constant field_types :=
                    Stmt.crate.Element (Index => F) .output_type;
                  Tnative : constant field_types :=
                    Stmt.column_info.Element (Index => F).field_type;
               begin
                  if Tnative /= Tout then
                     raise BINDING_TYPE_MISMATCH with "native type : " &
                       field_types'Image (Tnative) & " binding type : " &
                       field_types'Image (Tout);
                  end if;
                  case Tnative is
                     when ft_nbyte0 =>
                        Stmt.crate.Element (F).a00.all := (ST = "1");
                     when ft_nbyte1 =>
                        Stmt.crate.Element (F).a01.all := convert (ST);
                     when ft_nbyte2 =>
                        Stmt.crate.Element (F).a02.all := convert (ST);
                     when ft_nbyte3 =>
                        Stmt.crate.Element (F).a03.all := convert (ST);
                     when ft_nbyte4 =>
                        Stmt.crate.Element (F).a04.all := convert (ST);
                     when ft_nbyte8 =>
                        Stmt.crate.Element (F).a05.all := convert (ST);
                     when ft_byte1 =>
                        Stmt.crate.Element (F).a06.all := convert (ST);
                     when ft_byte2 =>
                        Stmt.crate.Element (F).a07.all := convert (ST);
                     when ft_byte3 =>
                        Stmt.crate.Element (F).a08.all := convert (ST);
                     when ft_byte4 =>
                        Stmt.crate.Element (F).a09.all := convert (ST);
                     when ft_byte8 =>
                        Stmt.crate.Element (F).a10.all := convert (ST);
                     when ft_real9  =>
                        Stmt.crate.Element (F).a11.all := convert (ST);
                     when ft_real18 =>
                        Stmt.crate.Element (F).a12.all := convert (ST);
                     when ft_textual =>
                        Stmt.crate.Element (F).a13.all :=
                          convert (ST, Stmt.con_max_blob);
                     when ft_widetext =>
                        Stmt.crate.Element (F).a14.all :=
                          convert (ST, Stmt.con_max_blob);
                     when ft_supertext =>
                        Stmt.crate.Element (F).a15.all :=
                          convert (ST, Stmt.con_max_blob);
                     when ft_timestamp =>
                        begin
                           Stmt.crate.Element (F).a16.all := convert (ST);
                        exception
                           when CAL.Time_Error =>
                              Stmt.crate.Element (F).a16.all := CAL.Time_Of
                                (Year  => CAL.Year_Number'First,
                                 Month => CAL.Month_Number'First,
                                 Day   => CAL.Day_Number'First);
                        end;
                     when ft_chain =>
                        if Stmt.crate.Element (F).a17.all'Length /= sz then
                           raise BINDING_SIZE_MISMATCH with "native size : " &
                             Stmt.crate.Element (F).a17.all'Length'Img &
                             " binding size : " & sz'Img;
                        end if;
                        Stmt.crate.Element (F).a17.all :=
                          convert (ST, Stmt.con_max_blob);
                     when ft_enumtype =>
                        --  It seems that mysql doesn't give up the enum index
                        --  easily.  Set to "0" for all members
                        Stmt.crate.Element (F).a18.all :=
                          (enumeration => convert (ST, Stmt.con_max_blob),
                           index => 0);
                     when ft_settype =>
                        Stmt.crate.Element (F).a19.all := convert (ST);
                  end case;
               end;
            end if;
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
               when ABM.MYSQL_TYPE_DECIMAL | ABM.MYSQL_TYPE_NEWDECIMAL =>
                  if Stmt.column_info.Element (sx).field_type = ft_real18 then
                     slots (sx).buffer :=
                       Stmt.bind_canvas (sx).buffer_double'Address;
                  else
                     slots (sx).buffer :=
                       Stmt.bind_canvas (sx).buffer_float'Address;
                  end if;
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

      use type AR.nbyte0_access;
      use type AR.nbyte1_access;
      use type AR.nbyte2_access;
      use type AR.nbyte3_access;
      use type AR.nbyte4_access;
      use type AR.nbyte8_access;
      use type AR.byte1_access;
      use type AR.byte2_access;
      use type AR.byte3_access;
      use type AR.byte4_access;
      use type AR.byte8_access;
      use type AR.real9_access;
      use type AR.real18_access;
      use type AR.str1_access;
      use type AR.str2_access;
      use type AR.str4_access;
      use type AR.time_access;
      use type AR.enum_access;
      use type AR.settype_access;

      procedure set_binary_buffer (Str : String)
      is
         len : constant ABM.IC.size_t := ABM.IC.size_t (Str'Length);
      begin
         canvas.buffer_binary := new ABM.IC.char_array (1 .. len);
         canvas.buffer_binary.all := ABM.IC.To_C (Str, False);
         canvas.length := ABM.IC.unsigned_long (len);

         struct.buffer       := canvas.buffer_binary.all'Address;
         struct.length       := canvas.length'Unchecked_Access;
      end set_binary_buffer;

   begin
      case vartype is
         when ft_nbyte0 | ft_nbyte1 | ft_nbyte2 | ft_nbyte3 | ft_nbyte4 |
              ft_nbyte8 => struct.is_unsigned := 1;
         when others => null;
      end case;
      case vartype is
         when ft_nbyte0 => struct.buffer_type := ABM.MYSQL_TYPE_TINY;
            if zone.a00 = null then
               if zone.v00 then
                  canvas.buffer_uint8 := 1;
               end if;
            else
               if zone.a00.all then
                  canvas.buffer_uint8 := 1;
               end if;
            end if;
            struct.buffer := canvas.buffer_uint8'Address;
         when ft_nbyte1 =>
            struct.buffer_type := ABM.MYSQL_TYPE_TINY;
            struct.buffer      := canvas.buffer_uint8'Address;
            if zone.a01 = null then
               canvas.buffer_uint8 := ABM.IC.unsigned_char (zone.v01);
            else
               canvas.buffer_uint8 := ABM.IC.unsigned_char (zone.a01.all);
            end if;
         when ft_nbyte2 =>
            struct.buffer_type := ABM.MYSQL_TYPE_SHORT;
            struct.buffer      := canvas.buffer_uint16'Address;
            if zone.a02 = null then
               canvas.buffer_uint16 := ABM.IC.unsigned_short (zone.v02);
            else
               canvas.buffer_uint16 := ABM.IC.unsigned_short (zone.a02.all);
            end if;
         when ft_nbyte3 =>
            struct.buffer_type := ABM.MYSQL_TYPE_LONG;
            struct.buffer      := canvas.buffer_uint32'Address;
            --  ABM.MYSQL_TYPE_INT24 not for input, use next biggest
            if zone.a03 = null then
               canvas.buffer_uint32 := ABM.IC.unsigned (zone.v03);
            else
               canvas.buffer_uint32 := ABM.IC.unsigned (zone.a03.all);
            end if;
         when ft_nbyte4 =>
            struct.buffer_type := ABM.MYSQL_TYPE_LONG;
            struct.buffer      := canvas.buffer_uint32'Address;
            if zone.a04 = null then
               canvas.buffer_uint32 := ABM.IC.unsigned (zone.v04);
            else
               canvas.buffer_uint32 := ABM.IC.unsigned (zone.a04.all);
            end if;
         when ft_nbyte8 =>
            struct.buffer_type := ABM.MYSQL_TYPE_LONGLONG;
            struct.buffer      := canvas.buffer_uint64'Address;
            if zone.a05 = null then
               canvas.buffer_uint64 := ABM.IC.unsigned_long (zone.v05);
            else
               canvas.buffer_uint64 := ABM.IC.unsigned_long (zone.a05.all);
            end if;
         when ft_byte1 =>
            struct.buffer_type := ABM.MYSQL_TYPE_TINY;
            struct.buffer      := canvas.buffer_int8'Address;
            if zone.a06 = null then
               canvas.buffer_int8 := ABM.IC.signed_char (zone.v06);
            else
               canvas.buffer_int8 := ABM.IC.signed_char (zone.a06.all);
            end if;
         when ft_byte2 =>
            struct.buffer_type := ABM.MYSQL_TYPE_SHORT;
            struct.buffer      := canvas.buffer_int16'Address;
            if zone.a07 = null then
               canvas.buffer_int16 := ABM.IC.short (zone.v07);
            else
               canvas.buffer_int16 := ABM.IC.short (zone.a07.all);
            end if;
         when ft_byte3 =>
            struct.buffer_type := ABM.MYSQL_TYPE_LONG;
            struct.buffer      := canvas.buffer_int32'Address;
            --  ABM.MYSQL_TYPE_INT24 not for input, use next biggest
            if zone.a08 = null then
               canvas.buffer_int32 := ABM.IC.int (zone.v08);
            else
               canvas.buffer_int32 := ABM.IC.int (zone.a08.all);
            end if;
         when ft_byte4 =>
            struct.buffer_type := ABM.MYSQL_TYPE_LONG;
            struct.buffer      := canvas.buffer_int32'Address;
            if zone.a09 = null then
               canvas.buffer_int32 := ABM.IC.int (zone.v09);
            else
               canvas.buffer_int32 := ABM.IC.int (zone.a09.all);
            end if;
         when ft_byte8 =>
            struct.buffer_type := ABM.MYSQL_TYPE_LONGLONG;
            struct.buffer      := canvas.buffer_int64'Address;
            if zone.a10 = null then
               canvas.buffer_int64 := ABM.IC.long (zone.v10);
            else
               canvas.buffer_int64 := ABM.IC.long (zone.a10.all);
            end if;
         when ft_real9 =>
            struct.buffer_type := ABM.MYSQL_TYPE_FLOAT;
            struct.buffer      := canvas.buffer_float'Address;
            if zone.a11 = null then
               canvas.buffer_float := ABM.IC.C_float (zone.v11);
            else
               canvas.buffer_float := ABM.IC.C_float (zone.a11.all);
            end if;
         when ft_real18 =>
            struct.buffer_type := ABM.MYSQL_TYPE_DOUBLE;
            struct.buffer      := canvas.buffer_double'Address;
            if zone.a12 = null then
               canvas.buffer_double := ABM.IC.double (zone.v12);
            else
               canvas.buffer_double := ABM.IC.double (zone.a12.all);
            end if;
         when ft_textual =>
            struct.buffer_type := ABM.MYSQL_TYPE_STRING;
            if zone.a13 = null then
               set_binary_buffer (ARC.convert (zone.v13));
            else
               set_binary_buffer (ARC.convert (zone.a13.all));
            end if;
         when ft_widetext =>
            struct.buffer_type := ABM.MYSQL_TYPE_STRING;
            if zone.a14 = null then
               set_binary_buffer (ARC.convert (zone.v14));
            else
               set_binary_buffer (ARC.convert (zone.a14.all));
            end if;
         when ft_supertext =>
            struct.buffer_type := ABM.MYSQL_TYPE_STRING;
            if zone.a15 = null then
               set_binary_buffer (ARC.convert (zone.v15));
            else
               set_binary_buffer (ARC.convert (zone.a15.all));
            end if;
         when ft_timestamp =>
            struct.buffer_type := ABM.MYSQL_TYPE_DATETIME;
            struct.buffer      := canvas.buffer_time'Address;
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
               canvas.buffer_time.minute := ABM.IC.unsigned (CFM.Minute (hack));
               canvas.buffer_time.second := ABM.IC.unsigned (CFM.Second (hack));
               canvas.buffer_time.second_part :=
                 ABM.IC.unsigned_long (CFM.Sub_Second (hack) * 1000000);
            end;
         when ft_chain =>
            struct.buffer_type := ABM.MYSQL_TYPE_BLOB;
            --  Chains are only available via access
            declare
               chainstr : String (1 .. zone.a17.all'Length);
            begin
               for x in chainstr'Range loop
                  chainstr (x) := Character'Val (zone.a17.all (x));
               end loop;
               set_binary_buffer (chainstr);
            end;
         when ft_enumtype =>
            --  ENUM is essentially a specific string on MySQL
            struct.buffer_type := ABM.MYSQL_TYPE_STRING;
            if zone.a18 = null then
               set_binary_buffer (ARC.convert (zone.v18));
            else
               set_binary_buffer (ARC.convert (zone.a18.all));
            end if;
         when ft_settype =>
            --  Set types are imploded strings on MySQL
            --  Only access is available here
            struct.buffer_type := ABM.MYSQL_TYPE_STRING;
            set_binary_buffer (ARC.convert (zone.a19));
      end case;
   end construct_bind_slot;


   -------------------
   --  log_problem  --
   -------------------
   procedure log_problem
     (statement  : MySQL_statement;
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


--     -------------------
--     --  auto_assign  --
--     -------------------
--     procedure auto_assign (Stmt  : out Base_Statement;
--                            index : Positive;
--                            value : String)
--     is
--     begin
--        --  this reads the column metadata, converts the value string to
--        --  native type and calls the correct assign
--
--        null;
--        case Stmt.column_info.Element (index).field_type is
--           when ft_nbyte0 => Stmt.assign (index, (value = "1"));
--           when ft_nbyte1 => Stmt.assign (index, AR.nbyte1 (convert (ST)));
--           when ft_nbyte2 => Stmt.assign (index, AR.nbyte2 (convert (ST)));
--           when ft_nbyte3 => Stmt.assign (index, AR.nbyte3 (convert (ST)));
--           when ft_nbyte4 => Stmt.assign (index, AR.nbyte4 (convert (ST)));
--           when ft_nbyte8 => Stmt.assign (index, AR.nbyte8 (convert (ST)));
--                    when ft_nbyte1 =>
--                       dvariant := (datatype => ft_nbyte1, v01 => convert (ST));
--                    when ft_nbyte2 =>
--                       dvariant := (datatype => ft_nbyte2, v02 => convert (ST));
--                    when ft_nbyte3 =>
--                       dvariant := (datatype => ft_nbyte3, v03 => convert (ST));
--                    when ft_nbyte4 =>
--                       dvariant := (datatype => ft_nbyte4, v04 => convert (ST));
--                    when ft_nbyte8 =>
--                       dvariant := (datatype => ft_nbyte8, v05 => convert (ST));
--                    when ft_byte1  =>
--                       dvariant := (datatype => ft_byte1, v06 => convert (ST));
--                    when ft_byte2  =>
--                       dvariant := (datatype => ft_byte2, v07 => convert (ST));
--                    when ft_byte3  =>
--                       dvariant := (datatype => ft_byte3, v08 => convert (ST));
--                    when ft_byte4  =>
--                       dvariant := (datatype => ft_byte4, v09 => convert (ST));
--                    when ft_byte8  =>
--                       dvariant := (datatype => ft_byte8, v10 => convert (ST));
--                    when ft_real9  =>
--                       dvariant := (datatype => ft_real9, v11 => convert (ST));
--                    when ft_real18 =>
--                       dvariant := (datatype => ft_real18, v12 => convert (ST));
--                    when ft_textual =>
--                       dvariant := (datatype => ft_textual,
--                                    v13 => convert (ST, Stmt.con_max_blob));
--                    when ft_widetext =>
--                       dvariant := (datatype => ft_widetext,
--                                    v14 => convert (ST, Stmt.con_max_blob));
--                    when ft_supertext =>
--                       dvariant := (datatype => ft_supertext,
--                                    v15 => convert (ST, Stmt.con_max_blob));
--                    when ft_timestamp =>
--                       begin
--                          dvariant := (datatype => ft_timestamp,
--                                       v16 => convert (ST));
--                       exception
--                          when CAL.Time_Error =>
--                             dvariant := (datatype => ft_textual, v13 =>
--                                             convert (ST, Stmt.con_max_blob));
--                       end;
--                    when ft_enumtype =>
--                       --  It seems that mysql doesn't give up the enum index
--                       --  easily.  Set to "0" for all members
--                       dvariant := (datatype => ft_enumtype,
--                                    V18 => (enumeration =>
--                                               convert (ST, Stmt.con_max_blob),
--                                              index => 0));
--                    when others =>
--                       null;
--
--                 end case;
--                 case Stmt.column_info.Element (Index => F).field_type is
--                    when ft_chain =>
--                       field := ARF.spawn_field
--                         (binob => convert (ST, Stmt.con_max_blob));
--                    when ft_settype =>
--                       field := ARF.spawn_field (enumset => convert (ST));
--                    when others =>
--                       field := ARF.spawn_field (data => dvariant,
--                                                 null_data => EN);
--                 end case;
--   end auto_assign;

end AdaBase.Statement.Base.MySQL;
