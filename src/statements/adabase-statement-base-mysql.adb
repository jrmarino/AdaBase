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
         if Stmt.cheat.result_handle /= null then
            Stmt.rows_leftover := True;
            Stmt.mysql_conn.all.free_result (Stmt.cheat.result_handle);
         end if;
      when prepared_statement =>
         if Stmt.cheat.stmt_handle /= null then
            Stmt.rows_leftover := True;
            Stmt.mysql_conn.all.prep_free_result (Stmt.cheat.stmt_handle);
         end if;
      end case;
      Stmt.clear_column_information;
      Stmt.cheat.delivery := completed;
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
            return Stmt.mysql_conn.all.driverMessage;
         when prepared_statement =>
            return Stmt.mysql_conn.all.prep_DriverMessage
              (Stmt.cheat.stmt_handle);
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
            return Stmt.mysql_conn.all.lastInsertID;
         when prepared_statement =>
            return Stmt.mysql_conn.all.prep_LastInsertID
              (Stmt.cheat.stmt_handle);
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
            return Stmt.mysql_conn.all.SqlState;
         when prepared_statement =>
            return Stmt.mysql_conn.all.prep_SqlState
              (Stmt.cheat.stmt_handle);
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
            return Stmt.mysql_conn.all.driverCode;
         when prepared_statement =>
            return Stmt.mysql_conn.all.prep_DriverCode
              (Stmt.cheat.stmt_handle);
      end case;
   end last_driver_code;


   ----------------------------
   --  execute  (version 1)  --
   ----------------------------
   overriding
   function execute (Stmt : MySQL_statement) return Boolean is
   begin
      if Stmt.type_of_statement = direct_statement then
         raise INVALID_FOR_DIRECT_QUERY
           with "execute is for prepared statements";
      end if;
      return False;
   end execute;


   ----------------------------
   --  execute  (version 2)  --
   ----------------------------
   overriding
   function execute (Stmt : MySQL_statement; bind_piped : String)
                     return Boolean
   is
   begin
      if Stmt.type_of_statement = direct_statement then
         raise INVALID_FOR_DIRECT_QUERY
           with "execute is for prepared statements";
      end if;
      return False;
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
            Object.transform_sql (sql => CT.USS (Object.initial_sql.all),
                                  new_sql => Object.sql_final.all);
            Object.mysql_conn.initialize_and_prepare_statement
              (stmt => Object.cheat.stmt_handle, sql => Object.sql_final.all);
            declare
                 params : Natural := Object.mysql_conn.prep_markers_found
                   (stmt => Object.cheat.stmt_handle);
            begin
               if params /= Natural (Object.alpha_markers.Length) then
                  raise ILLEGAL_BIND_SQL
                    with "marker mismatch," & Object.alpha_markers.Length'Img
                      & " expected but " & params'Img & " is the real number";
               end if;
            end;
      end case;
   end initialize;


   ---------------------
   --  direct_result  --
   ---------------------
   procedure process_direct_result (Stmt : in out MySQL_statement)
   is
      use type ABM.MYSQL_RES_Access;
   begin
      case Stmt.con_buffered is
         when True => Stmt.mysql_conn.all.store_result
              (result_handle => Stmt.cheat.result_handle);
         when False => Stmt.mysql_conn.all.use_result
              (result_handle => Stmt.cheat.result_handle);
      end case;
      Stmt.result_present := (Stmt.cheat.result_handle /= null);
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
   procedure clear_column_information (Stmt : in out MySQL_statement) is
   begin
      Stmt.column_info.Clear;
      Stmt.crate.Clear;
      Stmt.headings_map.Clear;
   end clear_column_information;


   -------------------------------
   --  scan_column_information  --
   -------------------------------
   procedure scan_column_information (Stmt : in out MySQL_statement)
   is
      use type ABM.MYSQL_FIELD_Access;
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
      loop
         field := Stmt.mysql_conn.fetch_field
           (result_handle => Stmt.cheat.result_handle);
         exit when field = null;
         declare
            info : column_info;
            brec : bindrec;
         begin
            info.field_name  := fn (Stmt.mysql_conn.field_name_field (field));
            info.table       := fn (Stmt.mysql_conn.field_name_table (field));
            info.mysql_type  := field.field_type;
            info.null_possible := Stmt.mysql_conn.field_allows_null (field);
            Stmt.mysql_conn.field_data_type
              (field    => field,
               std_type => info.field_type,
               size     => info.field_size);
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
   function fetch_next (Stmt : MySQL_statement) return ARS.DataRow_Access
   is
      datarow : ARS.DataRow_Access := null;
   begin
      if Stmt.cheat.delivery = completed then
         return datarow;
      end if;
      case Stmt.type_of_statement is
         when prepared_statement =>
            raise INVALID_FOR_RESULT_SET with "not yet implemented";
            return datarow;
         when direct_statement =>
            return Stmt.internal_fetch_row_direct;
      end case;
   end fetch_next;


   -------------------
   --  fetch_bound  --
   -------------------
   overriding
   function fetch_bound (Stmt : MySQL_statement) return Boolean is
   begin
      if Stmt.cheat.delivery = completed then
         return False;
      end if;
      case Stmt.type_of_statement is
         when prepared_statement =>
            raise INVALID_FOR_RESULT_SET with "not yet implemented";
            return False;
         when direct_statement =>
            return Stmt.internal_fetch_bound_direct;
      end case;
   end fetch_bound;


   -----------------
   --  fetch_all  --
   -----------------
   overriding
   function fetch_all (Stmt : MySQL_statement) return ARS.DataRowSet
   is
      maxrows : Natural := Natural (Stmt.rows_returned);
      tmpset  : ARS.DataRowSet (1 .. maxrows + 1) := (others => null);
      nullset : ARS.DataRowSet (1 .. 0);
      index   : Natural := 1;
   begin
      if (Stmt.cheat.delivery = completed) or else (maxrows = 0) then
         return nullset;
      end if;
      --  It is possible that one or more rows was individually fetched
      --  before the entire set was fetched.  Let's consider this legal so
      --  use a repeat loop to check each row and return a partial set
      --  if necessary.
      loop
         tmpset (index) := Stmt.fetch_next;
         exit when ARS.complete (tmpset (index));
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
   procedure fetch_next_set (Stmt    : out MySQL_statement;
                             success : out Boolean)
   is
     another_set : Boolean;
   begin
      success := False;
      another_set := Stmt.mysql_conn.fetch_next_set;
      if not another_set then
         return;
      end if;
      Stmt.internal_direct_post_exec (newset => True);
      success := True;
   end fetch_next_set;


   ---------------------------
   --  convert to Ada Time  --
   ---------------------------
   function convert (nv : String) return CAL.Time
   is
      len    : constant Natural  := nv'Length;
      year   : CAL.Year_Number   := CAL.Year_Number'First;
      month  : CAL.Month_Number  := CAL.Month_Number'First;
      day    : CAL.Day_Number    := CAL.Day_Number'First;
      hour   : CFM.Hour_Number   := CFM.Hour_Number'First;
      minute : CFM.Minute_Number := CFM.Minute_Number'First;
      second : CFM.Second_Number := CFM.Second_Number'First;
      cursor : Positive;
   begin
      case len is
         when 8 | 14 => cursor := 5;
         when others => cursor := 3;
      end case;
      year := Integer'Value (nv (nv'First .. cursor - 1));
      if len > 2 then
         month := Integer'Value (nv (cursor .. cursor + 1));
         cursor := cursor + 2;
         if len > 4 then
            day := Integer'Value (nv (cursor .. cursor + 1));
            cursor := cursor + 2;
            if len > 6 then
               hour := Integer'Value (nv (cursor .. cursor + 1));
               cursor := cursor + 2;
               if len > 8 then
                  minute := Integer'Value (nv (cursor .. cursor + 1));
                  cursor := cursor + 2;
                  if len > 10 then
                     second := Integer'Value (nv (cursor .. cursor + 1));
                  end if;
               end if;
            end if;
         end if;
      end if;
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


   ---------------------------------
   --  internal_fetch_row_direct  --
   ---------------------------------
   function internal_fetch_row_direct (Stmt : MySQL_statement)
                                       return ARS.DataRow_Access
   is
      use type ABM.ICS.chars_ptr;
      use type ABM.MYSQL_ROW_access;
      rptr : ABM.MYSQL_ROW_access :=
        Stmt.mysql_conn.fetch_row (Stmt.cheat.result_handle);
   begin
      if rptr = null then
         Stmt.cheat.delivery := completed;
         Stmt.mysql_conn.free_result (Stmt.cheat.result_handle);
         --  Stmt.clear_column_information;  (won't work inside func in Ada05)
         return null;
      end if;
      Stmt.cheat.delivery := progressing;

      declare
         maxlen : constant Natural := Natural (Stmt.column_info.Length);
         type rowtype is array (1 .. maxlen) of ABM.ICS.chars_ptr;
         type rowtype_access is access all rowtype;

         row    : rowtype_access;
         result : ARS.DataRow_Access := new ARS.DataRow;

         field_lengths : constant ACM.fldlen := Stmt.mysql_conn.fetch_lengths
           (result_handle => Stmt.cheat.result_handle,
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

   end internal_fetch_row_direct;


   -----------------------------------
   --  internal_fetch_bound_direct  --
   -----------------------------------
   function internal_fetch_bound_direct (Stmt : MySQL_statement)
                                         return Boolean
   is
      use type ABM.ICS.chars_ptr;
      use type ABM.MYSQL_ROW_access;
      rptr : ABM.MYSQL_ROW_access :=
        Stmt.mysql_conn.fetch_row (Stmt.cheat.result_handle);
   begin
      if rptr = null then
         Stmt.cheat.delivery := completed;
         Stmt.mysql_conn.free_result (Stmt.cheat.result_handle);
         --  Stmt.clear_column_information;  (won't work inside func in Ada05)
         return False;
      end if;
      Stmt.cheat.delivery := progressing;

      declare
         maxlen : constant Natural := Natural (Stmt.column_info.Length);
         type rowtype is array (1 .. maxlen) of ABM.ICS.chars_ptr;
         type rowtype_access is access all rowtype;

         row : rowtype_access;
         field_lengths : constant ACM.fldlen := Stmt.mysql_conn.fetch_lengths
           (result_handle => Stmt.cheat.result_handle,
            num_columns   => maxlen);

         function Convert is new Ada.Unchecked_Conversion
           (Source => ABM.MYSQL_ROW_access, Target => rowtype_access);
      begin
         row := Convert (rptr);
         for F in 1 .. maxlen loop
            if Stmt.crate.Element (Index => F).bound then
               declare
                  last_one : constant Boolean := (F = maxlen);
                  heading  : constant String := CT.USS
                    (Stmt.column_info.Element (Index => F).field_name);
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
   end internal_fetch_bound_direct;


   ----------------------------------
   --  internal_direct_post_exec   --
   ----------------------------------
   procedure internal_direct_post_exec (Stmt : in out MySQL_statement;
                                        newset : Boolean := False) is
   begin
      Stmt.num_columns := 0;
      Stmt.successful_execution := False;
      if not newset then
         Stmt.connection.execute (sql => Stmt.sql_final.all);
         Stmt.log_nominal (category => statement_execution,
                           message => Stmt.sql_final.all);
      end if;
      Stmt.process_direct_result;
      Stmt.successful_execution := True;
      if Stmt.result_present then
         Stmt.num_columns := Stmt.mysql_conn.fields_in_result
                               (Stmt.cheat.result_handle);
         if Stmt.con_buffered then
            Stmt.size_of_rowset := Stmt.mysql_conn.rows_in_result
                                     (Stmt.cheat.result_handle);
         end if;
         Stmt.scan_column_information;
         Stmt.cheat.delivery := pending;
      else
         declare
            returned_cols : Natural;
         begin
            returned_cols := Stmt.mysql_conn.all.field_count;
            if returned_cols = 0 then
               Stmt.impacted := Stmt.mysql_conn.rows_affected_by_execution;
            else
               raise ACM.RESULT_FAIL with "Columns returned without result";
            end if;
         end;
         Stmt.cheat.delivery := completed;
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

end AdaBase.Statement.Base.MySQL;
