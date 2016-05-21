--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body AdaBase.Statement.Base is

   ------------------
   --  successful  --
   ------------------
   overriding
   function successful  (Stmt : Base_Statement) return Boolean
   is
   begin
      return Stmt.successful_execution;
   end successful;


   ----------------------
   --  data_discarded  --
   ----------------------
   overriding
   function data_discarded  (Stmt : Base_Statement) return Boolean
   is
   begin
      return Stmt.rows_leftover;
   end data_discarded;


   ---------------------
   --  rows_affected  --
   ---------------------
   overriding
   function rows_affected (Stmt : Base_Statement) return Affected_Rows
   is
   begin
      if not Stmt.successful_execution then
         raise PRIOR_EXECUTION_FAILED
           with "Has query been executed yet?";
      end if;
      if Stmt.result_present then
         raise INVALID_FOR_RESULT_SET
           with "Result set found; use rows_returned";
      else
         return Stmt.impacted;
      end if;
   end rows_affected;


   ---------------------
   --  transform_sql  --
   ---------------------
   function transform_sql (Stmt : out Base_Statement; sql : String)
                           return String
   is
      procedure reserve_marker;

      sql_mask : String := CT.redact_quotes (sql);

      procedure reserve_marker
      is
         brec   : bindrec;
      begin
         brec.v00 := False;
         Stmt.realmccoy.Append (New_Item => brec);
      end reserve_marker;

   begin
      Stmt.alpha_markers.Clear;
      Stmt.realmccoy.Clear;

      if CT.IsBlank (sql) then
         return "";
      end if;

      declare
         --  This block does two things:
         --  1) finds "?" and increments the replacement index
         --  2) finds ":[A-Za-z0-9_]*", replaces with "?", increments the
         --     replacement index, and pushes the string into alpha markers
         --  Normally ? and : aren't mixed but we will support it.
         procedure replace_alias;
         procedure lock_and_advance (symbol : Character);

         start    : Natural  := 0;
         final    : Natural  := 0;
         arrow    : Positive := 1;
         polaris  : Natural  := 0;
         scanning : Boolean  := False;
         product  : String (1 .. sql'Length) := (others => ' ');

         adjacent_error : constant String :=
                          "Bindings are not separated; they are touching: ";

         procedure lock_and_advance (symbol : Character) is
         begin
            polaris := polaris + 1;
            product (polaris) := symbol;
         end lock_and_advance;

         procedure replace_alias is
            len    : Natural := final - start;
            alias  : String (1 .. len) := sql_mask (start + 1 .. final);
         begin
            if Stmt.alpha_markers.Contains (Key => alias) then
               raise ILLEGAL_BIND_SQL with "multiple instances of " & alias;
            end if;
            reserve_marker;
            Stmt.alpha_markers.Insert (alias, Stmt.realmccoy.Last_Index);
            scanning := False;
         end replace_alias;

      begin
         loop
            case sql_mask (arrow) is
               when ASCII.Query =>
                  if scanning then
                     raise ILLEGAL_BIND_SQL
                       with adjacent_error & sql_mask (start .. arrow);
                  end if;
                  reserve_marker;
                  lock_and_advance (ASCII.Query);
               when ASCII.Colon =>
                  if scanning then
                     raise ILLEGAL_BIND_SQL
                       with adjacent_error & sql_mask (start .. arrow);
                  end if;
                  scanning := True;
                  start := arrow;
               when others =>
                  if scanning then
                     case sql_mask (arrow) is
                        when 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' =>
                           final := arrow;
                        when others =>
                           replace_alias;
                           lock_and_advance (ASCII.Query);
                           lock_and_advance (sql (arrow));
                     end case;
                  else
                     lock_and_advance (sql (arrow));
                  end if;
            end case;
            if scanning and then arrow = sql_mask'Length then
               replace_alias;
               lock_and_advance (ASCII.Query);
            end if;
            exit when arrow = sql_mask'Length;
            arrow := arrow + 1;
         end loop;
         return product (1 .. polaris);
      end;
   end transform_sql;


   ----------------------------------
   --  convert string to textwide  --
   ----------------------------------
   function convert (nv : String) return AR.textwide is
   begin
      return SUW.To_Unbounded_Wide_String (ACC.To_Wide_String (nv));
   end convert;


   -----------------------------------
   --  convert string to textsuper  --
   -----------------------------------
   function convert (nv : String) return AR.textsuper is
   begin
      return SWW.To_Unbounded_Wide_Wide_String (ACC.To_Wide_Wide_String (nv));
   end convert;


   --------------------
   --  Same_Strings  --
   --------------------
   function Same_Strings (S, T : String) return Boolean is
   begin
      return S = T;
   end Same_Strings;


   -------------------
   --  log_nominal  --
   -------------------
   procedure log_nominal (statement : Base_Statement;
                          category  : Log_Category;
                          message   : String)
   is
   begin
      logger_access.all.log_nominal
        (driver   => statement.dialect,
         category => category,
         message  => CT.SUS (message));
   end log_nominal;


   --------------------
   --  bind_proceed  --
   --------------------
   function bind_proceed (Stmt : Base_Statement; index : Positive)
                          return Boolean is
   begin
      if not Stmt.successful_execution then
         raise PRIOR_EXECUTION_FAILED
           with "Use bind after 'execute' but before 'fetch_next'";
      end if;
      if index > Stmt.crate.Last_Index then
         raise BINDING_COLUMN_NOT_FOUND
           with "Index" & index'Img & " is too high; only" &
           Stmt.crate.Last_Index'Img & " columns exist.";
      end if;
      return True;
   end bind_proceed;


   ------------------
   --  bind_index  --
   ------------------
   function bind_index (Stmt : Base_Statement; heading : String)
                        return Positive
   is
      use type Markers.Cursor;
      cursor : Markers.Cursor;
   begin
      cursor := Stmt.headings_map.Find (Key => heading);
      if cursor = Markers.No_Element then
         raise BINDING_COLUMN_NOT_FOUND with
           "There is no column named '" & heading & "'.";
      end if;
      return Markers.Element (Position => cursor);
   end bind_index;


   ---------------------------------
   --  check_bound_column_access  --
   ---------------------------------
   procedure check_bound_column_access (absent : Boolean) is
   begin
      if absent then
         raise ILLEGAL_BIND_SQL with
           "Binding column with null access is illegal";
      end if;
   end check_bound_column_access;


   ------------------------------------------------------
   --  20 bind functions (impossible to make generic)  --
   ------------------------------------------------------
   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.nbyte0_access)
   is
      use type AR.nbyte0_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_nbyte0, a00 => vaxx, v00 => False,
                    bound => True, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.nbyte1_access)
   is
      use type AR.nbyte1_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_nbyte1, a01 => vaxx, v01 => 0,
                    bound => True, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.nbyte2_access)
   is
      use type AR.nbyte2_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_nbyte2, a02 => vaxx, v02 => 0,
                    bound => True, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.nbyte3_access)
   is
      use type AR.nbyte3_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_nbyte3, a03 => vaxx, v03 => 0,
                    bound => True, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.nbyte4_access)
   is
      use type AR.nbyte4_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_nbyte4, a04 => vaxx, v04 => 0,
                    bound => True, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.nbyte8_access)
   is
      use type AR.nbyte8_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_nbyte8, a05 => vaxx, v05 => 0,
                    bound => True, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.byte1_access)
   is
      use type AR.byte1_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_byte1, a06 => vaxx, v06 => 0,
                    bound => True, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.byte2_access)
   is
      use type AR.byte2_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_byte2, a07 => vaxx, v07 => 0,
                    bound => True, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.byte3_access)
   is
      use type AR.byte3_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_byte3, a08 => vaxx, v08 => 0,
                    bound => True, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.byte4_access)
   is
      use type AR.byte4_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_byte4, a09 => vaxx, v09 => 0,
                    bound => True, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.byte8_access)
   is
      use type AR.byte8_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_byte8, a10 => vaxx, v10 => 0,
                    bound => True, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.real9_access)
   is
      use type AR.real9_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_real9, a11 => vaxx, v11 => 0.0,
                    bound => True, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.real18_access)
   is
      use type AR.real18_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_real18, a12 => vaxx, v12 => 0.0,
                    bound => True, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.str1_access)
   is
      use type AR.str1_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_textual, a13 => vaxx, v13 => CT.blank,
                    bound => True, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.str2_access)
   is
      use type AR.str2_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_widetext, a14 => vaxx, bound => True,
                    v14 => AR.blank_wstring, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.str4_access)
   is
      use type AR.str4_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_supertext, a15 => vaxx, bound => True,
                    v15 => AR.blank_wwstring, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.time_access)
   is
      use type AR.time_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_timestamp, a16 => vaxx,
                    v16 => CAL.Clock, bound => True, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.chain_access)
   is
      use type AR.chain_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_chain, a17 => vaxx,
                    v17 => CT.blank, bound => True, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.enum_access)
   is
      use type AR.enum_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_enumtype, a18 => vaxx, bound => True,
                    v18 => AR.PARAM_IS_ENUM, null_data => False));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.settype_access)
   is
      use type AR.settype_access;
      absent : Boolean := (vaxx = null);
   begin
      check_bound_column_access (absent);
      if Stmt.bind_proceed (index => index) then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_settype, a19 => vaxx,
                    v19 => CT.blank, bound => True, null_data => False));
      end if;
   end bind;


   ------------------------------------------------------------------
   --  bind via headings  (believe me, generics are not possible)  --
   ------------------------------------------------------------------
   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.nbyte0_access) is
   begin
      Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.nbyte1_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.nbyte2_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.nbyte3_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.nbyte4_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.nbyte8_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.byte1_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.byte2_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.byte3_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.byte4_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.byte8_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.real9_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.real18_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.str1_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.str2_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.str4_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.time_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.chain_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.enum_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.settype_access) is
   begin
        Stmt.bind (vaxx => vaxx, index => Stmt.bind_index (heading));
   end bind;


   --------------------
   --  assign_index  --
   --------------------
   function assign_index (Stmt : Base_Statement; moniker : String)
                          return Positive
   is
      use type Markers.Cursor;
      cursor : Markers.Cursor;
   begin
      cursor := Stmt.alpha_markers.Find (Key => moniker);
      if cursor = Markers.No_Element then
         raise MARKER_NOT_FOUND with
           "There is no marker known as '" & moniker & "'.";
      end if;
      return Markers.Element (Position => cursor);
   end assign_index;


   ------------------------------------------------------------------
   --  assign via moniker (Access, 20)                                        --
   ------------------------------------------------------------------
   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.nbyte0_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.nbyte1_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.nbyte2_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.nbyte3_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.nbyte4_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.nbyte8_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.byte1_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.byte2_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.byte3_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.byte4_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.byte8_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.real9_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.real18_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.str1_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.str2_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.str4_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.time_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.chain_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.enum_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.settype_access) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;


   ------------------------------------------------------------------
   --  assign via moniker (Value, 20)                                        --
   ------------------------------------------------------------------
   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.nbyte0) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.nbyte1) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.nbyte2) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.nbyte3) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.nbyte4) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.nbyte8) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.byte1) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.byte2) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.byte3) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.byte4) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.byte8) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.real9) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.real18) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : String) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.textual) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.textwide) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.textsuper) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : CAL.Time) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.chain) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.enumtype) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.settype) is
   begin
      Stmt.assign (vaxx => vaxx, index => Stmt.assign_index (moniker));
   end assign;

   ------------------------------------------------------
   --  20 + 20 = 40 assign functions                   --
   ------------------------------------------------------
   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.nbyte0_access)
   is
      use type AR.nbyte0_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_nbyte0, a00 => vaxx, v00 => False,
                 bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.nbyte0) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_nbyte0, a00 => null, v00 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.nbyte1_access)
   is
      use type AR.nbyte1_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_nbyte1, a01 => vaxx, v01 => 0,
                 bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.nbyte1) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_nbyte1, a01 => null, v01 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.nbyte2_access)
   is
      use type AR.nbyte2_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_nbyte2, a02 => vaxx, v02 => 0,
                 bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.nbyte2) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_nbyte2, a02 => null, v02 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.nbyte3_access)
   is
      use type AR.nbyte3_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_nbyte3, a03 => vaxx, v03 => 0,
                 bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.nbyte3) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_nbyte3, a03 => null, v03 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.nbyte4_access)
   is
      use type AR.nbyte4_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_nbyte4, a04 => vaxx, v04 => 0,
                 bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.nbyte4) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_nbyte4, a04 => null, v04 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.nbyte8_access)
   is
      use type AR.nbyte8_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_nbyte8, a05 => vaxx, v05 => 0,
                 bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.nbyte8) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_nbyte8, a05 => null, v05 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.byte1_access)
   is
      use type AR.byte1_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_byte1, a06 => vaxx, v06 => 0,
                 bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.byte1) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_byte1, a06 => null, v06 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.byte2_access)
   is
      use type AR.byte2_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_byte2, a07 => vaxx, v07 => 0,
                 bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.byte2) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_byte2, a07 => null, v07 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.byte3_access)
   is
      use type AR.byte3_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_byte3, a08 => vaxx, v08 => 0,
                 bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.byte3) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_byte3, a08 => null, v08 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.byte4_access)
   is
      use type AR.byte4_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_byte4, a09 => vaxx,  v09 => 0,
                 bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.byte4) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_byte4, a09 => null,  v09 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.byte8_access)
   is
      use type AR.byte8_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_byte8, a10 => vaxx,  v10 => 0,
                 bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.byte8) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_byte8, a10 => null,  v10 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.real9_access)
   is
      use type AR.real9_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_real9, a11 => vaxx, v11 => 0.0,
                 bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.real9) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_real9, a11 => null, v11 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.real18_access)
   is
      use type AR.real18_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_real18, a12 => vaxx, v12 => 0.0,
                 bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.real18) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_real18, a12 => null, v12 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.str1_access)
   is
      use type AR.str1_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_textual, a13 => vaxx, v13 => CT.blank,
                 bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : String) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_textual, a13 => null, v13 => CT.SUS (vaxx),
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.textual) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_textual, a13 => null, v13 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.str2_access)
   is
      use type AR.str2_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_widetext, a14 => vaxx,
                 v14 => AR.blank_wstring, bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.textwide) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_widetext, a14 => null, v14 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.str4_access)
   is
      use type AR.str4_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_supertext, a15 => vaxx, bound => True,
                 v15 => AR.blank_wwstring, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.textsuper) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_supertext, a15 => null, v15 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.time_access)
   is
      use type AR.time_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_timestamp, a16 => vaxx,
                 v16 => CAL.Clock, bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : CAL.Time) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_timestamp, a16 => null, v16 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.chain_access)
   is
      use type AR.chain_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_chain, a17 => vaxx,
                 v17 => CT.blank, bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.chain)
   is
      payload : constant String := ARC.convert (vaxx);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_chain, a17 => null,
                 v17 => CT.SUS (payload), bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.enum_access)
   is
      use type AR.enum_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_enumtype, a18 => vaxx,
                 v18 => AR.PARAM_IS_ENUM, bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.enumtype) is
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_enumtype, a18 => null, v18 => vaxx,
                 bound => True, null_data => False));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.settype_access)
   is
      use type AR.settype_access;
      absent : Boolean := (vaxx = null);
   begin
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_settype, a19 => vaxx,
                 v19 => CT.blank, bound => True, null_data => absent));
   end assign;

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.settype)
   is
      payload : AR.textual := CT.blank;
   begin
      for x in vaxx'Range loop
         if x /= vaxx'First then
            CT.SU.Append (payload, ",");
         end if;
         CT.SU.Append (payload, vaxx (x).enumeration);
      end loop;
      Stmt.realmccoy.Replace_Element
        (index, (output_type => ft_settype, a19 => null,
                 v19 => payload, bound => True, null_data => False));
   end assign;


   ------------------
   --  iterate #1  --
   ------------------
   overriding
   procedure iterate (Stmt    : out Base_Statement;
                      process : not null access procedure) is
   begin
      loop
         exit when not fetch_bound (Stmt => Base_Statement'Class (Stmt));
         process.all;
      end loop;
   end iterate;


   ------------------
   --  iterate #2  --
   ------------------
   overriding
   procedure iterate (Stmt    : out Base_Statement;
                      process : not null access procedure (row : ARS.DataRow))
   is
   begin
      loop
         declare
            local_row : ARS.DataRow :=
                        fetch_next (Stmt => Base_Statement'Class (Stmt));
         begin
            exit when local_row.data_exhausted;
            process.all (row => local_row);
         end;
      end loop;
   end iterate;


   -------------------
   --  auto_assign  --
   -------------------
   procedure auto_assign (Stmt  : out Base_Statement; index : Positive;
                          value : String)
   is
      zone : bindrec renames Stmt.realmccoy.Element (index);
      ST   : AR.textual;
      STW  : AR.textwide;
      STS  : AR.textsuper;
      hold : ARF.variant;
   begin
      case zone.output_type is
         when ft_widetext =>
            ST  := CT.SUS (value);
            STW := SUW.To_Unbounded_Wide_String (ARC.convert (ST));
         when ft_supertext =>
            ST  := CT.SUS (value);
            STS := SWW.To_Unbounded_Wide_Wide_String (ARC.convert (ST));
         when ft_timestamp | ft_settype | ft_chain =>
            null;
         when others =>
            ST := CT.SUS (value);
      end case;
      case zone.output_type is
         when ft_nbyte0    => hold := (ft_nbyte0, ARC.convert (ST));
         when ft_nbyte1    => hold := (ft_nbyte1, ARC.convert (ST));
         when ft_nbyte2    => hold := (ft_nbyte2, ARC.convert (ST));
         when ft_nbyte3    => hold := (ft_nbyte3, ARC.convert (ST));
         when ft_nbyte4    => hold := (ft_nbyte4, ARC.convert (ST));
         when ft_nbyte8    => hold := (ft_nbyte8, ARC.convert (ST));
         when ft_byte1     => hold := (ft_byte1, ARC.convert (ST));
         when ft_byte2     => hold := (ft_byte2, ARC.convert (ST));
         when ft_byte3     => hold := (ft_byte3, ARC.convert (ST));
         when ft_byte4     => hold := (ft_byte4, ARC.convert (ST));
         when ft_byte8     => hold := (ft_byte8, ARC.convert (ST));
         when ft_real9     => hold := (ft_real9, ARC.convert (ST));
         when ft_real18    => hold := (ft_real18, ARC.convert (ST));
         when ft_textual   => hold := (ft_textual, ST);
         when ft_widetext  => hold := (ft_widetext, STW);
         when ft_supertext => hold := (ft_supertext, STS);
         when ft_timestamp => hold := (ft_timestamp, (ARC.convert (value)));
         when ft_chain     => null;
         when ft_enumtype  => hold := (ft_enumtype, (ARC.convert (ST)));
         when ft_settype   => null;
      end case;
      case zone.output_type is
         when ft_nbyte0    => Stmt.assign (index, hold.v00);
         when ft_nbyte1    => Stmt.assign (index, hold.v01);
         when ft_nbyte2    => Stmt.assign (index, hold.v02);
         when ft_nbyte3    => Stmt.assign (index, hold.v03);
         when ft_nbyte4    => Stmt.assign (index, hold.v04);
         when ft_nbyte8    => Stmt.assign (index, hold.v05);
         when ft_byte1     => Stmt.assign (index, hold.v06);
         when ft_byte2     => Stmt.assign (index, hold.v07);
         when ft_byte3     => Stmt.assign (index, hold.v08);
         when ft_byte4     => Stmt.assign (index, hold.v09);
         when ft_byte8     => Stmt.assign (index, hold.v10);
         when ft_real9     => Stmt.assign (index, hold.v11);
         when ft_real18    => Stmt.assign (index, hold.v12);
         when ft_textual   => Stmt.assign (index, hold.v13);
         when ft_widetext  => Stmt.assign (index, hold.v14);
         when ft_supertext => Stmt.assign (index, hold.v15);
         when ft_timestamp => Stmt.assign (index, hold.v16);
         when ft_enumtype  => Stmt.assign (index, hold.v18);
         when ft_chain     =>
            declare
               my_chain : AR.chain := ARC.convert (value);
            begin
               Stmt.assign (index, my_chain);
            end;
         when ft_settype   =>
            declare
               set : AR.settype := ARC.convert (value);
            begin
               Stmt.assign (index, set);
            end;
      end case;
   end auto_assign;


end AdaBase.Statement.Base;
