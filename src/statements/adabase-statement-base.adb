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


   ---------------------
   --  rows_affected  --
   ---------------------
   overriding
   function rows_affected (Stmt : Base_Statement) return AffectedRows
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
   procedure transform_sql (Stmt : out Base_Statement; sql : String;
                            new_sql : out String)
   is
      sql_mask : String := sql;
   begin
      new_sql := sql;
      Stmt.alpha_markers.Clear;
      if sql'Length = 0 then
         return;
      end if;
      declare
         --  This block will mask anything between quotes (single or double)
         --  These are considered to be literal and not suitable for binding
         type seeking is (none, single, double);
         seek_status : seeking := none;
         arrow : Positive := 1;
      begin
         loop
            case sql (arrow) is
               when ''' =>
                  case seek_status is
                     when none =>
                        seek_status := single;
                        sql_mask (arrow) := '#';
                     when single =>
                        seek_status := none;
                        sql_mask (arrow) := '#';
                     when double => null;
                  end case;
               when ASCII.Quotation =>
                  case seek_status is
                     when none =>
                        seek_status := double;
                        sql_mask (arrow) := '#';
                     when double =>
                        seek_status := none;
                        sql_mask (arrow) := '#';
                     when single => null;
                  end case;
               when others => null;
            end case;
            exit when arrow = sql'Length;
            arrow := arrow + 1;
         end loop;
      end;
      declare
         --  This block does two things:
         --  1) finds "?" and increments the replacement index
         --  2) finds ":[A-Za-z0-9]*", replaces with "?", increments the
         --     replacement index, and pushes the string into alpha markers
         --  Normally ? and : aren't mixed but we will support it.
         procedure replace_alias;
         index    : Natural  := 0;
         start    : Natural  := 0;
         arrow    : Positive := 1;
         scanning : Boolean  := False;

         procedure replace_alias is
            len    : Natural := arrow - start;
            alias  : String (1 .. len) := sql_mask (start .. arrow - 1);
            scab   : String (1 .. len) := ('?', others => ' ');
         begin
            if Stmt.alpha_markers.Contains (Key => alias) then
               raise ILLEGAL_BIND_SQL with "multiple instances of " & alias;
            end if;
            index := index + 1;
            Stmt.alpha_markers.Insert (Key => alias, New_Item => index);
            new_sql (start .. arrow - 1) := scab;
            scanning := False;
         end replace_alias;

      begin
         loop
            case sql_mask (arrow) is
               when ASCII.Query =>
                  if scanning then
                     replace_alias;
                  end if;
                  index := index + 1;
               when ASCII.Colon =>
                  if scanning then
                     raise ILLEGAL_BIND_SQL with
                       "Bindings are not separated; they are touching";
                  end if;
                  scanning := True;
                  start := arrow;
               when others =>
                  if scanning then
                     case sql_mask (arrow) is
                        when 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' => null;
                        when others => replace_alias;
                     end case;
                  end if;
            end case;
            if scanning and then arrow = sql_mask'Length then
               replace_alias;
            end if;
            exit when arrow = sql_mask'Length;
            arrow := arrow + 1;
         end loop;
      end;
   end transform_sql;


   -------------------------------
   --  convert string to chain  --
   -------------------------------
   function convert (nv : String; maxsize : BLOB_maximum) return AR.chain
   is
      maxlinks : Natural := nv'Last;
   begin
      if maxlinks > maxsize then
         maxlinks := maxsize;
      end if;
      declare
         result : AR.chain (nv'First .. maxlinks);
      begin
         for x in 1 .. maxlinks loop
            result (x) := AR.nbyte1 (Character'Pos (nv (x)));
         end loop;
         return result;
      end;
   end convert;

   ---------------------------------
   --  convert string to textual  --
   ---------------------------------
   function convert (nv : String; maxsize : BLOB_maximum) return AR.textual
   is
      maxlinks : Natural := nv'Last;
   begin
      if maxlinks > maxsize then
         maxlinks := maxsize;
      end if;
      return SU.To_Unbounded_String (nv (nv'First .. maxlinks));
   end convert;

   ----------------------------------
   --  convert string to textwide  --
   ----------------------------------
   function convert (nv : String; maxsize : BLOB_maximum) return AR.textwide
   is
      maxlinks : Natural := nv'Last;
   begin
      if maxlinks > maxsize then
         maxlinks := maxsize;
      end if;
      return SUW.To_Unbounded_Wide_String
        (ACC.To_Wide_String (nv (nv'First .. maxlinks)));
   end convert;


   -----------------------------------
   --  convert string to textsuper  --
   -----------------------------------
   function convert (nv : String; maxsize : BLOB_maximum) return AR.textsuper
   is
      maxlinks : Natural := nv'Last;
   begin
      if maxlinks > maxsize then
         maxlinks := maxsize;
      end if;
      return SWW.To_Unbounded_Wide_Wide_String
        (ACC.To_Wide_Wide_String (nv (nv'First .. maxlinks)));
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
                          category  : LogCategory;
                          message   : String)
   is
   begin
      logger_access.all.log_nominal
        (driver   => statement.dialect,
         category => category,
         message  => SU.To_Unbounded_String (message));
   end log_nominal;


   -------------------
   --  log_problem  --
   -------------------
   procedure log_problem
     (statement  : Base_Statement;
      category   : LogCategory;
      message    : String;
      pull_codes : Boolean := False;
      break      : Boolean := False)
   is
      error_msg  : stmttext    := blank;
      error_code : DriverCodes := 0;
      sqlstate   : TSqlState   := stateless;
   begin
      if pull_codes then
         error_msg  := SU.To_Unbounded_String
                      (statement.connection.all.driverMessage);
         error_code := statement.connection.all.driverCode;
         sqlstate   := statement.connection.all.SqlState;
      end if;

      logger_access.all.log_problem
          (driver     => statement.dialect,
           category   => category,
           message    => SU.To_Unbounded_String (message),
           error_msg  => error_msg,
           error_code => error_code,
           sqlstate   => sqlstate,
           break      => break);
   end log_problem;


   --------------------
   --  bind_proceed  --
   --------------------
   function bind_proceed (Stmt : Base_Statement) return Boolean is
   begin
      if not Stmt.successful_execution then
         raise PRIOR_EXECUTION_FAILED
           with "Use bind after 'execute' but before 'fetch_next'";
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


   ------------------------------------------------------
   --  20 bind functions (impossible to make generic)  --
   ------------------------------------------------------
   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.nbyte0_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_nbyte0, a00 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.nbyte1_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_nbyte1, a01 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.nbyte2_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_nbyte2, a02 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.nbyte3_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_nbyte3, a03 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.nbyte4_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_nbyte4, a04 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.nbyte8_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_nbyte8, a05 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.byte1_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_byte1, a06 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.byte2_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_byte2, a07 => vaxx, bound => True));
      end if;
   end bind;


   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.byte3_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_byte3, a08 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.byte4_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_byte4, a09 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.byte8_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_byte8, a10 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.real9_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_real9, a11 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.real18_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_real18, a12 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.str1_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_textual, a13 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.str2_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_widetext, a14 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.str4_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_supertext, a15 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.time_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_timestamp, a16 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.chain_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_chain, a17 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.enum_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_enumtype, a18 => vaxx, bound => True));
      end if;
   end bind;

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.settype_access) is
   begin
      if Stmt.bind_proceed then
         Stmt.crate.Replace_Element
           (index, (output_type => ft_settype, a19 => vaxx, bound => True));
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


end AdaBase.Statement.Base;
