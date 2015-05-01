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

   ---------------------
   --  transform_sql  --
   ---------------------
   procedure transform_sql (Stmt : out Base_Statement; sql : in out String)
   is
      sql_mask : String := sql;
      sql_hold : String := sql;
   begin
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
            sql_hold (start .. arrow - 1) := scab;
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
      sql := sql_hold;
   end transform_sql;


   --------------------
   --  Same_Strings  --
   --------------------
   function Same_Strings (S, T : String) return Boolean is
   begin
      return S = T;
   end Same_Strings;

end AdaBase.Statement.Base;
