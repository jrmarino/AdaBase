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

with Ada.Characters.Handling;

package body AdaBase.Results.Sets is

   package ACH renames Ada.Characters.Handling;

   --------------
   --  column  --
   --------------
   function column (row : DataRow; index : Positive) return ARF.std_field is
   begin
      if index > Natural (row.crate.Length) then
         raise COLUMN_DOES_NOT_EXIST with "Column" & index'Img &
           " requested, but only" & row.crate.Length'Img & " rows present.";
      end if;
      return row.crate.Element (Index => index).all;
   end column;


   ------------
   --  hash  --
   ------------
   function hash (row : DataRow; heading : String) return ARF.std_field
   is
      use type heading_map.Cursor;
      cursor : heading_map.Cursor;
      index  : Positive;
      headup : String := ACH.To_Upper (heading);
   begin
      cursor := row.map.Find (Key => headup);
      if cursor = heading_map.No_Element then
         raise COLUMN_DOES_NOT_EXIST with
           "There is no column named '" & headup & "'.";
      end if;
      index := heading_map.Element (Position => cursor);
      return row.crate.Element (Index => index).all;
   end hash;


   -------------
   --  count  --
   -------------
   function count  (row : DataRow) return Natural is
   begin
      return Natural (row.crate.Length);
   end count;


   --------------------
   --  Same_Strings  --
   --------------------
   function Same_Strings (S, T : String) return Boolean is
   begin
      return S = T;
   end Same_Strings;


   ------------
   --  push  --
   ------------
   procedure push (row        : out DataRow;
                   heading    : String;
                   field      : ARF.field_access;
                   last_field : Boolean := False)
   is
   begin
      if row.locked then
         raise CONSTRUCTOR_DO_NOT_USE with "The push method is not for you.";
      end if;

      if last_field then
         row.locked := True;
      end if;

      row.crate.Append (New_Item => field);
      row.map.Insert (Key      => ACH.To_Upper (heading),
                      New_Item => row.crate.Last_Index);
   end push;


end AdaBase.Results.Sets;
