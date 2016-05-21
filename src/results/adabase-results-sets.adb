--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Characters.Handling;

package body AdaBase.Results.Sets is

   package ACH renames Ada.Characters.Handling;

   --------------
   --  column  --
   --------------
   function column (row : Datarow; index : Positive) return ARF.Std_Field is
   begin
      if index > Natural (row.crate.Length) then
         raise COLUMN_DOES_NOT_EXIST with "Column" & index'Img &
           " requested, but only" & row.crate.Length'Img & " columns present.";
      end if;
      return row.crate.Element (Index => index);
   end column;


   --------------
   --  column  --
   --------------
   function column (row : Datarow; heading : String) return ARF.Std_Field
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
      return row.crate.Element (Index => index);
   end column;


   -------------
   --  count  --
   -------------
   function count  (row : Datarow) return Natural is
   begin
      return Natural (row.crate.Length);
   end count;


   ----------------------
   --  data_exhausted  --
   ----------------------
   function data_exhausted (row : Datarow) return Boolean is
   begin
      return row.done;
   end data_exhausted;


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
   procedure push (row        : out Datarow;
                   heading    : String;
                   field      : ARF.Std_Field;
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
