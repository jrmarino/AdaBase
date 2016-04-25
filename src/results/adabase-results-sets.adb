--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Characters.Handling;

package body AdaBase.Results.Sets is

   package ACH renames Ada.Characters.Handling;

   ----------------
   --  complete  --
   ----------------
   function complete (DRA : DataRow_Access) return Boolean is
   begin
      return (DRA = null);
   end complete;


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


   --------------
   --  column  --
   --------------
   function column (row : DataRow; heading : String) return ARF.std_field
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
   end column;


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
