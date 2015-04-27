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

with AdaBase.Results.Field;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package AdaBase.Results.Sets is

   package ARF renames AdaBase.Results.Field;

   type DataRow is tagged limited private;
   type DataRowSet is array (Positive range <>) of DataRow;

   function column (row : DataRow; index : Positive) return ARF.std_field;
   function column (row : DataRow; heading : String) return ARF.std_field;
   function count  (row : DataRow) return Natural;

   --  Since it doesn't seem to be possible to construct this type with
   --  descriminates, it needs to be created first and populated with data,
   --  field by field.  The "push" procedure is public only for the driver
   --  or driver's statement, but when pushing is done, the record is locked
   --  to block any attempt by user to push more data onto this.

   procedure push (row        : out DataRow;
                   heading    : String;
                   field      : ARF.field_access;
                   last_field : Boolean := False);

private

   use type ARF.field_access;

   function Same_Strings (S, T : String) return Boolean;

   package field_crate is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => ARF.field_access);

   package heading_map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Positive,
      Equivalent_Keys => Same_Strings,
      Hash            => Ada.Strings.Hash);

   type DataRow is tagged limited
      record
         crate  : field_crate.Vector;
         map    : heading_map.Map;
         locked : Boolean := False;
      end record;


end AdaBase.Results.Sets;
