--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Results.Field;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package AdaBase.Results.Sets is

   package ARF renames AdaBase.Results.Field;

   type Datarow is tagged private;
   type Datarow_Access is access all Datarow;
   type Datarow_Set is array (Positive range <>) of Datarow;

   Empty_Datarow : constant Datarow;

   function column (row : Datarow; index : Positive) return ARF.std_field;
   function column (row : Datarow; heading : String) return ARF.std_field;
   function count  (row : Datarow) return Natural;
   function data_exhausted (row : Datarow) return Boolean;

   --  Since it doesn't seem to be possible to construct this type with
   --  descriminates, it needs to be created first and populated with data,
   --  field by field.  The "push" procedure is public only for the driver
   --  or driver's statement, but when pushing is done, the record is locked
   --  to block any attempt by user to push more data onto this.

   procedure push (row        : out Datarow;
                   heading    : String;
                   field      : ARF.std_field;
                   last_field : Boolean := False);

private

   use type ARF.std_field;

   function Same_Strings (S, T : String) return Boolean;

   package field_crate is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => ARF.std_field);

   package heading_map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Positive,
      Equivalent_Keys => Same_Strings,
      Hash            => Ada.Strings.Hash);

   type Datarow is tagged
      record
         crate  : field_crate.Vector;
         map    : heading_map.Map;
         locked : Boolean := False;
         done   : Boolean := False;
      end record;

   Empty_Datarow : constant Datarow := (field_crate.Empty_Vector,
                                        heading_map.Empty_Map,
                                        True, True);

end AdaBase.Results.Sets;
