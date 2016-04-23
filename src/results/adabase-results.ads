--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with CommonText;
with Ada.Calendar;
with Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;

package AdaBase.Results is

   package CT   renames CommonText;
   package AC   renames Ada.Calendar;
   package SUW  renames Ada.Strings.Wide_Unbounded;
   package SUWW renames Ada.Strings.Wide_Wide_Unbounded;

   subtype textual   is CT.Text;
   subtype textwide  is SUW.Unbounded_Wide_String;
   subtype textsuper is SUWW.Unbounded_Wide_Wide_String;

   TARGET_TYPE_TOO_NARROW : exception;
   CONVERSION_FAILED      : exception;
   UNSUPPORTED_CONVERSION : exception;
   COLUMN_DOES_NOT_EXIST  : exception;
   CONSTRUCTOR_DO_NOT_USE : exception;

   -------------------------------------------
   --  Supported Field Types (Standardized) --
   -------------------------------------------

   type nbyte1 is mod 2 ** 8;
   type nbyte2 is mod 2 ** 16;
   type nbyte3 is mod 2 ** 24;
   type nbyte4 is mod 2 ** 32;
   type nbyte8 is mod 2 ** 64;
   type byte8  is range -2 ** 63 .. 2 ** 63 - 1;
   type byte4  is range -2 ** 31 .. 2 ** 31 - 1;
   type byte3  is range -2 ** 23 .. 2 ** 23 - 1;
   type byte2  is range -2 ** 15 .. 2 ** 15 - 1;
   type byte1  is range -2 **  7 .. 2 **  7 - 1;
   type real9  is digits 9;
   type real18 is digits 18;

   subtype nbyte0 is Boolean;

   type chain is array (Positive range <>) of nbyte1;
   type enumtype is record
      enumeration : textual;
      index       : Natural;
   end record;
   type settype is array (Positive range <>) of enumtype;

   type nbyte0_access  is access all nbyte0;
   type nbyte1_access  is access all nbyte1;
   type nbyte2_access  is access all nbyte2;
   type nbyte3_access  is access all nbyte3;
   type nbyte4_access  is access all nbyte4;
   type nbyte8_access  is access all nbyte8;
   type byte1_access   is access all byte1;
   type byte2_access   is access all byte2;
   type byte3_access   is access all byte3;
   type byte4_access   is access all byte4;
   type byte8_access   is access all byte8;
   type real9_access   is access all real9;
   type real18_access  is access all real18;
   type str1_access    is access all textual;
   type str2_access    is access all textwide;
   type str4_access    is access all textsuper;
   type time_access    is access all AC.Time;
   type chain_access   is access all chain;     --  stored as access
   type enum_access    is access all enumtype;
   type settype_access is access all settype;   --  stored as access

end AdaBase.Results;
