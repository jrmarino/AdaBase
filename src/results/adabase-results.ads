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

with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;

package AdaBase.Results is

   package AC   renames Ada.Calendar;
   package SU   renames Ada.Strings.Unbounded;
   package SUW  renames Ada.Strings.Wide_Unbounded;
   package SUWW renames Ada.Strings.Wide_Wide_Unbounded;

   subtype textual   is SU.Unbounded_String;
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
