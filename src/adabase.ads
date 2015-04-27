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

with Ada.Strings.Unbounded;

package AdaBase is

   package SU renames Ada.Strings.Unbounded;

   type ErrorMode      is (silent, warning, raise_exception);
   type CaseMode       is (lower_case, natural_case, upper_case);
   type StringMode     is (return_null, return_zero_length);
   type TransIsolation is (read_uncommitted, read_committed, repeatable_read,
                           serializable);
   type LogCategory    is (connecting, disconnecting, transaction, execution,
                           statement_preparation, statement_execution,
                           miscellaneous, note);
   type TDriver        is (foundation, driver_mysql, driver_firebird,
                           driver_postgresql);
   type TIsoKeywords   is array (TransIsolation) of String (1 .. 16);
   type TraxID         is mod 2 ** 64;

   subtype BLOB_maximum  is Positive range 2 ** 12 .. 2 ** 30;
   subtype TSqlState     is String (1 .. 5);
   subtype DriverCodes   is Integer range -999 .. 1999;
   subtype PosixPort     is Natural range 0 .. 65535;
   subtype AffectedRows  is Integer;
   subtype textual       is SU.Unbounded_String;

   IsoKeywords : constant TIsoKeywords :=
     ("READ UNCOMMITTED",
      "READ COMMITTED  ",
      "REPEATABLE READ ",
      "SERIALIZABLE    ");

   blank       : constant textual := SU.Null_Unbounded_String;
   blankstring : constant String    := "";
   stateless   : constant TSqlState := "     ";
   portless    : constant PosixPort := 0;

   type Error_Info is record
      sql_state      : TSqlState   := stateless;
      driver_code    : DriverCodes := 0;
      driver_message : textual     := blank;
   end record;


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
   type chain_access is access all chain;
   type enumtype is record
      enumeration : textual;
      index       : Natural;
   end record;
   type settype is array (Positive range <>) of enumtype;
   type settype_access is access all settype;

end AdaBase;
