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

package AdaBase.DataTypes is

   package SU renames Ada.Strings.Unbounded;

   type ErrorMode      is (silent, warning, raise_exception);
   type CaseMode       is (lower_case, natural_case, upper_case);
   type StringMode     is (return_null, return_zero_length);
   type TransIsolation is (read_uncommitted, read_committed, repeatable_read,
                           serializable);
   type LogCategory    is (connection, disconnection, transaction, execution,
                           statement_preparation, statement_execution,
                           miscellaneous, note);
   type TDriver        is (foundation, mysql, firebird, postgresql);
   type TIsoKeywords   is array (TransIsolation) of String (1 .. 16);
   type TraxID         is mod 2 ** 64;

   subtype BLOB_maximum  is Positive range 2 ** 12 .. 2 ** 30;
   subtype TSqlState     is String (1 .. 5);
   subtype DriverCodes   is Integer range -999 .. 1999;
   subtype AffectedRows  is Integer;
   subtype textual       is SU.Unbounded_String;

   IsoKeywords : constant TIsoKeywords :=
     ("READ UNCOMMITTED",
      "READ COMMITTED  ",
      "REPEATABLE READ ",
      "SERIALIZABLE    ");

   blank : constant textual := SU.Null_Unbounded_String;
   stateless : constant TSqlState := "     ";

   type Error_Info is record
      sql_state      : TSqlState   := stateless;
      driver_code    : DriverCodes := 0;
      driver_message : textual     := blank;
   end record;


end AdaBase.DataTypes;
