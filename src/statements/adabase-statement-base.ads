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


with AdaBase.Interfaces.Statement;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package AdaBase.Statement.Base is

   package AIS renames AdaBase.Interfaces.Statement;

   type Base_Statement is abstract limited new Base_Pure and AIS.iStatement
   with private;

   type stmt_type is (direct_statement, prepared_statement);

   ILLEGAL_BIND_SQL : exception;

private

   function Same_Strings (S, T : String) return Boolean;

   procedure transform_sql (Stmt : out Base_Statement; sql : in out String);

   package Markers is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Positive,
      Equivalent_Keys => Same_Strings,
      Hash            => Ada.Strings.Hash);

   type Base_Statement is abstract limited new Base_Pure and AIS.iStatement
     with record
      successful_execution : Boolean   := False;
      type_of_statement    : stmt_type := direct_statement;
      alpha_markers        : Markers.Map;
   end record;



end AdaBase.Statement.Base;
