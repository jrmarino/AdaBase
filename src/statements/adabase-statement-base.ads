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


with AdaBase.Connection.Base;
with AdaBase.Interfaces.Statement;
with AdaBase.Logger.Facility;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;

package AdaBase.Statement.Base is

   package SU  renames Ada.Strings.Unbounded;
   package ACB renames AdaBase.Connection.Base;
   package AIS renames AdaBase.Interfaces.Statement;
   package ALF renames AdaBase.Logger.Facility;

   subtype stmttext is SU.Unbounded_String;

   blank : constant stmttext := SU.Null_Unbounded_String;

   type Base_Statement is
     abstract limited new Base_Pure and AIS.iStatement with private;
   type basic_statement is access all Base_Statement'Class;

   type stmt_type is (direct_statement, prepared_statement);

   ILLEGAL_BIND_SQL         : exception;
   INVALID_FOR_DIRECT_QUERY : exception;
   INVALID_FOR_RESULT_SET   : exception;
   PRIOR_EXECUTION_FAILED   : exception;

   overriding
   function rows_affected (Stmt : Base_Statement) return AffectedRows;

   overriding
   function successful (Stmt : Base_Statement) return Boolean;

private

   logger_access : ALF.LogFacility_access;

   function Same_Strings (S, T : String) return Boolean;

   procedure transform_sql (Stmt : out Base_Statement; sql : String;
                           new_sql : out String);

   procedure log_nominal (statement : Base_Statement;
                          category  : LogCategory;
                          message   : String);

   procedure log_problem
     (statement  : Base_Statement;
      category   : LogCategory;
      message    : String;
      pull_codes : Boolean := False;
      break      : Boolean := False);


   package Markers is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Positive,
      Equivalent_Keys => Same_Strings,
      Hash            => Ada.Strings.Hash);

   type Base_Statement is
     abstract limited new Base_Pure and AIS.iStatement with record
      successful_execution : Boolean      := False;
      result_present       : Boolean      := False;
      dialect              : TDriver      := foundation;
      impacted             : AffectedRows := 0;
      connection           : ACB.Base_Connection_Access;
      alpha_markers        : Markers.Map;
   end record;


end AdaBase.Statement.Base;
