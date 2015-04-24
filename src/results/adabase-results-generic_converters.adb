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

with Ada.Strings.Fixed;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Characters.Conversions;

package body AdaBase.Results.Generic_Converters is

   package AS  renames Ada.Strings;
   package ACC renames Ada.Characters.Conversions;

   --------------------------
   --  GENERIC convertstr  --
   --------------------------
   function convertstr (nv : String) return IntType is
   begin
      return IntType'Value (nv);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & nv & "'";
   end convertstr;


   --------------------------
   --  GENERIC convertst2  --
   --------------------------
   function convertst2 (nv : String) return RealType is
   begin
      return RealType'Value (nv);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & nv & "'";
   end convertst2;


   --------------------------
   --  GENERIC convertst3  --
   --------------------------
   function convertst3 (nv : Wide_String) return IntType
   is
      str : constant String := ACC.To_String (Item => nv);
   begin
      return IntType'Value (str);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & str & "'";
   end convertst3;


   --------------------------
   --  GENERIC convertst4  --
   --------------------------
   function convertst4 (nv : Wide_String) return RealType
   is
      str : constant String := ACC.To_String (Item => nv);
   begin
      return RealType'Value (str);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & str & "'";
   end convertst4;


   --------------------------
   --  GENERIC convertst5  --
   --------------------------
   function convertst5 (nv : Wide_Wide_String) return IntType
   is
      str : constant String := ACC.To_String (Item => nv);
   begin
      return IntType'Value (str);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & str & "'";
   end convertst5;


   --------------------------
   --  GENERIC convertst6  --
   --------------------------
   function convertst6 (nv : Wide_Wide_String) return RealType
   is
      str : constant String := ACC.To_String (Item => nv);
   begin
      return RealType'Value (str);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & str & "'";
   end convertst6;



   ----------------------------
   --  GENERIC convert2str1  --
   ----------------------------
   function convert2str1 (nv : IntType) return AD.textual is
   begin
      return ctrim (nv'Img);
   end convert2str1;


   ----------------------------
   --  GENERIC convert2str2  --
   ----------------------------
   function convert2str2 (nv : IntType) return textwide is
   begin
      return wtrim (ACC.To_Wide_String (nv'Img));
   end convert2str2;


   ----------------------------
   --  GENERIC convert2str3  --
   ----------------------------
   function convert2str3 (nv : IntType) return textsuper is
   begin
      return strim (ACC.To_Wide_Wide_String (nv'Img));
   end convert2str3;


   ----------------------------
   --  GENERIC convert3str1  --
   ----------------------------
   function convert3str1 (nv : RealType) return AD.textual is
   begin
      return ctrim (nv'Img);
   end convert3str1;


   ----------------------------
   --  GENERIC convert3str2  --
   ----------------------------
   function convert3str2 (nv : RealType) return textwide is
   begin
      return wtrim (ACC.To_Wide_String (nv'Img));
   end convert3str2;


   ----------------------------
   --  GENERIC convert3str3  --
   ----------------------------
   function convert3str3 (nv : RealType) return textsuper is
   begin
      return strim (ACC.To_Wide_Wide_String (nv'Img));
   end convert3str3;


   ------------------------------
   --  PRIVATE TRIM FUNCTIONS  --
   ------------------------------

   function ctrim (raw : String) return AD.textual
   is
      trimmed_str : constant String := AS.Fixed.Trim (raw, AS.Left);
   begin
      return AD.SU.To_Unbounded_String (trimmed_str);
   end ctrim;

   function wtrim (raw : Wide_String) return textwide
   is
      trimmed_str : constant Wide_String := AS.Wide_Fixed.Trim (raw, AS.Left);
   begin
      return SUW.To_Unbounded_Wide_String (trimmed_str);
   end wtrim;

   function strim (raw : Wide_Wide_String) return textsuper
   is
      trimmed_str : constant Wide_Wide_String :=
        AS.Wide_Wide_Fixed.Trim (raw, AS.Left);
   begin
      return SUWW.To_Unbounded_Wide_Wide_String (trimmed_str);
   end strim;

end AdaBase.Results.Generic_Converters;
