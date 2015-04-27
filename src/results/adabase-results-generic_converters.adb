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
   function convertstr (nv : textual) return IntType
   is
      nverr : constant String := SU.To_String (nv);
   begin
      return IntType'Value (nverr);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & nverr & "'";
   end convertstr;


   --------------------------
   --  GENERIC convertst2  --
   --------------------------
   function convertst2 (nv : textual) return RealType
   is
      nverr : constant String := SU.To_String (nv);
   begin
      return RealType'Value (nverr);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & nverr & "'";
   end convertst2;


   --------------------------
   --  GENERIC convertst3  --
   --------------------------
   function convertst3 (nv : textwide) return IntType
   is
      wstr : constant Wide_String := SUW.To_Wide_String (Source => nv);
      str  : constant String := ACC.To_String (Item => wstr);
   begin
      return IntType'Value (str);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & str & "'";
   end convertst3;


   --------------------------
   --  GENERIC convertst4  --
   --------------------------
   function convertst4 (nv : textwide) return RealType
   is
      wstr : constant Wide_String := SUW.To_Wide_String (Source => nv);
      str  : constant String := ACC.To_String (Item => wstr);
   begin
      return RealType'Value (str);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & str & "'";
   end convertst4;


   --------------------------
   --  GENERIC convertst5  --
   --------------------------
   function convertst5 (nv : textsuper) return IntType
   is
      wwstr : constant Wide_Wide_String := SUWW.To_Wide_Wide_String (nv);
      str   : constant String := ACC.To_String (Item => wwstr);
   begin
      return IntType'Value (str);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & str & "'";
   end convertst5;


   --------------------------
   --  GENERIC convertst6  --
   --------------------------
   function convertst6 (nv : textsuper) return RealType
   is
      wwstr : constant Wide_Wide_String := SUWW.To_Wide_Wide_String (nv);
      str   : constant String := ACC.To_String (Item => wwstr);
   begin
      return RealType'Value (str);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & str & "'";
   end convertst6;


   ----------------------------
   --  GENERIC convert2str1  --
   ----------------------------
   function convert2str1 (nv : IntType) return String is
   begin
      return ctrim (nv'Img);
   end convert2str1;


   ----------------------------
   --  GENERIC convert2str2  --
   ----------------------------
   function convert2str2 (nv : IntType) return Wide_String is
   begin
      return wtrim (nv'Img);
   end convert2str2;


   ----------------------------
   --  GENERIC convert2str3  --
   ----------------------------
   function convert2str3 (nv : IntType) return Wide_Wide_String is
   begin
      return strim (nv'Img);
   end convert2str3;


   ----------------------------
   --  GENERIC convert3str1  --
   ----------------------------
   function convert3str1 (nv : RealType) return String is
   begin
      return ctrim (nv'Img);
   end convert3str1;


   ----------------------------
   --  GENERIC convert3str2  --
   ----------------------------
   function convert3str2 (nv : RealType) return Wide_String is
   begin
      return wtrim (nv'Img);
   end convert3str2;


   ----------------------------
   --  GENERIC convert3str3  --
   ----------------------------
   function convert3str3 (nv : RealType) return Wide_Wide_String is
   begin
      return strim (nv'Img);
   end convert3str3;


   ------------------------------
   --  PRIVATE TRIM FUNCTIONS  --
   ------------------------------

   function ctrim (raw : String) return String is
   begin
      return AS.Fixed.Trim (raw, AS.Left);
   end ctrim;

   function wtrim (raw : String) return Wide_String is
   begin
      return ACC.To_Wide_String (AS.Fixed.Trim (raw, AS.Left));
   end wtrim;

   function strim (raw : String) return Wide_Wide_String is
   begin
      return ACC.To_Wide_Wide_String (AS.Fixed.Trim (raw, AS.Left));
   end strim;

end AdaBase.Results.Generic_Converters;
