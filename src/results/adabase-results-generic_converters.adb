--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

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
   function convertstr (nv : Textual) return IntType
   is
      nverr : constant String := CT.USS (nv);
   begin
      return IntType'Value (nverr);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & nverr & "' (S/I)";
   end convertstr;


   --------------------------
   --  GENERIC convertst2  --
   --------------------------
   function convertst2 (nv : Textual) return RealType
   is
      nverr : constant String := CT.USS (nv);
   begin
      return RealType'Value (nverr);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & nverr & "' (S/R)";
   end convertst2;


   --------------------------
   --  GENERIC convertst3  --
   --------------------------
   function convertst3 (nv : Textwide) return IntType
   is
      wstr : constant Wide_String := SUW.To_Wide_String (Source => nv);
      str  : constant String := ACC.To_String (Item => wstr);
   begin
      return IntType'Value (str);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & str & "' (WS/I)";
   end convertst3;


   --------------------------
   --  GENERIC convertst4  --
   --------------------------
   function convertst4 (nv : Textwide) return RealType
   is
      wstr : constant Wide_String := SUW.To_Wide_String (Source => nv);
      str  : constant String := ACC.To_String (Item => wstr);
   begin
      return RealType'Value (str);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & str & "' (WS/R)";
   end convertst4;


   --------------------------
   --  GENERIC convertst5  --
   --------------------------
   function convertst5 (nv : Textsuper) return IntType
   is
      wwstr : constant Wide_Wide_String := SUWW.To_Wide_Wide_String (nv);
      str   : constant String := ACC.To_String (Item => wwstr);
   begin
      return IntType'Value (str);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & str & "' (WWS/I)";
   end convertst5;


   --------------------------
   --  GENERIC convertst6  --
   --------------------------
   function convertst6 (nv : Textsuper) return RealType
   is
      wwstr : constant Wide_Wide_String := SUWW.To_Wide_Wide_String (nv);
      str   : constant String := ACC.To_String (Item => wwstr);
   begin
      return RealType'Value (str);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & str & "' (WWS/R)";
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


   ---------------------------
   --  GENERIC convert4str  --
   ---------------------------
   function convert4str (nv : String) return IntType is
   begin
      return IntType'Value (nv);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & nv & "' (ST/I)";
   end convert4str;


   ---------------------------
   --  GENERIC convert4st2  --
   ---------------------------
   function convert4st2 (nv : String) return RealType is
   begin
      return RealType'Value (nv);
   exception
      when others =>
         raise CONVERSION_FAILED with "Tried to convert '" & nv & "' (ST/R)";
   end convert4st2;


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
