--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Strings.Fixed;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Characters.Conversions;
with Ada.Strings.UTF_Encoding.Strings;

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


   --------------------
   --  convert2utf8  --
   --------------------
   function convert2utf8 (nv : IntType) return Text_UTF8 is
   begin
      return SUTF.Strings.Encode (Item => ctrim (nv'Img));
   end convert2utf8;


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


   --------------------
   --  convert3utf8  --
   --------------------
   function convert3utf8 (nv : RealType) return Text_UTF8 is
   begin
      return SUTF.Strings.Encode (Item => ctrim (nv'Img));
   end convert3utf8;


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


   --------------------
   --  convert2bits  --
   --------------------
   function convert2bits (nv : ModType) return Bits
   is
      result : Bits (0 .. width) := (others => 0);
      mask   : ModType;
   begin
      for x in result'Range loop
         mask := 2 ** x;
         if (nv and mask) > 0 then
            result (x) := 1;
         end if;
      end loop;
      return result;
   end convert2bits;


   ---------------------
   --  convert2chain  --
   ---------------------
   function convert2chain (nv : ModType) return Chain
   is
      bitwidth : constant Natural := (width * 8) - 1;
      result : Chain (1 .. width) := (others => 0);
      asbits  : Bits (0 .. bitwidth) := (others => 0);
      arrow   : Natural := 0;
      mask    : ModType;
      submask : constant array (0 .. 7) of NByte1 := (2 ** 0, 2 ** 1,
                                                      2 ** 2, 2 ** 3,
                                                      2 ** 4, 2 ** 5,
                                                      2 ** 6, 2 ** 7);
   begin
      --  convert to bits first
      for x in asbits'Range loop
         mask := 2 ** x;
         if (nv and mask) > 0 then
            result (x) := 1;
         end if;
      end loop;
      --  convert from bits to nbyte1
      for x in result'Range loop
         for y in submask'Range loop
            if asbits (arrow + y) = 1 then
               result (x) := result (x) + submask (y);
            end if;
         end loop;
         arrow := arrow + 8;
      end loop;
      return result;
   end convert2chain;


   --------------------
   --  convert_bits  --
   --------------------
   function convert_bits (nv : Bits) return ModType
   is
      numbits : Natural := nv'Length;
      asbits : Bits (0 .. numbits - 1) := nv;
      result : ModType := 0;
   begin
      if asbits'Last > MSB then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      for x in asbits'Range loop
         if asbits (x) > 0 then
            result := result + (2 ** x);
         end if;
      end loop;
      return result;
   end convert_bits;


   ---------------------
   --  convert_chain  --
   ---------------------
   function convert_chain (nv : Chain) return ModType
   is
      link : NByte1;
      counter : Natural := 0;
      result : ModType := 0;
   begin
      if nv'Length > width then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      for x in nv'Range loop
         link := nv (x);
         result := result + ModType (link * (2 ** counter));
         counter := counter + 8;
      end loop;
      return result;
   end convert_chain;


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
