---  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Results.Generic_Converters;

package AdaBase.Results.Converters is

   package GEN renames AdaBase.Results.Generic_Converters;

   -------------
   -- NByte 0 --
   -------------
   function convert (nv : NByte0) return NByte1;
   function convert (nv : NByte0) return NByte2;
   function convert (nv : NByte0) return NByte3;
   function convert (nv : NByte0) return NByte4;
   function convert (nv : NByte0) return NByte8;
   function convert (nv : NByte0) return Byte1;
   function convert (nv : NByte0) return Byte2;
   function convert (nv : NByte0) return Byte3;
   function convert (nv : NByte0) return Byte4;
   function convert (nv : NByte0) return Byte8;
   function convert (nv : NByte0) return Real9;
   function convert (nv : NByte0) return Real18;
   function convert (nv : NByte0) return String;
   function convert (nv : NByte0) return Wide_String;
   function convert (nv : NByte0) return Wide_Wide_String;
   function convert (nv : NByte0) return Bits;
   function convert (nv : NByte0) return Chain;
   function convert (nv : NByte0) return Textual;
   function cv2utf8 (nv : NByte0) return Text_UTF8;


   -------------
   -- NByte 1 --
   -------------
   function convert (nv : NByte1) return NByte0;
   function convert (nv : NByte1) return NByte2;
   function convert (nv : NByte1) return NByte3;
   function convert (nv : NByte1) return NByte4;
   function convert (nv : NByte1) return NByte8;
   function convert (nv : NByte1) return Byte1;
   function convert (nv : NByte1) return Byte2;
   function convert (nv : NByte1) return Byte3;
   function convert (nv : NByte1) return Byte4;
   function convert (nv : NByte1) return Byte8;
   function convert (nv : NByte1) return Real9;
   function convert (nv : NByte1) return Real18;
   function cv2utf8 is new GEN.convert2utf8 (IntType => NByte1);
   function convert is new GEN.convert2str1 (IntType => NByte1);
   function convert is new GEN.convert2str2 (IntType => NByte1);
   function convert is new GEN.convert2str3 (IntType => NByte1);
   function convert is new GEN.convert2bits (ModType => NByte1, width => 7);
   function convert is new GEN.convert2chain (ModType => NByte1, width => 1);
   function convert (nv : NByte1) return Textual;


   -------------
   -- NByte 2 --
   -------------
   function convert (nv : NByte2) return NByte0;
   function convert (nv : NByte2) return NByte1;
   function convert (nv : NByte2) return NByte3;
   function convert (nv : NByte2) return NByte4;
   function convert (nv : NByte2) return NByte8;
   function convert (nv : NByte2) return Byte1;
   function convert (nv : NByte2) return Byte2;
   function convert (nv : NByte2) return Byte3;
   function convert (nv : NByte2) return Byte4;
   function convert (nv : NByte2) return Byte8;
   function convert (nv : NByte2) return Real9;
   function convert (nv : NByte2) return Real18;
   function cv2utf8 is new GEN.convert2utf8 (IntType => NByte2);
   function convert is new GEN.convert2str1 (IntType => NByte2);
   function convert is new GEN.convert2str2 (IntType => NByte2);
   function convert is new GEN.convert2str3 (IntType => NByte2);
   function convert is new GEN.convert2bits (ModType => NByte2, width => 15);
   function convert is new GEN.convert2chain (ModType => NByte2, width => 2);
   function convert (nv : NByte2) return Textual;


   -------------
   -- NByte 3 --
   -------------
   function convert (nv : NByte3) return NByte0;
   function convert (nv : NByte3) return NByte1;
   function convert (nv : NByte3) return NByte2;
   function convert (nv : NByte3) return NByte4;
   function convert (nv : NByte3) return NByte8;
   function convert (nv : NByte3) return Byte1;
   function convert (nv : NByte3) return Byte2;
   function convert (nv : NByte3) return Byte3;
   function convert (nv : NByte3) return Byte4;
   function convert (nv : NByte3) return Byte8;
   function convert (nv : NByte3) return Real9;
   function convert (nv : NByte3) return Real18;
   function cv2utf8 is new GEN.convert2utf8 (IntType => NByte3);
   function convert is new GEN.convert2str1 (IntType => NByte3);
   function convert is new GEN.convert2str2 (IntType => NByte3);
   function convert is new GEN.convert2str3 (IntType => NByte3);
   function convert is new GEN.convert2bits (ModType => NByte3, width => 23);
   function convert is new GEN.convert2chain (ModType => NByte3, width => 3);
   function convert (nv : NByte3) return Textual;


   -------------
   -- NByte 4 --
   -------------
   function convert (nv : NByte4) return NByte0;
   function convert (nv : NByte4) return NByte1;
   function convert (nv : NByte4) return NByte2;
   function convert (nv : NByte4) return NByte3;
   function convert (nv : NByte4) return NByte8;
   function convert (nv : NByte4) return Byte1;
   function convert (nv : NByte4) return Byte2;
   function convert (nv : NByte4) return Byte3;
   function convert (nv : NByte4) return Byte4;
   function convert (nv : NByte4) return Byte8;
   function convert (nv : NByte4) return Real9;
   function convert (nv : NByte4) return Real18;
   function cv2utf8 is new GEN.convert2utf8 (IntType => NByte4);
   function convert is new GEN.convert2str1 (IntType => NByte4);
   function convert is new GEN.convert2str2 (IntType => NByte4);
   function convert is new GEN.convert2str3 (IntType => NByte4);
   function convert is new GEN.convert2bits (ModType => NByte4, width => 31);
   function convert is new GEN.convert2chain (ModType => NByte4, width => 4);
   function convert (nv : NByte4) return Textual;


   -------------
   -- NByte 8 --
   -------------
   function convert (nv : NByte8) return NByte0;
   function convert (nv : NByte8) return NByte1;
   function convert (nv : NByte8) return NByte2;
   function convert (nv : NByte8) return NByte3;
   function convert (nv : NByte8) return NByte4;
   function convert (nv : NByte8) return Byte1;
   function convert (nv : NByte8) return Byte2;
   function convert (nv : NByte8) return Byte3;
   function convert (nv : NByte8) return Byte4;
   function convert (nv : NByte8) return Byte8;
   function convert (nv : NByte8) return Real9;
   function convert (nv : NByte8) return Real18;
   function cv2utf8 is new GEN.convert2utf8 (IntType => NByte8);
   function convert is new GEN.convert2str1 (IntType => NByte8);
   function convert is new GEN.convert2str2 (IntType => NByte8);
   function convert is new GEN.convert2str3 (IntType => NByte8);
   function convert is new GEN.convert2bits (ModType => NByte8, width => 63);
   function convert is new GEN.convert2chain (ModType => NByte8, width => 8);
   function convert (nv : NByte8) return Textual;


   ------------
   -- Byte 1 --
   ------------
   function convert (nv : Byte1) return NByte0;
   function convert (nv : Byte1) return NByte1;
   function convert (nv : Byte1) return NByte2;
   function convert (nv : Byte1) return NByte3;
   function convert (nv : Byte1) return NByte4;
   function convert (nv : Byte1) return NByte8;
   function convert (nv : Byte1) return Byte2;
   function convert (nv : Byte1) return Byte3;
   function convert (nv : Byte1) return Byte4;
   function convert (nv : Byte1) return Byte8;
   function convert (nv : Byte1) return Real9;
   function convert (nv : Byte1) return Real18;
   function cv2utf8 is new GEN.convert2utf8 (IntType => Byte1);
   function convert is new GEN.convert2str1 (IntType => Byte1);
   function convert is new GEN.convert2str2 (IntType => Byte1);
   function convert is new GEN.convert2str3 (IntType => Byte1);
   function convert (nv : Byte1) return Textual;


   -----------
   -- Byte2 --
   -----------
   function convert (nv : Byte2) return NByte0;
   function convert (nv : Byte2) return NByte1;
   function convert (nv : Byte2) return NByte2;
   function convert (nv : Byte2) return NByte3;
   function convert (nv : Byte2) return NByte4;
   function convert (nv : Byte2) return NByte8;
   function convert (nv : Byte2) return Byte1;
   function convert (nv : Byte2) return Byte3;
   function convert (nv : Byte2) return Byte4;
   function convert (nv : Byte2) return Byte8;
   function convert (nv : Byte2) return Real9;
   function convert (nv : Byte2) return Real18;
   function cv2utf8 is new GEN.convert2utf8 (IntType => Byte2);
   function convert is new GEN.convert2str1 (IntType => Byte2);
   function convert is new GEN.convert2str2 (IntType => Byte2);
   function convert is new GEN.convert2str3 (IntType => Byte2);
   function convert (nv : Byte2) return Textual;


   -----------
   -- Byte3 --
   -----------
   function convert (nv : Byte3) return NByte0;
   function convert (nv : Byte3) return NByte1;
   function convert (nv : Byte3) return NByte2;
   function convert (nv : Byte3) return NByte3;
   function convert (nv : Byte3) return NByte4;
   function convert (nv : Byte3) return NByte8;
   function convert (nv : Byte3) return Byte1;
   function convert (nv : Byte3) return Byte2;
   function convert (nv : Byte3) return Byte4;
   function convert (nv : Byte3) return Byte8;
   function convert (nv : Byte3) return Real9;
   function convert (nv : Byte3) return Real18;
   function cv2utf8 is new GEN.convert2utf8 (IntType => Byte3);
   function convert is new GEN.convert2str1 (IntType => Byte3);
   function convert is new GEN.convert2str2 (IntType => Byte3);
   function convert is new GEN.convert2str3 (IntType => Byte3);
   function convert (nv : Byte3) return Textual;


   -----------
   -- Byte4 --
   -----------
   function convert (nv : Byte4) return NByte0;
   function convert (nv : Byte4) return NByte1;
   function convert (nv : Byte4) return NByte2;
   function convert (nv : Byte4) return NByte3;
   function convert (nv : Byte4) return NByte4;
   function convert (nv : Byte4) return NByte8;
   function convert (nv : Byte4) return Byte1;
   function convert (nv : Byte4) return Byte2;
   function convert (nv : Byte4) return Byte3;
   function convert (nv : Byte4) return Byte8;
   function convert (nv : Byte4) return Real9;
   function convert (nv : Byte4) return Real18;
   function cv2utf8 is new GEN.convert2utf8 (IntType => Byte4);
   function convert is new GEN.convert2str1 (IntType => Byte4);
   function convert is new GEN.convert2str2 (IntType => Byte4);
   function convert is new GEN.convert2str3 (IntType => Byte4);
   function convert (nv : Byte4) return Textual;


   -----------
   -- Byte8 --
   -----------
   function convert (nv : Byte8) return NByte0;
   function convert (nv : Byte8) return NByte1;
   function convert (nv : Byte8) return NByte2;
   function convert (nv : Byte8) return NByte3;
   function convert (nv : Byte8) return NByte4;
   function convert (nv : Byte8) return NByte8;
   function convert (nv : Byte8) return Byte1;
   function convert (nv : Byte8) return Byte2;
   function convert (nv : Byte8) return Byte3;
   function convert (nv : Byte8) return Byte4;
   function convert (nv : Byte8) return Real9;
   function convert (nv : Byte8) return Real18;
   function cv2utf8 is new GEN.convert2utf8 (IntType => Byte8);
   function convert is new GEN.convert2str1 (IntType => Byte8);
   function convert is new GEN.convert2str2 (IntType => Byte8);
   function convert is new GEN.convert2str3 (IntType => Byte8);
   function convert (nv : Byte8) return Textual;


   -----------
   -- Real9 --
   -----------
   function convert (nv : Real9) return Real18;
   function cv2utf8 is new GEN.convert3utf8 (RealType => Real9);
   function convert is new GEN.convert3str1 (RealType => Real9);
   function convert is new GEN.convert3str2 (RealType => Real9);
   function convert is new GEN.convert3str3 (RealType => Real9);
   function convert (nv : Real9) return Textual;


   ------------
   -- Real18 --
   ------------
   function convert (nv : Real18) return Real9;
   function cv2utf8 is new GEN.convert3utf8 (RealType => Real18);
   function convert is new GEN.convert3str1 (RealType => Real18);
   function convert is new GEN.convert3str2 (RealType => Real18);
   function convert is new GEN.convert3str3 (RealType => Real18);
   function convert (nv : Real18) return Textual;


   ------------
   -- String --
   ------------
   function convert (nv : Textual) return NByte0;
   function convert is new GEN.convertstr (IntType => NByte1);
   function convert is new GEN.convertstr (IntType => NByte2);
   function convert is new GEN.convertstr (IntType => NByte3);
   function convert is new GEN.convertstr (IntType => NByte4);
   function convert is new GEN.convertstr (IntType => NByte8);
   function convert is new GEN.convertstr (IntType => Byte1);
   function convert is new GEN.convertstr (IntType => Byte2);
   function convert is new GEN.convertstr (IntType => Byte3);
   function convert is new GEN.convertstr (IntType => Byte4);
   function convert is new GEN.convertstr (IntType => Byte8);
   function convert is new GEN.convertst2 (RealType => Real9);
   function convert is new GEN.convertst2 (RealType => Real18);
   function cv2utf8 (nv : Textual) return Text_UTF8;
   function convert (nv : Textual) return String;
   function convert (nv : Textual) return Wide_String;
   function convert (nv : Textual) return Wide_Wide_String;
   function convert (nv : Textual) return AC.Time;
   function convert (nv : Textual) return Chain;
   function convert (nv : Textual) return Enumtype;
   function convert (nv : Textual) return Settype;
   function convert (nv : Textual) return Bits;
   function convert (nv : String) return AC.Time;
   function convert (nv : String) return Enumtype;
   function convert (nv : String; fixed : Natural := 0) return Chain;
   function convert (nv : String; fixed : Natural := 0) return Settype;
   function convert (nv : String; fixed : Natural := 0) return Bits;
   function cv2utf8 (nv : String) return Text_UTF8;
   function cvu2str (nv : Text_UTF8) return String;
   function cvu2str (nv : Text_UTF8) return Wide_String;
   function cvu2str (nv : Text_UTF8) return Wide_Wide_String;


   -----------------
   -- Wide_String --
   -----------------
   function convert (nv : Textwide) return NByte0;
   function convert is new GEN.convertst3 (IntType => NByte1);
   function convert is new GEN.convertst3 (IntType => NByte2);
   function convert is new GEN.convertst3 (IntType => NByte3);
   function convert is new GEN.convertst3 (IntType => NByte4);
   function convert is new GEN.convertst3 (IntType => NByte8);
   function convert is new GEN.convertst3 (IntType => Byte1);
   function convert is new GEN.convertst3 (IntType => Byte2);
   function convert is new GEN.convertst3 (IntType => Byte3);
   function convert is new GEN.convertst3 (IntType => Byte4);
   function convert is new GEN.convertst3 (IntType => Byte8);
   function convert is new GEN.convertst4 (RealType => Real9);
   function convert is new GEN.convertst4 (RealType => Real18);
   function convert (nv : Textwide) return String;
   function convert (nv : Textwide) return Wide_String;
   function convert (nv : Textwide) return Wide_Wide_String;
   function convert (nv : Textwide) return AC.Time;
   function convert (nv : Textwide) return Chain;
   function convert (nv : Textwide) return Enumtype;
   function convert (nv : Textwide) return Settype;
   function convert (nv : Textwide) return Textual;
   function convert (nv : Textwide) return Bits;
   function cv2utf8 (nv : Textwide) return Text_UTF8;


   ----------------------
   -- Wide_Wide_String --
   ----------------------
   function convert (nv : Textsuper) return NByte0;
   function convert is new GEN.convertst5 (IntType => NByte1);
   function convert is new GEN.convertst5 (IntType => NByte2);
   function convert is new GEN.convertst5 (IntType => NByte3);
   function convert is new GEN.convertst5 (IntType => NByte4);
   function convert is new GEN.convertst5 (IntType => NByte8);
   function convert is new GEN.convertst5 (IntType => Byte1);
   function convert is new GEN.convertst5 (IntType => Byte2);
   function convert is new GEN.convertst5 (IntType => Byte3);
   function convert is new GEN.convertst5 (IntType => Byte4);
   function convert is new GEN.convertst5 (IntType => Byte8);
   function convert is new GEN.convertst6 (RealType => Real9);
   function convert is new GEN.convertst6 (RealType => Real18);
   function convert (nv : Textsuper) return String;
   function convert (nv : Textsuper) return Wide_String;
   function convert (nv : Textsuper) return Wide_Wide_String;
   function convert (nv : Textsuper) return AC.Time;
   function convert (nv : Textsuper) return Chain;
   function convert (nv : Textsuper) return Enumtype;
   function convert (nv : Textsuper) return Settype;
   function convert (nv : Textsuper) return Textual;
   function convert (nv : Textsuper) return Bits;
   function cv2utf8 (nv : Textsuper) return Text_UTF8;


   ----------
   -- TIME --
   ----------
   function convert (nv : AC.Time) return String;
   function convert (nv : AC.Time) return Wide_String;
   function convert (nv : AC.Time) return Wide_Wide_String;
   function convert (nv : AC.Time) return Textual;
   function cv2utf8 (nv : AC.Time) return Text_UTF8;


   -----------------
   -- ENUMERATION --
   -----------------
   function convert (nv : Enumtype) return String;
   function convert (nv : Enumtype) return Wide_String;
   function convert (nv : Enumtype) return Wide_Wide_String;
   function convert (nv : Enumtype) return Textual;
   function cv2utf8 (nv : Enumtype) return Text_UTF8;


   ----------------
   -- BITS TYPE  --
   ----------------
   function convert (nv : Bits) return NByte0;
   function convert is new GEN.convert_bits (ModType => NByte1, MSB => 7);
   function convert is new GEN.convert_bits (ModType => NByte2, MSB => 15);
   function convert is new GEN.convert_bits (ModType => NByte3, MSB => 23);
   function convert is new GEN.convert_bits (ModType => NByte4, MSB => 31);
   function convert is new GEN.convert_bits (ModType => NByte8, MSB => 63);
   function convert (nv : Bits) return String;
   function convert (nv : Bits) return Wide_String;
   function convert (nv : Bits) return Wide_Wide_String;
   function convert (nv : Bits) return Textual;
   function convert (nv : Bits) return Chain;


   ------------------------
   --  CHAIN (OF BYTES)  --
   ------------------------
   function convert (nv : Chain) return NByte0;
   function convert is new GEN.convert_chain (ModType => NByte1, width => 1);
   function convert is new GEN.convert_chain (ModType => NByte2, width => 2);
   function convert is new GEN.convert_chain (ModType => NByte3, width => 3);
   function convert is new GEN.convert_chain (ModType => NByte4, width => 4);
   function convert is new GEN.convert_chain (ModType => NByte8, width => 8);
   function convert (nv : Chain) return String;
   function convert (nv : Chain) return Wide_String;
   function convert (nv : Chain) return Wide_Wide_String;
   function convert (nv : Chain) return Textual;
   function convert (nv : Chain) return Bits;


   ---------------
   -- SET TYPE  --
   ---------------
   function convert (nv : Settype) return String;
   function convert (nv : Settype) return Wide_String;
   function convert (nv : Settype) return Wide_Wide_String;
   function convert (nv : Settype) return Textual;
   function cv2utf8 (nv : Settype) return Text_UTF8;


end AdaBase.Results.Converters;
