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
   function convert (nv : NByte0) return Chain;


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
   function convert is new GEN.convert2str1 (IntType => NByte1);
   function convert is new GEN.convert2str2 (IntType => NByte1);
   function convert is new GEN.convert2str3 (IntType => NByte1);
   function convert (nv : NByte1) return Chain;


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
   function convert is new GEN.convert2str1 (IntType => NByte2);
   function convert is new GEN.convert2str2 (IntType => NByte2);
   function convert is new GEN.convert2str3 (IntType => NByte2);
   function convert (nv : NByte2) return Chain;


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
   function convert is new GEN.convert2str1 (IntType => NByte3);
   function convert is new GEN.convert2str2 (IntType => NByte3);
   function convert is new GEN.convert2str3 (IntType => NByte3);
   function convert (nv : NByte3) return Chain;


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
   function convert is new GEN.convert2str1 (IntType => NByte4);
   function convert is new GEN.convert2str2 (IntType => NByte4);
   function convert is new GEN.convert2str3 (IntType => NByte4);
   function convert (nv : NByte4) return Chain;


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
   function convert is new GEN.convert2str1 (IntType => NByte8);
   function convert is new GEN.convert2str2 (IntType => NByte8);
   function convert is new GEN.convert2str3 (IntType => NByte8);
   function convert (nv : NByte8) return Chain;


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
   function convert is new GEN.convert2str1 (IntType => Byte1);
   function convert is new GEN.convert2str2 (IntType => Byte1);
   function convert is new GEN.convert2str3 (IntType => Byte1);


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
   function convert is new GEN.convert2str1 (IntType => Byte2);
   function convert is new GEN.convert2str2 (IntType => Byte2);
   function convert is new GEN.convert2str3 (IntType => Byte2);


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
   function convert is new GEN.convert2str1 (IntType => Byte3);
   function convert is new GEN.convert2str2 (IntType => Byte3);
   function convert is new GEN.convert2str3 (IntType => Byte3);


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
   function convert is new GEN.convert2str1 (IntType => Byte4);
   function convert is new GEN.convert2str2 (IntType => Byte4);
   function convert is new GEN.convert2str3 (IntType => Byte4);


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
   function convert is new GEN.convert2str1 (IntType => Byte8);
   function convert is new GEN.convert2str2 (IntType => Byte8);
   function convert is new GEN.convert2str3 (IntType => Byte8);


   -----------
   -- Real9 --
   -----------
   function convert (nv : Real9) return Real18;
   function convert is new GEN.convert3str1 (RealType => Real9);
   function convert is new GEN.convert3str2 (RealType => Real9);
   function convert is new GEN.convert3str3 (RealType => Real9);


   ------------
   -- Real18 --
   ------------
   function convert (nv : Real18) return Real9;
   function convert is new GEN.convert3str1 (RealType => Real18);
   function convert is new GEN.convert3str2 (RealType => Real18);
   function convert is new GEN.convert3str3 (RealType => Real18);


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
   function convert (nv : Textual) return String;
   function convert (nv : Textual) return Wide_String;
   function convert (nv : Textual) return Wide_Wide_String;
   function convert (nv : Textual) return AC.Time;
   function convert (nv : Textual) return Chain;
   function convert (nv : Textual) return Enumtype;
   function convert (nv : Textual) return Settype;
   function convert (nv : String) return AC.Time;
   function convert (nv : String) return Enumtype;
   function convert (nv : String; fixed : Natural := 0) return Chain;
   function convert (nv : String; fixed : Natural := 0) return Settype;


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


   ----------
   -- TIME --
   ----------
   function convert (nv : AC.Time) return String;
   function convert (nv : AC.Time) return Wide_String;
   function convert (nv : AC.Time) return Wide_Wide_String;


   -----------------
   -- ENUMERATION --
   -----------------
   function convert (nv : Enumtype) return String;
   function convert (nv : Enumtype) return Wide_String;
   function convert (nv : Enumtype) return Wide_Wide_String;


   ------------------------
   --  CHAIN (OF BYTES)  --
   ------------------------
   function convert (nv : Chain) return NByte0;
   function convert (nv : Chain) return NByte1;
   function convert (nv : Chain) return NByte2;
   function convert (nv : Chain) return NByte3;
   function convert (nv : Chain) return NByte4;
   function convert (nv : Chain) return NByte8;
   function convert (nv : Chain) return String;
   function convert (nv : Chain) return Wide_String;
   function convert (nv : Chain) return Wide_Wide_String;


   ---------------
   -- SET TYPE  --
   ---------------
   function convert (nv : Settype) return String;
   function convert (nv : Settype) return Wide_String;
   function convert (nv : Settype) return Wide_Wide_String;


end AdaBase.Results.Converters;
