---  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Results.Generic_Converters;

package AdaBase.Results.Converters is

   package GEN renames AdaBase.Results.Generic_Converters;

   -------------
   -- nbyte 0 --
   -------------
   function convert (nv : nbyte0) return nbyte1;
   function convert (nv : nbyte0) return nbyte2;
   function convert (nv : nbyte0) return nbyte3;
   function convert (nv : nbyte0) return nbyte4;
   function convert (nv : nbyte0) return nbyte8;
   function convert (nv : nbyte0) return byte1;
   function convert (nv : nbyte0) return byte2;
   function convert (nv : nbyte0) return byte3;
   function convert (nv : nbyte0) return byte4;
   function convert (nv : nbyte0) return byte8;
   function convert (nv : nbyte0) return real9;
   function convert (nv : nbyte0) return real18;
   function convert (nv : nbyte0) return String;
   function convert (nv : nbyte0) return Wide_String;
   function convert (nv : nbyte0) return Wide_Wide_String;
   function convert (nv : nbyte0) return chain;


   -------------
   -- nbyte 1 --
   -------------
   function convert (nv : nbyte1) return nbyte0;
   function convert (nv : nbyte1) return nbyte2;
   function convert (nv : nbyte1) return nbyte3;
   function convert (nv : nbyte1) return nbyte4;
   function convert (nv : nbyte1) return nbyte8;
   function convert (nv : nbyte1) return byte1;
   function convert (nv : nbyte1) return byte2;
   function convert (nv : nbyte1) return byte3;
   function convert (nv : nbyte1) return byte4;
   function convert (nv : nbyte1) return byte8;
   function convert (nv : nbyte1) return real9;
   function convert (nv : nbyte1) return real18;
   function convert is new GEN.convert2str1 (IntType => nbyte1);
   function convert is new GEN.convert2str2 (IntType => nbyte1);
   function convert is new GEN.convert2str3 (IntType => nbyte1);
   function convert (nv : nbyte1) return chain;


   -------------
   -- nbyte 2 --
   -------------
   function convert (nv : nbyte2) return nbyte0;
   function convert (nv : nbyte2) return nbyte1;
   function convert (nv : nbyte2) return nbyte3;
   function convert (nv : nbyte2) return nbyte4;
   function convert (nv : nbyte2) return nbyte8;
   function convert (nv : nbyte2) return byte1;
   function convert (nv : nbyte2) return byte2;
   function convert (nv : nbyte2) return byte3;
   function convert (nv : nbyte2) return byte4;
   function convert (nv : nbyte2) return byte8;
   function convert (nv : nbyte2) return real9;
   function convert (nv : nbyte2) return real18;
   function convert is new GEN.convert2str1 (IntType => nbyte2);
   function convert is new GEN.convert2str2 (IntType => nbyte2);
   function convert is new GEN.convert2str3 (IntType => nbyte2);
   function convert (nv : nbyte2) return chain;


   -------------
   -- nbyte 3 --
   -------------
   function convert (nv : nbyte3) return nbyte0;
   function convert (nv : nbyte3) return nbyte1;
   function convert (nv : nbyte3) return nbyte2;
   function convert (nv : nbyte3) return nbyte4;
   function convert (nv : nbyte3) return nbyte8;
   function convert (nv : nbyte3) return byte1;
   function convert (nv : nbyte3) return byte2;
   function convert (nv : nbyte3) return byte3;
   function convert (nv : nbyte3) return byte4;
   function convert (nv : nbyte3) return byte8;
   function convert (nv : nbyte3) return real9;
   function convert (nv : nbyte3) return real18;
   function convert is new GEN.convert2str1 (IntType => nbyte3);
   function convert is new GEN.convert2str2 (IntType => nbyte3);
   function convert is new GEN.convert2str3 (IntType => nbyte3);
   function convert (nv : nbyte3) return chain;


   -------------
   -- nbyte 4 --
   -------------
   function convert (nv : nbyte4) return nbyte0;
   function convert (nv : nbyte4) return nbyte1;
   function convert (nv : nbyte4) return nbyte2;
   function convert (nv : nbyte4) return nbyte3;
   function convert (nv : nbyte4) return nbyte8;
   function convert (nv : nbyte4) return byte1;
   function convert (nv : nbyte4) return byte2;
   function convert (nv : nbyte4) return byte3;
   function convert (nv : nbyte4) return byte4;
   function convert (nv : nbyte4) return byte8;
   function convert (nv : nbyte4) return real9;
   function convert (nv : nbyte4) return real18;
   function convert is new GEN.convert2str1 (IntType => nbyte4);
   function convert is new GEN.convert2str2 (IntType => nbyte4);
   function convert is new GEN.convert2str3 (IntType => nbyte4);
   function convert (nv : nbyte4) return chain;


   -------------
   -- nbyte 8 --
   -------------
   function convert (nv : nbyte8) return nbyte0;
   function convert (nv : nbyte8) return nbyte1;
   function convert (nv : nbyte8) return nbyte2;
   function convert (nv : nbyte8) return nbyte3;
   function convert (nv : nbyte8) return nbyte4;
   function convert (nv : nbyte8) return byte1;
   function convert (nv : nbyte8) return byte2;
   function convert (nv : nbyte8) return byte3;
   function convert (nv : nbyte8) return byte4;
   function convert (nv : nbyte8) return byte8;
   function convert (nv : nbyte8) return real9;
   function convert (nv : nbyte8) return real18;
   function convert is new GEN.convert2str1 (IntType => nbyte8);
   function convert is new GEN.convert2str2 (IntType => nbyte8);
   function convert is new GEN.convert2str3 (IntType => nbyte8);
   function convert (nv : nbyte8) return chain;


   ------------
   -- byte 1 --
   ------------
   function convert (nv : byte1) return nbyte0;
   function convert (nv : byte1) return nbyte1;
   function convert (nv : byte1) return nbyte2;
   function convert (nv : byte1) return nbyte3;
   function convert (nv : byte1) return nbyte4;
   function convert (nv : byte1) return nbyte8;
   function convert (nv : byte1) return byte2;
   function convert (nv : byte1) return byte3;
   function convert (nv : byte1) return byte4;
   function convert (nv : byte1) return byte8;
   function convert (nv : byte1) return real9;
   function convert (nv : byte1) return real18;
   function convert is new GEN.convert2str1 (IntType => byte1);
   function convert is new GEN.convert2str2 (IntType => byte1);
   function convert is new GEN.convert2str3 (IntType => byte1);


   -----------
   -- byte2 --
   -----------
   function convert (nv : byte2) return nbyte0;
   function convert (nv : byte2) return nbyte1;
   function convert (nv : byte2) return nbyte2;
   function convert (nv : byte2) return nbyte3;
   function convert (nv : byte2) return nbyte4;
   function convert (nv : byte2) return nbyte8;
   function convert (nv : byte2) return byte1;
   function convert (nv : byte2) return byte3;
   function convert (nv : byte2) return byte4;
   function convert (nv : byte2) return byte8;
   function convert (nv : byte2) return real9;
   function convert (nv : byte2) return real18;
   function convert is new GEN.convert2str1 (IntType => byte2);
   function convert is new GEN.convert2str2 (IntType => byte2);
   function convert is new GEN.convert2str3 (IntType => byte2);


   -----------
   -- byte3 --
   -----------
   function convert (nv : byte3) return nbyte0;
   function convert (nv : byte3) return nbyte1;
   function convert (nv : byte3) return nbyte2;
   function convert (nv : byte3) return nbyte3;
   function convert (nv : byte3) return nbyte4;
   function convert (nv : byte3) return nbyte8;
   function convert (nv : byte3) return byte1;
   function convert (nv : byte3) return byte2;
   function convert (nv : byte3) return byte4;
   function convert (nv : byte3) return byte8;
   function convert (nv : byte3) return real9;
   function convert (nv : byte3) return real18;
   function convert is new GEN.convert2str1 (IntType => byte3);
   function convert is new GEN.convert2str2 (IntType => byte3);
   function convert is new GEN.convert2str3 (IntType => byte3);


   -----------
   -- byte4 --
   -----------
   function convert (nv : byte4) return nbyte0;
   function convert (nv : byte4) return nbyte1;
   function convert (nv : byte4) return nbyte2;
   function convert (nv : byte4) return nbyte3;
   function convert (nv : byte4) return nbyte4;
   function convert (nv : byte4) return nbyte8;
   function convert (nv : byte4) return byte1;
   function convert (nv : byte4) return byte2;
   function convert (nv : byte4) return byte3;
   function convert (nv : byte4) return byte8;
   function convert (nv : byte4) return real9;
   function convert (nv : byte4) return real18;
   function convert is new GEN.convert2str1 (IntType => byte4);
   function convert is new GEN.convert2str2 (IntType => byte4);
   function convert is new GEN.convert2str3 (IntType => byte4);


   -----------
   -- byte8 --
   -----------
   function convert (nv : byte8) return nbyte0;
   function convert (nv : byte8) return nbyte1;
   function convert (nv : byte8) return nbyte2;
   function convert (nv : byte8) return nbyte3;
   function convert (nv : byte8) return nbyte4;
   function convert (nv : byte8) return nbyte8;
   function convert (nv : byte8) return byte1;
   function convert (nv : byte8) return byte2;
   function convert (nv : byte8) return byte3;
   function convert (nv : byte8) return byte4;
   function convert (nv : byte8) return real9;
   function convert (nv : byte8) return real18;
   function convert is new GEN.convert2str1 (IntType => byte8);
   function convert is new GEN.convert2str2 (IntType => byte8);
   function convert is new GEN.convert2str3 (IntType => byte8);


   -----------
   -- real9 --
   -----------
   function convert (nv : real9) return real18;
   function convert is new GEN.convert3str1 (RealType => real9);
   function convert is new GEN.convert3str2 (RealType => real9);
   function convert is new GEN.convert3str3 (RealType => real9);


   ------------
   -- real18 --
   ------------
   function convert (nv : real18) return real9;
   function convert is new GEN.convert3str1 (RealType => real18);
   function convert is new GEN.convert3str2 (RealType => real18);
   function convert is new GEN.convert3str3 (RealType => real18);


   ------------
   -- String --
   ------------
   function convert (nv : textual) return nbyte0;
   function convert is new GEN.convertstr (IntType => nbyte1);
   function convert is new GEN.convertstr (IntType => nbyte2);
   function convert is new GEN.convertstr (IntType => nbyte3);
   function convert is new GEN.convertstr (IntType => nbyte4);
   function convert is new GEN.convertstr (IntType => nbyte8);
   function convert is new GEN.convertstr (IntType => byte1);
   function convert is new GEN.convertstr (IntType => byte2);
   function convert is new GEN.convertstr (IntType => byte3);
   function convert is new GEN.convertstr (IntType => byte4);
   function convert is new GEN.convertstr (IntType => byte8);
   function convert is new GEN.convertst2 (RealType => real9);
   function convert is new GEN.convertst2 (RealType => real18);
   function convert (nv : textual) return String;
   function convert (nv : textual) return Wide_String;
   function convert (nv : textual) return Wide_Wide_String;


   -----------------
   -- Wide_String --
   -----------------
   function convert (nv : textwide) return nbyte0;
   function convert is new GEN.convertst3 (IntType => nbyte1);
   function convert is new GEN.convertst3 (IntType => nbyte2);
   function convert is new GEN.convertst3 (IntType => nbyte3);
   function convert is new GEN.convertst3 (IntType => nbyte4);
   function convert is new GEN.convertst3 (IntType => nbyte8);
   function convert is new GEN.convertst3 (IntType => byte1);
   function convert is new GEN.convertst3 (IntType => byte2);
   function convert is new GEN.convertst3 (IntType => byte3);
   function convert is new GEN.convertst3 (IntType => byte4);
   function convert is new GEN.convertst3 (IntType => byte8);
   function convert is new GEN.convertst4 (RealType => real9);
   function convert is new GEN.convertst4 (RealType => real18);
   function convert (nv : textwide) return String;
   function convert (nv : textwide) return Wide_String;
   function convert (nv : textwide) return Wide_Wide_String;


   ----------------------
   -- Wide_Wide_String --
   ----------------------
   function convert (nv : textsuper) return nbyte0;
   function convert is new GEN.convertst5 (IntType => nbyte1);
   function convert is new GEN.convertst5 (IntType => nbyte2);
   function convert is new GEN.convertst5 (IntType => nbyte3);
   function convert is new GEN.convertst5 (IntType => nbyte4);
   function convert is new GEN.convertst5 (IntType => nbyte8);
   function convert is new GEN.convertst5 (IntType => byte1);
   function convert is new GEN.convertst5 (IntType => byte2);
   function convert is new GEN.convertst5 (IntType => byte3);
   function convert is new GEN.convertst5 (IntType => byte4);
   function convert is new GEN.convertst5 (IntType => byte8);
   function convert is new GEN.convertst6 (RealType => real9);
   function convert is new GEN.convertst6 (RealType => real18);
   function convert (nv : textsuper) return String;
   function convert (nv : textsuper) return Wide_String;
   function convert (nv : textsuper) return Wide_Wide_String;


   ----------
   -- TIME --
   ----------
   function convert (nv : AC.Time) return String;
   function convert (nv : AC.Time) return Wide_String;
   function convert (nv : AC.Time) return Wide_Wide_String;


   -----------------
   -- ENUMERATION --
   -----------------
   function convert (nv : enumtype) return String;
   function convert (nv : enumtype) return Wide_String;
   function convert (nv : enumtype) return Wide_Wide_String;


   ------------------------
   --  CHAIN (OF BYTES)  --
   ------------------------
   function convert (nv : chain) return nbyte0;
   function convert (nv : chain) return nbyte1;
   function convert (nv : chain) return nbyte2;
   function convert (nv : chain) return nbyte3;
   function convert (nv : chain) return nbyte4;
   function convert (nv : chain) return nbyte8;


   ---------------
   -- SET TYPE  --
   ---------------
   function convert (nv : settype_access) return String;
   function convert (nv : settype_access) return Wide_String;
   function convert (nv : settype_access) return Wide_Wide_String;


end AdaBase.Results.Converters;
