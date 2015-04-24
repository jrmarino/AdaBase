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

with AdaBase.Results.Generic_Converters;

package AdaBase.Results.Converters is

   package GEN renames AdaBase.Results.Generic_Converters;

   -------------
   -- nbyte 0 --
   -------------
   function convert (nv : AD.nbyte0) return AD.nbyte1;
   function convert (nv : AD.nbyte0) return AD.nbyte2;
   function convert (nv : AD.nbyte0) return AD.nbyte3;
   function convert (nv : AD.nbyte0) return AD.nbyte4;
   function convert (nv : AD.nbyte0) return AD.nbyte8;
   function convert (nv : AD.nbyte0) return AD.byte1;
   function convert (nv : AD.nbyte0) return AD.byte2;
   function convert (nv : AD.nbyte0) return AD.byte3;
   function convert (nv : AD.nbyte0) return AD.byte4;
   function convert (nv : AD.nbyte0) return AD.byte8;
   function convert (nv : AD.nbyte0) return AD.real9;
   function convert (nv : AD.nbyte0) return AD.real18;
   function convert (nv : AD.nbyte0) return AD.textual;
   function convert (nv : AD.nbyte0) return textwide;
   function convert (nv : AD.nbyte0) return textsuper;

   -------------
   -- nbyte 1 --
   -------------
   function convert (nv : AD.nbyte1) return AD.nbyte0;
   function convert (nv : AD.nbyte1) return AD.nbyte2;
   function convert (nv : AD.nbyte1) return AD.nbyte3;
   function convert (nv : AD.nbyte1) return AD.nbyte4;
   function convert (nv : AD.nbyte1) return AD.nbyte8;
   function convert (nv : AD.nbyte1) return AD.byte1;
   function convert (nv : AD.nbyte1) return AD.byte2;
   function convert (nv : AD.nbyte1) return AD.byte3;
   function convert (nv : AD.nbyte1) return AD.byte4;
   function convert (nv : AD.nbyte1) return AD.byte8;
   function convert (nv : AD.nbyte1) return AD.real9;
   function convert (nv : AD.nbyte1) return AD.real18;
   function convert is new GEN.convert2str1 (IntType => AD.nbyte1);
   function convert is new GEN.convert2str2 (IntType => AD.nbyte1);
   function convert is new GEN.convert2str3 (IntType => AD.nbyte1);

   -------------
   -- nbyte 2 --
   -------------
   function convert (nv : AD.nbyte2) return AD.nbyte0;
   function convert (nv : AD.nbyte2) return AD.nbyte1;
   function convert (nv : AD.nbyte2) return AD.nbyte3;
   function convert (nv : AD.nbyte2) return AD.nbyte4;
   function convert (nv : AD.nbyte2) return AD.nbyte8;
   function convert (nv : AD.nbyte2) return AD.byte1;
   function convert (nv : AD.nbyte2) return AD.byte2;
   function convert (nv : AD.nbyte2) return AD.byte3;
   function convert (nv : AD.nbyte2) return AD.byte4;
   function convert (nv : AD.nbyte2) return AD.byte8;
   function convert (nv : AD.nbyte2) return AD.real9;
   function convert (nv : AD.nbyte2) return AD.real18;
   function convert is new GEN.convert2str1 (IntType => AD.nbyte2);
   function convert is new GEN.convert2str2 (IntType => AD.nbyte2);
   function convert is new GEN.convert2str3 (IntType => AD.nbyte2);

   -------------
   -- nbyte 3 --
   -------------
   function convert (nv : AD.nbyte3) return AD.nbyte0;
   function convert (nv : AD.nbyte3) return AD.nbyte1;
   function convert (nv : AD.nbyte3) return AD.nbyte2;
   function convert (nv : AD.nbyte3) return AD.nbyte4;
   function convert (nv : AD.nbyte3) return AD.nbyte8;
   function convert (nv : AD.nbyte3) return AD.byte1;
   function convert (nv : AD.nbyte3) return AD.byte2;
   function convert (nv : AD.nbyte3) return AD.byte3;
   function convert (nv : AD.nbyte3) return AD.byte4;
   function convert (nv : AD.nbyte3) return AD.byte8;
   function convert (nv : AD.nbyte3) return AD.real9;
   function convert (nv : AD.nbyte3) return AD.real18;
   function convert is new GEN.convert2str1 (IntType => AD.nbyte3);
   function convert is new GEN.convert2str2 (IntType => AD.nbyte3);
   function convert is new GEN.convert2str3 (IntType => AD.nbyte3);

   -------------
   -- nbyte 4 --
   -------------
   function convert (nv : AD.nbyte4) return AD.nbyte0;
   function convert (nv : AD.nbyte4) return AD.nbyte1;
   function convert (nv : AD.nbyte4) return AD.nbyte2;
   function convert (nv : AD.nbyte4) return AD.nbyte3;
   function convert (nv : AD.nbyte4) return AD.nbyte8;
   function convert (nv : AD.nbyte4) return AD.byte1;
   function convert (nv : AD.nbyte4) return AD.byte2;
   function convert (nv : AD.nbyte4) return AD.byte3;
   function convert (nv : AD.nbyte4) return AD.byte4;
   function convert (nv : AD.nbyte4) return AD.byte8;
   function convert (nv : AD.nbyte4) return AD.real9;
   function convert (nv : AD.nbyte4) return AD.real18;
   function convert is new GEN.convert2str1 (IntType => AD.nbyte4);
   function convert is new GEN.convert2str2 (IntType => AD.nbyte4);
   function convert is new GEN.convert2str3 (IntType => AD.nbyte4);


   -------------
   -- nbyte 8 --
   -------------
   function convert (nv : AD.nbyte8) return AD.nbyte0;
   function convert (nv : AD.nbyte8) return AD.nbyte1;
   function convert (nv : AD.nbyte8) return AD.nbyte2;
   function convert (nv : AD.nbyte8) return AD.nbyte3;
   function convert (nv : AD.nbyte8) return AD.nbyte4;
   function convert (nv : AD.nbyte8) return AD.byte1;
   function convert (nv : AD.nbyte8) return AD.byte2;
   function convert (nv : AD.nbyte8) return AD.byte3;
   function convert (nv : AD.nbyte8) return AD.byte4;
   function convert (nv : AD.nbyte8) return AD.byte8;
   function convert (nv : AD.nbyte8) return AD.real9;
   function convert (nv : AD.nbyte8) return AD.real18;
   function convert is new GEN.convert2str1 (IntType => AD.nbyte8);
   function convert is new GEN.convert2str2 (IntType => AD.nbyte8);
   function convert is new GEN.convert2str3 (IntType => AD.nbyte8);


   ------------
   -- byte 1 --
   ------------
   function convert (nv : AD.byte1) return AD.nbyte0;
   function convert (nv : AD.byte1) return AD.nbyte1;
   function convert (nv : AD.byte1) return AD.nbyte2;
   function convert (nv : AD.byte1) return AD.nbyte3;
   function convert (nv : AD.byte1) return AD.nbyte4;
   function convert (nv : AD.byte1) return AD.nbyte8;
   function convert (nv : AD.byte1) return AD.byte2;
   function convert (nv : AD.byte1) return AD.byte3;
   function convert (nv : AD.byte1) return AD.byte4;
   function convert (nv : AD.byte1) return AD.byte8;
   function convert (nv : AD.byte1) return AD.real9;
   function convert (nv : AD.byte1) return AD.real18;
   function convert is new GEN.convert2str1 (IntType => AD.byte1);
   function convert is new GEN.convert2str2 (IntType => AD.byte1);
   function convert is new GEN.convert2str3 (IntType => AD.byte1);


   -----------
   -- byte2 --
   -----------
   function convert (nv : AD.byte2) return AD.nbyte0;
   function convert (nv : AD.byte2) return AD.nbyte1;
   function convert (nv : AD.byte2) return AD.nbyte2;
   function convert (nv : AD.byte2) return AD.nbyte3;
   function convert (nv : AD.byte2) return AD.nbyte4;
   function convert (nv : AD.byte2) return AD.nbyte8;
   function convert (nv : AD.byte2) return AD.byte1;
   function convert (nv : AD.byte2) return AD.byte3;
   function convert (nv : AD.byte2) return AD.byte4;
   function convert (nv : AD.byte2) return AD.byte8;
   function convert (nv : AD.byte2) return AD.real9;
   function convert (nv : AD.byte2) return AD.real18;
   function convert is new GEN.convert2str1 (IntType => AD.byte2);
   function convert is new GEN.convert2str2 (IntType => AD.byte2);
   function convert is new GEN.convert2str3 (IntType => AD.byte2);


   -----------
   -- byte3 --
   -----------
   function convert (nv : AD.byte3) return AD.nbyte0;
   function convert (nv : AD.byte3) return AD.nbyte1;
   function convert (nv : AD.byte3) return AD.nbyte2;
   function convert (nv : AD.byte3) return AD.nbyte3;
   function convert (nv : AD.byte3) return AD.nbyte4;
   function convert (nv : AD.byte3) return AD.nbyte8;
   function convert (nv : AD.byte3) return AD.byte1;
   function convert (nv : AD.byte3) return AD.byte2;
   function convert (nv : AD.byte3) return AD.byte4;
   function convert (nv : AD.byte3) return AD.byte8;
   function convert (nv : AD.byte3) return AD.real9;
   function convert (nv : AD.byte3) return AD.real18;
   function convert is new GEN.convert2str1 (IntType => AD.byte3);
   function convert is new GEN.convert2str2 (IntType => AD.byte3);
   function convert is new GEN.convert2str3 (IntType => AD.byte3);


   -----------
   -- byte4 --
   -----------
   function convert (nv : AD.byte4) return AD.nbyte0;
   function convert (nv : AD.byte4) return AD.nbyte1;
   function convert (nv : AD.byte4) return AD.nbyte2;
   function convert (nv : AD.byte4) return AD.nbyte3;
   function convert (nv : AD.byte4) return AD.nbyte4;
   function convert (nv : AD.byte4) return AD.nbyte8;
   function convert (nv : AD.byte4) return AD.byte1;
   function convert (nv : AD.byte4) return AD.byte2;
   function convert (nv : AD.byte4) return AD.byte3;
   function convert (nv : AD.byte4) return AD.byte8;
   function convert (nv : AD.byte4) return AD.real9;
   function convert (nv : AD.byte4) return AD.real18;
   function convert is new GEN.convert2str1 (IntType => AD.byte4);
   function convert is new GEN.convert2str2 (IntType => AD.byte4);
   function convert is new GEN.convert2str3 (IntType => AD.byte4);


   -----------
   -- byte8 --
   -----------
   function convert (nv : AD.byte8) return AD.nbyte0;
   function convert (nv : AD.byte8) return AD.nbyte1;
   function convert (nv : AD.byte8) return AD.nbyte2;
   function convert (nv : AD.byte8) return AD.nbyte3;
   function convert (nv : AD.byte8) return AD.nbyte4;
   function convert (nv : AD.byte8) return AD.nbyte8;
   function convert (nv : AD.byte8) return AD.byte1;
   function convert (nv : AD.byte8) return AD.byte2;
   function convert (nv : AD.byte8) return AD.byte3;
   function convert (nv : AD.byte8) return AD.byte4;
   function convert (nv : AD.byte8) return AD.real9;
   function convert (nv : AD.byte8) return AD.real18;
   function convert is new GEN.convert2str1 (IntType => AD.byte8);
   function convert is new GEN.convert2str2 (IntType => AD.byte8);
   function convert is new GEN.convert2str3 (IntType => AD.byte8);


   -----------
   -- real9 --
   -----------
   function convert (nv : AD.real9) return AD.real18;
   function convert is new GEN.convert3str1 (RealType => AD.real9);
   function convert is new GEN.convert3str2 (RealType => AD.real9);
   function convert is new GEN.convert3str3 (RealType => AD.real9);


   ------------
   -- real18 --
   ------------
   function convert (nv : AD.real18) return AD.real9;
   function convert is new GEN.convert3str1 (RealType => AD.real18);
   function convert is new GEN.convert3str2 (RealType => AD.real18);
   function convert is new GEN.convert3str3 (RealType => AD.real18);


   ------------
   -- String --
   ------------
   function convert (nv : String) return AD.nbyte0;
   function convert is new GEN.convertstr (IntType => AD.nbyte1);
   function convert is new GEN.convertstr (IntType => AD.nbyte2);
   function convert is new GEN.convertstr (IntType => AD.nbyte3);
   function convert is new GEN.convertstr (IntType => AD.nbyte4);
   function convert is new GEN.convertstr (IntType => AD.nbyte8);
   function convert is new GEN.convertstr (IntType => AD.byte1);
   function convert is new GEN.convertstr (IntType => AD.byte2);
   function convert is new GEN.convertstr (IntType => AD.byte3);
   function convert is new GEN.convertstr (IntType => AD.byte4);
   function convert is new GEN.convertstr (IntType => AD.byte8);
   function convert is new GEN.convertst2 (RealType => AD.real9);
   function convert is new GEN.convertst2 (RealType => AD.real18);
   function convert (nv : String) return Wide_String;
   function convert (nv : String) return Wide_Wide_String;


   -----------------
   -- Wide_String --
   -----------------
   function convert (nv : Wide_String) return AD.nbyte0;
   function convert is new GEN.convertst3 (IntType => AD.nbyte1);
   function convert is new GEN.convertst3 (IntType => AD.nbyte2);
   function convert is new GEN.convertst3 (IntType => AD.nbyte3);
   function convert is new GEN.convertst3 (IntType => AD.nbyte4);
   function convert is new GEN.convertst3 (IntType => AD.nbyte8);
   function convert is new GEN.convertst3 (IntType => AD.byte1);
   function convert is new GEN.convertst3 (IntType => AD.byte2);
   function convert is new GEN.convertst3 (IntType => AD.byte3);
   function convert is new GEN.convertst3 (IntType => AD.byte4);
   function convert is new GEN.convertst3 (IntType => AD.byte8);
   function convert is new GEN.convertst4 (RealType => AD.real9);
   function convert is new GEN.convertst4 (RealType => AD.real18);
   function convert (nv : Wide_String) return String;
   function convert (nv : Wide_String) return Wide_Wide_String;


   ----------------------
   -- Wide_Wide_String --
   ----------------------
   function convert (nv : Wide_Wide_String) return AD.nbyte0;
   function convert is new GEN.convertst5 (IntType => AD.nbyte1);
   function convert is new GEN.convertst5 (IntType => AD.nbyte2);
   function convert is new GEN.convertst5 (IntType => AD.nbyte3);
   function convert is new GEN.convertst5 (IntType => AD.nbyte4);
   function convert is new GEN.convertst5 (IntType => AD.nbyte8);
   function convert is new GEN.convertst5 (IntType => AD.byte1);
   function convert is new GEN.convertst5 (IntType => AD.byte2);
   function convert is new GEN.convertst5 (IntType => AD.byte3);
   function convert is new GEN.convertst5 (IntType => AD.byte4);
   function convert is new GEN.convertst5 (IntType => AD.byte8);
   function convert is new GEN.convertst6 (RealType => AD.real9);
   function convert is new GEN.convertst6 (RealType => AD.real18);
   function convert (nv : Wide_Wide_String) return String;
   function convert (nv : Wide_Wide_String) return Wide_String;


   ----------
   -- TIME --
   ----------
   function convert (nv : AC.Time) return String;
   function convert (nv : AC.Time) return Wide_String;
   function convert (nv : AC.Time) return Wide_Wide_String;


   -----------------
   -- ENUMERATION --
   -----------------
   function convert (nv : AD.enumtype) return AD.nbyte1;
   function convert (nv : AD.enumtype) return AD.nbyte2;
   function convert (nv : AD.enumtype) return AD.nbyte3;
   function convert (nv : AD.enumtype) return AD.nbyte4;
   function convert (nv : AD.enumtype) return AD.nbyte8;
   function convert (nv : AD.enumtype) return AD.byte1;
   function convert (nv : AD.enumtype) return AD.byte2;
   function convert (nv : AD.enumtype) return AD.byte3;
   function convert (nv : AD.enumtype) return AD.byte4;
   function convert (nv : AD.enumtype) return AD.byte8;
   function convert (nv : AD.enumtype) return AD.textual;
   function convert (nv : AD.enumtype) return textwide;
   function convert (nv : AD.enumtype) return textsuper;


   ------------------------
   --  CHAIN (OF BYTES)  --
   ------------------------
   function convert (nv : AD.chain) return AD.nbyte1;
   function convert (nv : AD.chain) return AD.nbyte2;
   function convert (nv : AD.chain) return AD.nbyte3;
   function convert (nv : AD.chain) return AD.nbyte4;
   function convert (nv : AD.chain) return AD.nbyte8;


end AdaBase.Results.Converters;
