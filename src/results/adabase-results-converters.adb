--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Strings;
with Ada.Characters.Conversions;
with Ada.Calendar.Formatting;
with Interfaces;
with System;

package body AdaBase.Results.Converters is

   package AS  renames Ada.Strings;
   package ACC renames Ada.Characters.Conversions;
   package ACF renames Ada.Calendar.Formatting;
   package BIT renames Interfaces;

   use type System.Bit_Order;
   Big_Endian : constant Boolean :=
     (System.Default_Bit_Order = System.High_Order_First);

   ---------------------------
   --  CONVERT FROM NBYTE0  --
   ---------------------------
   function convert (nv : nbyte0) return nbyte1 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : nbyte0) return nbyte2 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : nbyte0) return nbyte3 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : nbyte0) return nbyte4 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : nbyte0) return nbyte8 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : nbyte0) return byte1 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : nbyte0) return byte2 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : nbyte0) return byte3 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : nbyte0) return byte4 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : nbyte0) return byte8 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : nbyte0) return real9 is
   begin
      case nv is
         when True  => return real9 (1);
         when False => return real9 (0);
      end case;
   end convert;

   function convert (nv : nbyte0) return real18 is
   begin
      case nv is
         when True  => return real18 (1);
         when False => return real18 (0);
      end case;
   end convert;

   function convert (nv : nbyte0) return String is
   begin
      case nv is
         when True  => return "1";
         when False => return "0";
      end case;
   end convert;

   function convert (nv : nbyte0) return Wide_String is
   begin
      case nv is
         when True  => return "1";
         when False => return "0";
      end case;
   end convert;

   function convert (nv : nbyte0) return Wide_Wide_String is
   begin
      case nv is
         when True  => return "1";
         when False => return "0";
      end case;
   end convert;

   function convert (nv : nbyte0) return chain
   is
     result : chain (1 .. 1) := (others => 0);
   begin
      if nv then
         result (1) := 1;
      end if;
      return result;
   end convert;


   ---------------------------
   --  CONVERT FROM NBYTE1  --
   ---------------------------
   function convert (nv : nbyte1) return nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte1) return nbyte2 is
   begin
      return nbyte2 (nv);
   end convert;

   function convert (nv : nbyte1) return nbyte3 is
   begin
      return nbyte3 (nv);
   end convert;

   function convert (nv : nbyte1) return nbyte4 is
   begin
      return nbyte4 (nv);
   end convert;

   function convert (nv : nbyte1) return nbyte8 is
   begin
      return nbyte8 (nv);
   end convert;

   function convert (nv : nbyte1) return byte1
   is
      max : constant nbyte1 := nbyte1 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte1 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte1) return byte2 is
   begin
      return byte2 (nv);
   end convert;

   function convert (nv : nbyte1) return byte3 is
   begin
      return byte3 (nv);
   end convert;

   function convert (nv : nbyte1) return byte4 is
   begin
      return byte4 (nv);
   end convert;

   function convert (nv : nbyte1) return byte8 is
   begin
      return byte8 (nv);
   end convert;

   function convert (nv : nbyte1) return real9 is
   begin
      return real9 (nv);
   end convert;

   function convert (nv : nbyte1) return real18 is
   begin
      return real18 (nv);
   end convert;

   function convert (nv : nbyte1) return chain
   is
      result : constant chain (1 .. 1) := (1 => nv);
   begin
      return result;
   end convert;


   ---------------------------
   --  CONVERT FROM NBYTE2  --
   ---------------------------
   function convert (nv : nbyte2) return nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte2) return nbyte1
   is
      max : constant nbyte2 := nbyte2 (nbyte1'Last);
   begin
      case nv is
         when 0 .. max => return nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte2) return nbyte3 is
   begin
      return nbyte3 (nv);
   end convert;

   function convert (nv : nbyte2) return nbyte4 is
   begin
      return nbyte4 (nv);
   end convert;

   function convert (nv : nbyte2) return nbyte8 is
   begin
      return nbyte8 (nv);
   end convert;

   function convert (nv : nbyte2) return byte1
   is
      max : constant nbyte2 := nbyte2 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte1 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte2) return byte2
   is
      max : constant nbyte2 := nbyte2 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte2 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte2) return byte3 is
   begin
      return byte3 (nv);
   end convert;

   function convert (nv : nbyte2) return byte4 is
   begin
      return byte4 (nv);
   end convert;

   function convert (nv : nbyte2) return byte8 is
   begin
      return byte8 (nv);
   end convert;

   function convert (nv : nbyte2) return real9 is
   begin
      return real9 (nv);
   end convert;

   function convert (nv : nbyte2) return real18 is
   begin
      return real18 (nv);
   end convert;

   function convert (nv : nbyte2) return chain
   is
      use type nbyte2;
      result : chain (1 .. 2);
      block1 : constant nbyte1 := nbyte1 (nv and 16#FF#);
      block2 : constant nbyte1 := nbyte1
                        (BIT.Shift_Right (BIT.Unsigned_16 (nv), 8));
   begin
      if Big_Endian then
         result := (block2, block1);
      else
         result := (block1, block2);
      end if;
      return result;
   end convert;


   ---------------------------
   --  CONVERT FROM NBYTE3  --
   ---------------------------
   function convert (nv : nbyte3) return nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte3) return nbyte1
   is
      max : constant nbyte3 := nbyte3 (nbyte1'Last);
   begin
      case nv is
         when 0 .. max => return nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte3) return nbyte2
   is
      max : constant nbyte3 := nbyte3 (nbyte2'Last);
   begin
      case nv is
         when 0 .. max => return nbyte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte3) return nbyte4 is
   begin
      return nbyte4 (nv);
   end convert;

   function convert (nv : nbyte3) return nbyte8 is
   begin
      return nbyte8 (nv);
   end convert;

   function convert (nv : nbyte3) return byte1
   is
      max : constant nbyte3 := nbyte3 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte1 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte3) return byte2
   is
      max : constant nbyte3 := nbyte3 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte2 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte3) return byte3
   is
      max : constant nbyte3 := nbyte3 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte3 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte3) return byte4 is
   begin
      return byte4 (nv);
   end convert;

   function convert (nv : nbyte3) return byte8 is
   begin
      return byte8 (nv);
   end convert;

   function convert (nv : nbyte3) return real9 is
   begin
      return real9 (nv);
   end convert;

   function convert (nv : nbyte3) return real18 is
   begin
      return real18 (nv);
   end convert;

   function convert (nv : nbyte3) return chain
   is
      use type nbyte3;
      result : chain (1 .. 3);
      block1 : constant nbyte1 := nbyte1 (nv and 16#FF#);
      block2 : constant nbyte1 := nbyte1 (BIT.Shift_Right
                        (BIT.Unsigned_32 (nv and 16#FF00#), 8));
      block3 : constant nbyte1 := nbyte1 (BIT.Shift_Right
                        (BIT.Unsigned_32 (nv and 16#FF0000#), 16));
   begin
      if Big_Endian then
         result := (block3, block2, block1);
      else
         result := (block1, block2, block3);
      end if;
      return result;
   end convert;


   ---------------------------
   --  CONVERT FROM NBYTE4  --
   ---------------------------
   function convert (nv : nbyte4) return nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte4) return nbyte1
   is
      max : constant nbyte4 := nbyte4 (nbyte1'Last);
   begin
      case nv is
         when 0 .. max => return nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte4) return nbyte2
   is
      max : constant nbyte4 := nbyte4 (nbyte2'Last);
   begin
      case nv is
         when 0 .. max => return nbyte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte4) return nbyte3
   is
      max : constant nbyte4 := nbyte4 (nbyte3'Last);
   begin
      case nv is
         when 0 .. max => return nbyte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte4) return nbyte8 is
   begin
      return nbyte8 (nv);
   end convert;

   function convert (nv : nbyte4) return byte1
   is
      max : constant nbyte4 := nbyte4 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte1 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte4) return byte2
   is
      max : constant nbyte4 := nbyte4 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte2 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte4) return byte3
   is
      max : constant nbyte4 := nbyte4 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte3 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte4) return byte4
   is
      max : constant nbyte4 := nbyte4 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte4 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte4) return byte8 is
   begin
      return byte8 (nv);
   end convert;

   function convert (nv : nbyte4) return real9 is
   begin
      return real9 (nv);
   end convert;

   function convert (nv : nbyte4) return real18 is
   begin
      return real18 (nv);
   end convert;

   function convert (nv : nbyte4) return chain
   is
      use type nbyte4;
      result : chain (1 .. 4);
      block1 : constant nbyte1 := nbyte1 (nv and 16#FF#);
      block2 : constant nbyte1 := nbyte1 (BIT.Shift_Right
                        (BIT.Unsigned_32 (nv and 16#FF00#), 8));
      block3 : constant nbyte1 := nbyte1 (BIT.Shift_Right
                        (BIT.Unsigned_32 (nv and 16#FF0000#), 16));
      block4 : constant nbyte1 := nbyte1 (BIT.Shift_Right
                        (BIT.Unsigned_32 (nv and 16#FF000000#), 24));
   begin
      if Big_Endian then
         result := (block4, block3, block2, block1);
      else
         result := (block1, block2, block3, block4);
      end if;
      return result;
   end convert;


   ---------------------------
   --  CONVERT FROM NBYTE8  --
   ---------------------------
   function convert (nv : nbyte8) return nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte8) return nbyte1
   is
      max : constant nbyte8 := nbyte8 (nbyte1'Last);
   begin
      case nv is
         when 0 .. max => return nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte8) return nbyte2
   is
      max : constant nbyte8 := nbyte8 (nbyte2'Last);
   begin
      case nv is
         when 0 .. max => return nbyte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte8) return nbyte3
   is
      max : constant nbyte8 := nbyte8 (nbyte3'Last);
   begin
      case nv is
         when 0 .. max => return nbyte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte8) return nbyte4
   is
      max : constant nbyte8 := nbyte8 (nbyte4'Last);
   begin
      case nv is
         when 0 .. max => return nbyte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte8) return byte1
   is
      max : constant nbyte8 := nbyte8 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte1 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte8) return byte2
   is
      max : constant nbyte8 := nbyte8 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte2 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte8) return byte3
   is
      max : constant nbyte8 := nbyte8 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte3 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte8) return byte4
   is
      max : constant nbyte8 := nbyte8 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte4 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte8) return byte8
   is
      max : constant nbyte8 := nbyte8 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : nbyte8) return real9 is
   begin
      return real9 (nv);
   end convert;

   function convert (nv : nbyte8) return real18 is
   begin
      return real18 (nv);
   end convert;

   function convert (nv : nbyte8) return chain
   is
      use type nbyte8;
      result : chain (1 .. 8);
      b      : chain (1 .. 8);
   begin
      b (1) := nbyte1 (nv and 16#FF#);
      for s in 1 .. 7 loop
         declare
            use type BIT.Unsigned_64;
            shft : constant Natural := s * 8;
            mask : constant BIT.Unsigned_64 := BIT.Shift_Left (16#FF#, shft);
            slvr : constant BIT.Unsigned_64 := BIT.Unsigned_64 (nv) and mask;
         begin
            b (s + 1) := nbyte1 (BIT.Shift_Right (slvr, shft));
         end;
      end loop;
      if Big_Endian then
         result := (b (8), b (7), b (6), b (5), b (4), b (3), b (2), b (1));
      else
         result := b;
      end if;
      return result;
   end convert;


   ---------------------------
   --  CONVERT FROM BYTE1  --
   ---------------------------
   function convert (nv : byte1) return nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte1) return nbyte1 is
   begin
      case nv is
         when 0 .. byte1'Last => return nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte1) return nbyte2 is
   begin
      case nv is
         when 0 .. byte1'Last => return nbyte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte1) return nbyte3 is
   begin
      case nv is
         when 0 .. byte1'Last => return nbyte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte1) return nbyte4 is
   begin
      case nv is
         when 0 .. byte1'Last => return nbyte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte1) return nbyte8 is
   begin
      case nv is
         when 0 .. byte1'Last => return nbyte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte1) return byte2 is
   begin
      return byte2 (nv);
   end convert;

   function convert (nv : byte1) return byte3 is
   begin
      return byte3 (nv);
   end convert;

   function convert (nv : byte1) return byte4 is
   begin
      return byte4 (nv);
   end convert;

   function convert (nv : byte1) return byte8 is
   begin
      return byte8 (nv);
   end convert;

   function convert (nv : byte1) return real9 is
   begin
      return real9 (nv);
   end convert;

   function convert (nv : byte1) return real18 is
   begin
      return real18 (nv);
   end convert;


   ---------------------------
   --  CONVERT FROM BYTE2  --
   ---------------------------
   function convert (nv : byte2) return nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte2) return nbyte1
   is
      max : constant byte2 := byte2 (nbyte1'Last);
   begin
      case nv is
         when 0 .. max => return nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte2) return nbyte2 is
   begin
      case nv is
         when 0 .. byte2'Last => return nbyte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte2) return nbyte3 is
   begin
      case nv is
         when 0 .. byte2'Last => return nbyte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte2) return nbyte4 is
   begin
      case nv is
         when 0 .. byte2'Last => return nbyte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte2) return nbyte8 is
   begin
      case nv is
         when 0 .. byte2'Last => return nbyte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte2) return byte1
   is
      max : constant byte2 := byte2 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte2) return byte3 is
   begin
      return byte3 (nv);
   end convert;

   function convert (nv : byte2) return byte4 is
   begin
      return byte4 (nv);
   end convert;

   function convert (nv : byte2) return byte8 is
   begin
      return byte8 (nv);
   end convert;

   function convert (nv : byte2) return real9 is
   begin
      return real9 (nv);
   end convert;

   function convert (nv : byte2) return real18 is
   begin
      return real18 (nv);
   end convert;


   ---------------------------
   --  CONVERT FROM BYTE3  --
   ---------------------------
   function convert (nv : byte3) return nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte3) return nbyte1
   is
      max : constant byte3 := byte3 (nbyte1'Last);
   begin
      case nv is
         when 0 .. max => return nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte3) return nbyte2
   is
      max : constant byte3 := byte3 (nbyte2'Last);
   begin
      case nv is
         when 0 .. max => return nbyte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte3) return nbyte3 is
   begin
      case nv is
         when 0 .. byte3'Last => return nbyte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte3) return nbyte4 is
   begin
      case nv is
         when 0 .. byte3'Last => return nbyte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte3) return nbyte8 is
   begin
      case nv is
         when 0 .. byte3'Last => return nbyte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte3) return byte1
   is
      max : constant byte3 := byte3 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte3) return byte2
   is
      max : constant byte3 := byte3 (byte2'Last);
   begin
      case nv is
         when 0 .. max => return byte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte3) return byte4 is
   begin
      return byte4 (nv);
   end convert;

   function convert (nv : byte3) return byte8 is
   begin
      return byte8 (nv);
   end convert;

   function convert (nv : byte3) return real9 is
   begin
      return real9 (nv);
   end convert;

   function convert (nv : byte3) return real18 is
   begin
      return real18 (nv);
   end convert;


   ---------------------------
   --  CONVERT FROM BYTE4  --
   ---------------------------
   function convert (nv : byte4) return nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte4) return nbyte1
   is
      max : constant byte4 := byte4 (nbyte1'Last);
   begin
      case nv is
         when 0 .. max => return nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte4) return nbyte2
   is
      max : constant byte4 := byte4 (nbyte2'Last);
   begin
      case nv is
         when 0 .. max => return nbyte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte4) return nbyte3
   is
      max : constant byte4 := byte4 (nbyte3'Last);
   begin
      case nv is
         when 0 .. max => return nbyte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte4) return nbyte4 is
   begin
      case nv is
         when 0 .. byte4'Last => return nbyte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte4) return nbyte8 is
   begin
      case nv is
         when 0 .. byte4'Last => return nbyte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte4) return byte1
   is
      max : constant byte4 := byte4 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte4) return byte2
   is
      max : constant byte4 := byte4 (byte2'Last);
   begin
      case nv is
         when 0 .. max => return byte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte4) return byte3
   is
      max : constant byte4 := byte4 (byte3'Last);
   begin
      case nv is
         when 0 .. max => return byte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte4) return byte8 is
   begin
      return byte8 (nv);
   end convert;

   function convert (nv : byte4) return real9 is
   begin
      return real9 (nv);
   end convert;

   function convert (nv : byte4) return real18 is
   begin
      return real18 (nv);
   end convert;


   ---------------------------
   --  CONVERT FROM BYTE8  --
   ---------------------------
   function convert (nv : byte8) return nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte8) return nbyte1
   is
      max : constant byte8 := byte8 (nbyte1'Last);
   begin
      case nv is
         when 0 .. max => return nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte8) return nbyte2
   is
      max : constant byte8 := byte8 (nbyte2'Last);
   begin
      case nv is
         when 0 .. max => return nbyte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte8) return nbyte3
   is
      max : constant byte8 := byte8 (nbyte3'Last);
   begin
      case nv is
         when 0 .. max => return nbyte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte8) return nbyte4
   is
      max : constant byte8 := byte8 (nbyte4'Last);
   begin
      case nv is
         when 0 .. max => return nbyte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte8) return nbyte8 is
   begin
      case nv is
         when 0 .. byte8'Last => return nbyte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte8) return byte1
   is
      max : constant byte8 := byte8 (byte1'Last);
   begin
      case nv is
         when 0 .. max => return byte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte8) return byte2
   is
      max : constant byte8 := byte8 (byte2'Last);
   begin
      case nv is
         when 0 .. max => return byte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte8) return byte3
   is
      max : constant byte8 := byte8 (byte3'Last);
   begin
      case nv is
         when 0 .. max => return byte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte8) return byte4
   is
      max : constant byte8 := byte8 (byte4'Last);
   begin
      case nv is
         when 0 .. max => return byte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : byte8) return real9 is
   begin
      return real9 (nv);
   end convert;

   function convert (nv : byte8) return real18 is
   begin
      return real18 (nv);
   end convert;


   --------------------------
   --  CONVERT FROM REAL9  --
   --------------------------
   function convert (nv : real9) return real18 is
   begin
      return real18 (nv);
   end convert;


   ---------------------------
   --  CONVERT FROM REAL18  --
   ---------------------------
   function convert (nv : real18) return real9 is
   begin
      return real9 (nv);
   end convert;


   ---------------------------------
   --  CONVERT STRING TO BOOLEAN  --
   ---------------------------------
   function convert (nv : textual) return nbyte0
   is
      nvstr : constant String := CT.USS (nv);
   begin
      if nvstr = "0" then
         return False;
      end if;
      if nvstr = "1" then
         return True;
      end if;
      raise CONVERSION_FAILED with "Tried to convert '" & nvstr & "' (S)";
   end convert;

   function convert (nv : textwide) return nbyte0
   is
      nvstr : constant Wide_String := SUW.To_Wide_String (Source => nv);
      nverr : constant String   := ACC.To_String (Item => nvstr);
   begin
      if nverr = "0" then
         return False;
      end if;
      if nverr = "1" then
         return True;
      end if;
      raise CONVERSION_FAILED with "Tried to convert '" & nverr & "' (WS)";
   end convert;

   function convert (nv : textsuper) return nbyte0
   is
      nvstr : constant Wide_Wide_String := SUWW.To_Wide_Wide_String (nv);
      nverr : constant String := ACC.To_String (Item => nvstr);
   begin
      if nverr = "0" then
         return False;
      end if;
      if nverr = "1" then
         return True;
      end if;
      raise CONVERSION_FAILED with "Tried to convert '" & nverr & "' (WWS)";
   end convert;


   ---------------------------------
   --  CONVERT *STRING TO *STRING --
   ---------------------------------
   function convert (nv : textual) return String is
   begin
      return CT.USS (nv);
   end convert;

   function convert (nv : textual) return Wide_String
   is
      nvstr : constant String := CT.USS (nv);
   begin
      return ACC.To_Wide_String (Item => nvstr);
   end convert;

   function convert (nv : textual) return Wide_Wide_String
   is
      nvstr : constant String := CT.USS (nv);
   begin
      return ACC.To_Wide_Wide_String (Item => nvstr);
   end convert;

   function convert (nv : textwide) return String
   is
      nvstr : constant Wide_String := SUW.To_Wide_String (Source => nv);
   begin
      return ACC.To_String (Item => nvstr);
   end convert;

   function convert (nv : textwide) return Wide_String is
   begin
      return SUW.To_Wide_String (Source => nv);
   end convert;

   function convert (nv : textwide) return Wide_Wide_String
   is
      nvstr : constant Wide_String := SUW.To_Wide_String (Source => nv);
   begin
      return ACC.To_Wide_Wide_String (Item => nvstr);
   end convert;

   function convert (nv : textsuper) return String
   is
      nvstr : constant Wide_Wide_String :=
        SUWW.To_Wide_Wide_String (Source => nv);
   begin
      return ACC.To_String (Item => nvstr);
   end convert;

   function convert (nv : textsuper) return Wide_String
   is
      nvstr : constant Wide_Wide_String :=
        SUWW.To_Wide_Wide_String (Source => nv);
   begin
      return ACC.To_Wide_String (Item => nvstr);
   end convert;

   function convert (nv : textsuper) return Wide_Wide_String is
   begin
      return SUWW.To_Wide_Wide_String (Source => nv);
   end convert;


   --------------------------------------
   -- CONVERT TIME TO ISO 8601 STRING  --
   --------------------------------------
   function convert (nv : AC.Time) return String is
   begin
      return ACF.Image (Date => nv);
   end convert;

   function convert (nv : AC.Time) return Wide_String is
   begin
      return ACC.To_Wide_String (Item => ACF.Image (Date => nv));
   end convert;

   function convert (nv : AC.Time) return Wide_Wide_String is
   begin
      return ACC.To_Wide_Wide_String (Item => ACF.Image (Date => nv));
   end convert;


   ----------------------------------------------------
   -- ENUMERATION - return either integer or string  --
   ----------------------------------------------------
   function convert (nv : enumtype) return nbyte0 is
   begin
      case nv.index is
         when 0 => return False;
         when 1 => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : enumtype) return nbyte1
   is
      max : constant Natural := Natural (nbyte1'Last);
   begin
      case nv.index is
         when 0 .. max => return nbyte1 (nv.index);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : enumtype) return nbyte2
   is
      max : constant Natural := Natural (nbyte2'Last);
   begin
      case nv.index is
         when 0 .. max => return nbyte2 (nv.index);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : enumtype) return nbyte3
   is
      max : constant Natural := Natural (nbyte3'Last);
   begin
      case nv.index is
         when 0 .. max => return nbyte3 (nv.index);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : enumtype) return nbyte4 is
   begin
      return nbyte4 (nv.index);
   end convert;

   function convert (nv : enumtype) return nbyte8 is
   begin
      return nbyte8 (nv.index);
   end convert;

   function convert (nv : enumtype) return byte1
   is
      max : constant Natural := Natural (byte1'Last);
   begin
      case nv.index is
         when 0 .. max => return byte1 (nv.index);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : enumtype) return byte2
   is
      max : constant Natural := Natural (byte2'Last);
   begin
      case nv.index is
         when 0 .. max => return byte2 (nv.index);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : enumtype) return byte3
   is
      max : constant Natural := Natural (byte3'Last);
   begin
      case nv.index is
         when 0 .. max => return byte3 (nv.index);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : enumtype) return byte4
   is
      max : constant Natural := Natural (byte4'Last);
   begin
      case nv.index is
         when 0 .. max => return byte4 (nv.index);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : enumtype) return byte8 is
   begin
      return byte8 (nv.index);
   end convert;

   function convert (nv : enumtype) return String is
   begin
      return CT.USS (nv.enumeration);
   end convert;

   function convert (nv : enumtype) return Wide_String
   is
      str : constant String := CT.USS (nv.enumeration);
   begin
      return ACC.To_Wide_String (Item => str);
   end convert;

   function convert (nv : enumtype) return Wide_Wide_String
   is
      str : constant String := CT.USS (nv.enumeration);
   begin
      return ACC.To_Wide_Wide_String (Item => str);
   end convert;

   ------------------------
   --  CHAIN (OF BYTES)  --
   ------------------------
   function convert (nv : chain) return nbyte0 is
   begin
      if nv'Length > 1 then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      case nv (1) is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : chain) return nbyte1 is
   begin
      if nv'Length > 1 then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      return nv (1);
   end convert;

   function convert (nv : chain) return nbyte2
   is
      use type nbyte2;
      cn : chain (1 .. 2) := (others => 0);
   begin
      if nv'Length > 2 then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      for n in 1 .. nv'Length loop
         cn (n) := nv (n);
      end loop;
      if Big_Endian then
         return
           nbyte2 (BIT.Shift_Left (BIT.Unsigned_16 (cn (1)), 8)) +
           nbyte2 (cn (2));
      else
         return
           nbyte2 (BIT.Shift_Left (BIT.Unsigned_16 (cn (2)), 8)) +
           nbyte2 (cn (1));
      end if;
   end convert;

   function convert (nv : chain) return nbyte3
   is
      use type nbyte3;
      cn : chain (1 .. 3) := (others => 0);
   begin
      if nv'Length > 3 then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      for n in 1 .. nv'Length loop
         cn (n) := nv (n);
      end loop;
      if Big_Endian then
         return
           nbyte3 (BIT.Shift_Left (BIT.Unsigned_32 (cn (1)), 16)) +
           nbyte3 (BIT.Shift_Left (BIT.Unsigned_16 (cn (2)), 8)) +
           nbyte3 (cn (3));
      else
         return
           nbyte3 (BIT.Shift_Left (BIT.Unsigned_32 (cn (3)), 16)) +
           nbyte3 (BIT.Shift_Left (BIT.Unsigned_16 (cn (2)), 8)) +
           nbyte3 (cn (1));
      end if;
   end convert;

   function convert (nv : chain) return nbyte4
   is
      use type nbyte4;
      cn : chain (1 .. 4) := (others => 0);
   begin
      if nv'Length > 4 then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      for n in 1 .. nv'Length loop
         cn (n) := nv (n);
      end loop;
      if Big_Endian then
         return
           nbyte4 (BIT.Shift_Left (BIT.Unsigned_32 (cn (1)), 24)) +
           nbyte4 (BIT.Shift_Left (BIT.Unsigned_32 (cn (2)), 16)) +
           nbyte4 (BIT.Shift_Left (BIT.Unsigned_16 (cn (3)), 8)) +
           nbyte4 (cn (4));
      else
         return
           nbyte4 (BIT.Shift_Left (BIT.Unsigned_32 (cn (4)), 24)) +
           nbyte4 (BIT.Shift_Left (BIT.Unsigned_32 (cn (3)), 16)) +
           nbyte4 (BIT.Shift_Left (BIT.Unsigned_16 (cn (2)), 8)) +
           nbyte4 (cn (1));
      end if;
   end convert;

   function convert (nv : chain) return nbyte8
   is
      use type nbyte8;
      cn : chain (1 .. 8) := (others => 0);
   begin
      if nv'Length > 8 then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      for n in 1 .. nv'Length loop
         cn (n) := nv (n);
      end loop;
      if Big_Endian then
         return
           nbyte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (1)), 56)) +
           nbyte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (2)), 48)) +
           nbyte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (3)), 40)) +
           nbyte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (4)), 32)) +
           nbyte8 (BIT.Shift_Left (BIT.Unsigned_32 (cn (5)), 24)) +
           nbyte8 (BIT.Shift_Left (BIT.Unsigned_32 (cn (6)), 16)) +
           nbyte8 (BIT.Shift_Left (BIT.Unsigned_16 (cn (7)), 8)) +
           nbyte8 (cn (8));
      else
         return
           nbyte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (8)), 56)) +
           nbyte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (7)), 48)) +
           nbyte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (6)), 40)) +
           nbyte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (5)), 32)) +
           nbyte8 (BIT.Shift_Left (BIT.Unsigned_32 (cn (4)), 24)) +
           nbyte8 (BIT.Shift_Left (BIT.Unsigned_32 (cn (3)), 16)) +
           nbyte8 (BIT.Shift_Left (BIT.Unsigned_16 (cn (2)), 8)) +
           nbyte8 (cn (1));
      end if;
   end convert;



   ---------------------------------------
   -- CONVERT SETS TO STRING (implode)  --
   ---------------------------------------
   function convert (nv : settype_access) return String
   is
      len    : Natural := 0;
      nvlen  : Natural := nv.all'Length;
   begin
      if nvlen = 0 then
         return "";
      end if;
      for x in 1 .. nvlen loop
         len := len + CT.SU.Length (nv.all (x).enumeration);
      end loop;
      len := len + nvlen - 1;
      for x in 1 .. nvlen loop
         len := len + CT.SU.Length (nv.all (x).enumeration);
      end loop;
      declare
         cursor : Natural := 1;
         curend : Natural;
         result : String (1 .. len) := (others => ',');
      begin
         for x in 1 .. nvlen loop
            curend := cursor - 1 + CT.SU.Length (nv.all (x).enumeration);
            result (cursor .. curend) := CT.USS (nv.all (x).enumeration);
            cursor := curend + 2;
         end loop;
         return result;
      end;
   end convert;

   function convert (nv : settype_access) return Wide_String
   is
      preview : String := convert (nv);
   begin
      return ACC.To_Wide_String (preview);
   end convert;

   function convert (nv : settype_access) return Wide_Wide_String
   is
      preview : String := convert (nv);
   begin
      return ACC.To_Wide_Wide_String (preview);
   end convert;


end AdaBase.Results.Converters;
