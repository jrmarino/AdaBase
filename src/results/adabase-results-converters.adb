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
   function convert (nv : NByte0) return NByte1 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : NByte0) return NByte2 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : NByte0) return NByte3 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : NByte0) return NByte4 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : NByte0) return NByte8 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : NByte0) return Byte1 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : NByte0) return Byte2 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : NByte0) return Byte3 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : NByte0) return Byte4 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : NByte0) return Byte8 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : NByte0) return Real9 is
   begin
      case nv is
         when True  => return Real9 (1);
         when False => return Real9 (0);
      end case;
   end convert;

   function convert (nv : NByte0) return Real18 is
   begin
      case nv is
         when True  => return Real18 (1);
         when False => return Real18 (0);
      end case;
   end convert;

   function convert (nv : NByte0) return String is
   begin
      case nv is
         when True  => return "1";
         when False => return "0";
      end case;
   end convert;

   function convert (nv : NByte0) return Wide_String is
   begin
      case nv is
         when True  => return "1";
         when False => return "0";
      end case;
   end convert;

   function convert (nv : NByte0) return Wide_Wide_String is
   begin
      case nv is
         when True  => return "1";
         when False => return "0";
      end case;
   end convert;

   function convert (nv : NByte0) return Chain
   is
     result : Chain (1 .. 1) := (others => 0);
   begin
      if nv then
         result (1) := 1;
      end if;
      return result;
   end convert;


   ---------------------------
   --  CONVERT FROM NBYTE1  --
   ---------------------------
   function convert (nv : NByte1) return NByte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte1) return NByte2 is
   begin
      return NByte2 (nv);
   end convert;

   function convert (nv : NByte1) return NByte3 is
   begin
      return NByte3 (nv);
   end convert;

   function convert (nv : NByte1) return NByte4 is
   begin
      return NByte4 (nv);
   end convert;

   function convert (nv : NByte1) return NByte8 is
   begin
      return NByte8 (nv);
   end convert;

   function convert (nv : NByte1) return Byte1
   is
      max : constant NByte1 := NByte1 (Byte1'Last);
   begin
      case nv is
         when 0 .. max => return Byte1 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte1) return Byte2 is
   begin
      return Byte2 (nv);
   end convert;

   function convert (nv : NByte1) return Byte3 is
   begin
      return Byte3 (nv);
   end convert;

   function convert (nv : NByte1) return Byte4 is
   begin
      return Byte4 (nv);
   end convert;

   function convert (nv : NByte1) return Byte8 is
   begin
      return Byte8 (nv);
   end convert;

   function convert (nv : NByte1) return Real9 is
   begin
      return Real9 (nv);
   end convert;

   function convert (nv : NByte1) return Real18 is
   begin
      return Real18 (nv);
   end convert;

   function convert (nv : NByte1) return Chain
   is
      result : constant Chain (1 .. 1) := (1 => nv);
   begin
      return result;
   end convert;


   ---------------------------
   --  CONVERT FROM NBYTE2  --
   ---------------------------
   function convert (nv : NByte2) return NByte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte2) return NByte1
   is
      max : constant NByte2 := NByte2 (NByte1'Last);
   begin
      case nv is
         when 0 .. max => return NByte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte2) return NByte3 is
   begin
      return NByte3 (nv);
   end convert;

   function convert (nv : NByte2) return NByte4 is
   begin
      return NByte4 (nv);
   end convert;

   function convert (nv : NByte2) return NByte8 is
   begin
      return NByte8 (nv);
   end convert;

   function convert (nv : NByte2) return Byte1
   is
      max : constant NByte2 := NByte2 (Byte1'Last);
   begin
      case nv is
         when 0 .. max => return Byte1 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte2) return Byte2
   is
      max : constant NByte2 := NByte2 (Byte1'Last);
   begin
      case nv is
         when 0 .. max => return Byte2 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte2) return Byte3 is
   begin
      return Byte3 (nv);
   end convert;

   function convert (nv : NByte2) return Byte4 is
   begin
      return Byte4 (nv);
   end convert;

   function convert (nv : NByte2) return Byte8 is
   begin
      return Byte8 (nv);
   end convert;

   function convert (nv : NByte2) return Real9 is
   begin
      return Real9 (nv);
   end convert;

   function convert (nv : NByte2) return Real18 is
   begin
      return Real18 (nv);
   end convert;

   function convert (nv : NByte2) return Chain
   is
      use type NByte2;
      result : Chain (1 .. 2);
      block1 : constant NByte1 := NByte1 (nv and 16#FF#);
      block2 : constant NByte1 := NByte1
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
   function convert (nv : NByte3) return NByte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte3) return NByte1
   is
      max : constant NByte3 := NByte3 (NByte1'Last);
   begin
      case nv is
         when 0 .. max => return NByte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte3) return NByte2
   is
      max : constant NByte3 := NByte3 (NByte2'Last);
   begin
      case nv is
         when 0 .. max => return NByte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte3) return NByte4 is
   begin
      return NByte4 (nv);
   end convert;

   function convert (nv : NByte3) return NByte8 is
   begin
      return NByte8 (nv);
   end convert;

   function convert (nv : NByte3) return Byte1
   is
      max : constant NByte3 := NByte3 (Byte1'Last);
   begin
      case nv is
         when 0 .. max => return Byte1 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte3) return Byte2
   is
      max : constant NByte3 := NByte3 (Byte1'Last);
   begin
      case nv is
         when 0 .. max => return Byte2 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte3) return Byte3
   is
      max : constant NByte3 := NByte3 (Byte1'Last);
   begin
      case nv is
         when 0 .. max => return Byte3 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte3) return Byte4 is
   begin
      return Byte4 (nv);
   end convert;

   function convert (nv : NByte3) return Byte8 is
   begin
      return Byte8 (nv);
   end convert;

   function convert (nv : NByte3) return Real9 is
   begin
      return Real9 (nv);
   end convert;

   function convert (nv : NByte3) return Real18 is
   begin
      return Real18 (nv);
   end convert;

   function convert (nv : NByte3) return Chain
   is
      use type NByte3;
      result : Chain (1 .. 3);
      block1 : constant NByte1 := NByte1 (nv and 16#FF#);
      block2 : constant NByte1 := NByte1 (BIT.Shift_Right
                        (BIT.Unsigned_32 (nv and 16#FF00#), 8));
      block3 : constant NByte1 := NByte1 (BIT.Shift_Right
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
   function convert (nv : NByte4) return NByte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte4) return NByte1
   is
      max : constant NByte4 := NByte4 (NByte1'Last);
   begin
      case nv is
         when 0 .. max => return NByte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte4) return NByte2
   is
      max : constant NByte4 := NByte4 (NByte2'Last);
   begin
      case nv is
         when 0 .. max => return NByte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte4) return NByte3
   is
      max : constant NByte4 := NByte4 (NByte3'Last);
   begin
      case nv is
         when 0 .. max => return NByte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte4) return NByte8 is
   begin
      return NByte8 (nv);
   end convert;

   function convert (nv : NByte4) return Byte1
   is
      max : constant NByte4 := NByte4 (Byte1'Last);
   begin
      case nv is
         when 0 .. max => return Byte1 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte4) return Byte2
   is
      max : constant NByte4 := NByte4 (Byte1'Last);
   begin
      case nv is
         when 0 .. max => return Byte2 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte4) return Byte3
   is
      max : constant NByte4 := NByte4 (Byte1'Last);
   begin
      case nv is
         when 0 .. max => return Byte3 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte4) return Byte4
   is
      max : constant NByte4 := NByte4 (Byte1'Last);
   begin
      case nv is
         when 0 .. max => return Byte4 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte4) return Byte8 is
   begin
      return Byte8 (nv);
   end convert;

   function convert (nv : NByte4) return Real9 is
   begin
      return Real9 (nv);
   end convert;

   function convert (nv : NByte4) return Real18 is
   begin
      return Real18 (nv);
   end convert;

   function convert (nv : NByte4) return Chain
   is
      use type NByte4;
      result : Chain (1 .. 4);
      block1 : constant NByte1 := NByte1 (nv and 16#FF#);
      block2 : constant NByte1 := NByte1 (BIT.Shift_Right
                        (BIT.Unsigned_32 (nv and 16#FF00#), 8));
      block3 : constant NByte1 := NByte1 (BIT.Shift_Right
                        (BIT.Unsigned_32 (nv and 16#FF0000#), 16));
      block4 : constant NByte1 := NByte1 (BIT.Shift_Right
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
   function convert (nv : NByte8) return NByte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte8) return NByte1
   is
      max : constant NByte8 := NByte8 (NByte1'Last);
   begin
      case nv is
         when 0 .. max => return NByte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte8) return NByte2
   is
      max : constant NByte8 := NByte8 (NByte2'Last);
   begin
      case nv is
         when 0 .. max => return NByte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte8) return NByte3
   is
      max : constant NByte8 := NByte8 (NByte3'Last);
   begin
      case nv is
         when 0 .. max => return NByte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte8) return NByte4
   is
      max : constant NByte8 := NByte8 (NByte4'Last);
   begin
      case nv is
         when 0 .. max => return NByte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte8) return Byte1
   is
      max : constant NByte8 := NByte8 (Byte1'Last);
   begin
      case nv is
         when 0 .. max => return Byte1 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte8) return Byte2
   is
      max : constant NByte8 := NByte8 (Byte2'Last);
   begin
      case nv is
         when 0 .. max => return Byte2 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte8) return Byte3
   is
      max : constant NByte8 := NByte8 (Byte3'Last);
   begin
      case nv is
         when 0 .. max => return Byte3 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte8) return Byte4
   is
      max : constant NByte8 := NByte8 (Byte4'Last);
   begin
      case nv is
         when 0 .. max => return Byte4 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte8) return Byte8
   is
      max : constant NByte8 := NByte8 (Byte8'Last);
   begin
      case nv is
         when 0 .. max => return Byte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : NByte8) return Real9 is
   begin
      return Real9 (nv);
   end convert;

   function convert (nv : NByte8) return Real18 is
   begin
      return Real18 (nv);
   end convert;

   function convert (nv : NByte8) return Chain
   is
      use type NByte8;
      result : Chain (1 .. 8);
      b      : Chain (1 .. 8);
   begin
      b (1) := NByte1 (nv and 16#FF#);
      for s in 1 .. 7 loop
         declare
            use type BIT.Unsigned_64;
            shft : constant Natural := s * 8;
            mask : constant BIT.Unsigned_64 := BIT.Shift_Left (16#FF#, shft);
            slvr : constant BIT.Unsigned_64 := BIT.Unsigned_64 (nv) and mask;
         begin
            b (s + 1) := NByte1 (BIT.Shift_Right (slvr, shft));
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
   function convert (nv : Byte1) return NByte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte1) return NByte1 is
   begin
      case nv is
         when 0 .. Byte1'Last => return NByte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte1) return NByte2 is
   begin
      case nv is
         when 0 .. Byte1'Last => return NByte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte1) return NByte3 is
   begin
      case nv is
         when 0 .. Byte1'Last => return NByte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte1) return NByte4 is
   begin
      case nv is
         when 0 .. Byte1'Last => return NByte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte1) return NByte8 is
   begin
      case nv is
         when 0 .. Byte1'Last => return NByte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte1) return Byte2 is
   begin
      return Byte2 (nv);
   end convert;

   function convert (nv : Byte1) return Byte3 is
   begin
      return Byte3 (nv);
   end convert;

   function convert (nv : Byte1) return Byte4 is
   begin
      return Byte4 (nv);
   end convert;

   function convert (nv : Byte1) return Byte8 is
   begin
      return Byte8 (nv);
   end convert;

   function convert (nv : Byte1) return Real9 is
   begin
      return Real9 (nv);
   end convert;

   function convert (nv : Byte1) return Real18 is
   begin
      return Real18 (nv);
   end convert;


   ---------------------------
   --  CONVERT FROM BYTE2  --
   ---------------------------
   function convert (nv : Byte2) return NByte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte2) return NByte1
   is
      max : constant Byte2 := Byte2 (NByte1'Last);
   begin
      case nv is
         when 0 .. max => return NByte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte2) return NByte2 is
   begin
      case nv is
         when 0 .. Byte2'Last => return NByte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte2) return NByte3 is
   begin
      case nv is
         when 0 .. Byte2'Last => return NByte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte2) return NByte4 is
   begin
      case nv is
         when 0 .. Byte2'Last => return NByte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte2) return NByte8 is
   begin
      case nv is
         when 0 .. Byte2'Last => return NByte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte2) return Byte1
   is
      max : constant Byte2 := Byte2 (Byte1'Last);
   begin
      case nv is
         when 0 .. max => return Byte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte2) return Byte3 is
   begin
      return Byte3 (nv);
   end convert;

   function convert (nv : Byte2) return Byte4 is
   begin
      return Byte4 (nv);
   end convert;

   function convert (nv : Byte2) return Byte8 is
   begin
      return Byte8 (nv);
   end convert;

   function convert (nv : Byte2) return Real9 is
   begin
      return Real9 (nv);
   end convert;

   function convert (nv : Byte2) return Real18 is
   begin
      return Real18 (nv);
   end convert;


   ---------------------------
   --  CONVERT FROM BYTE3  --
   ---------------------------
   function convert (nv : Byte3) return NByte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte3) return NByte1
   is
      max : constant Byte3 := Byte3 (NByte1'Last);
   begin
      case nv is
         when 0 .. max => return NByte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte3) return NByte2
   is
      max : constant Byte3 := Byte3 (NByte2'Last);
   begin
      case nv is
         when 0 .. max => return NByte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte3) return NByte3 is
   begin
      case nv is
         when 0 .. Byte3'Last => return NByte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte3) return NByte4 is
   begin
      case nv is
         when 0 .. Byte3'Last => return NByte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte3) return NByte8 is
   begin
      case nv is
         when 0 .. Byte3'Last => return NByte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte3) return Byte1
   is
      max : constant Byte3 := Byte3 (Byte1'Last);
   begin
      case nv is
         when 0 .. max => return Byte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte3) return Byte2
   is
      max : constant Byte3 := Byte3 (Byte2'Last);
   begin
      case nv is
         when 0 .. max => return Byte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte3) return Byte4 is
   begin
      return Byte4 (nv);
   end convert;

   function convert (nv : Byte3) return Byte8 is
   begin
      return Byte8 (nv);
   end convert;

   function convert (nv : Byte3) return Real9 is
   begin
      return Real9 (nv);
   end convert;

   function convert (nv : Byte3) return Real18 is
   begin
      return Real18 (nv);
   end convert;


   ---------------------------
   --  CONVERT FROM BYTE4  --
   ---------------------------
   function convert (nv : Byte4) return NByte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte4) return NByte1
   is
      max : constant Byte4 := Byte4 (NByte1'Last);
   begin
      case nv is
         when 0 .. max => return NByte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte4) return NByte2
   is
      max : constant Byte4 := Byte4 (NByte2'Last);
   begin
      case nv is
         when 0 .. max => return NByte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte4) return NByte3
   is
      max : constant Byte4 := Byte4 (NByte3'Last);
   begin
      case nv is
         when 0 .. max => return NByte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte4) return NByte4 is
   begin
      case nv is
         when 0 .. Byte4'Last => return NByte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte4) return NByte8 is
   begin
      case nv is
         when 0 .. Byte4'Last => return NByte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte4) return Byte1
   is
      max : constant Byte4 := Byte4 (Byte1'Last);
   begin
      case nv is
         when 0 .. max => return Byte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte4) return Byte2
   is
      max : constant Byte4 := Byte4 (Byte2'Last);
   begin
      case nv is
         when 0 .. max => return Byte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte4) return Byte3
   is
      max : constant Byte4 := Byte4 (Byte3'Last);
   begin
      case nv is
         when 0 .. max => return Byte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte4) return Byte8 is
   begin
      return Byte8 (nv);
   end convert;

   function convert (nv : Byte4) return Real9 is
   begin
      return Real9 (nv);
   end convert;

   function convert (nv : Byte4) return Real18 is
   begin
      return Real18 (nv);
   end convert;


   ---------------------------
   --  CONVERT FROM BYTE8  --
   ---------------------------
   function convert (nv : Byte8) return NByte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte8) return NByte1
   is
      max : constant Byte8 := Byte8 (NByte1'Last);
   begin
      case nv is
         when 0 .. max => return NByte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte8) return NByte2
   is
      max : constant Byte8 := Byte8 (NByte2'Last);
   begin
      case nv is
         when 0 .. max => return NByte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte8) return NByte3
   is
      max : constant Byte8 := Byte8 (NByte3'Last);
   begin
      case nv is
         when 0 .. max => return NByte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte8) return NByte4
   is
      max : constant Byte8 := Byte8 (NByte4'Last);
   begin
      case nv is
         when 0 .. max => return NByte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte8) return NByte8 is
   begin
      case nv is
         when 0 .. Byte8'Last => return NByte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte8) return Byte1
   is
      min : constant Byte8 := Byte8 (Byte1'First);
      max : constant Byte8 := Byte8 (Byte1'Last);
   begin
      case nv is
         when min .. max => return Byte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte8) return Byte2
   is
      min : constant Byte8 := Byte8 (Byte2'First);
      max : constant Byte8 := Byte8 (Byte2'Last);
   begin
      case nv is
         when min .. max => return Byte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte8) return Byte3
   is
      min : constant Byte8 := Byte8 (Byte3'First);
      max : constant Byte8 := Byte8 (Byte3'Last);
   begin
      case nv is
         when min .. max => return Byte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte8) return Byte4
   is
      min : constant Byte8 := Byte8 (Byte4'First);
      max : constant Byte8 := Byte8 (Byte4'Last);
   begin
      case nv is
         when min .. max => return Byte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : Byte8) return Real9 is
   begin
      return Real9 (nv);
   end convert;

   function convert (nv : Byte8) return Real18 is
   begin
      return Real18 (nv);
   end convert;


   --------------------------
   --  CONVERT FROM REAL9  --
   --------------------------
   function convert (nv : Real9) return Real18 is
   begin
      return Real18 (nv);
   end convert;


   ---------------------------
   --  CONVERT FROM REAL18  --
   ---------------------------
   function convert (nv : Real18) return Real9 is
   begin
      return Real9 (nv);
   end convert;


   ---------------------------------
   --  CONVERT STRING TO BOOLEAN  --
   ---------------------------------
   function convert (nv : Textual) return NByte0
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

   function convert (nv : Textwide) return NByte0
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

   function convert (nv : Textsuper) return NByte0
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


   -------------------------------
   --  CONVERT FROM *STRING TO  --
   -------------------------------
   function convert (nv : Textual) return String is
   begin
      return CT.USS (nv);
   end convert;

   function convert (nv : Textual) return Wide_String
   is
      nvstr : constant String := CT.USS (nv);
   begin
      return ACC.To_Wide_String (Item => nvstr);
   end convert;

   function convert (nv : Textual) return Wide_Wide_String
   is
      nvstr : constant String := CT.USS (nv);
   begin
      return ACC.To_Wide_Wide_String (Item => nvstr);
   end convert;

   function convert (nv : String; fixed : Natural := 0) return Chain
   is
      Chainlen : Natural := CT.len (nv);
   begin
      if fixed > Chainlen then
         Chainlen := fixed;
      end if;
      declare
         result : Chain (1 .. Chainlen) := (others => 0);
         arrow  : Natural := result'First;
      begin
         for x in nv'Range loop
            result (arrow) := NByte1 (Character'Pos (nv (x)));
            arrow := arrow + 1;
         end loop;
         return result;
      end;
   end convert;

   function convert (nv : String) return Enumtype
   is
      result : Enumtype;
   begin
      result.enumeration := CT.SUS (nv);
      return result;
   end convert;

   function convert (nv : Textual) return Enumtype
   is
      result : Enumtype;
   begin
      result.enumeration := nv;
      return result;
   end convert;

   function convert (nv : Textual) return Chain is
   begin
      return convert (CT.USS (nv));
   end convert;

   function convert (nv : Textual) return AC.Time is
   begin
      return convert (CT.USS (nv));
   end convert;

   function convert (nv : Textual) return Settype is
   begin
      return convert (CT.USS (nv));
   end convert;

   function convert (nv : Textwide) return String
   is
      nvstr : constant Wide_String := SUW.To_Wide_String (Source => nv);
   begin
      return ACC.To_String (Item => nvstr);
   end convert;

   function convert (nv : Textwide) return Wide_String is
   begin
      return SUW.To_Wide_String (Source => nv);
   end convert;

   function convert (nv : Textwide) return Wide_Wide_String
   is
      nvstr : constant Wide_String := SUW.To_Wide_String (Source => nv);
   begin
      return ACC.To_Wide_Wide_String (Item => nvstr);
   end convert;

   function convert (nv : Textwide) return Chain
   is
      nvstr : constant String := convert (nv);
   begin
      return convert (nvstr);
   end convert;

   function convert (nv : Textwide) return AC.Time
   is
      nvstr : constant String := convert (nv);
   begin
      return convert (nvstr);
   end convert;

   function convert (nv : Textwide) return Settype
   is
      nvstr : constant String := convert (nv);
   begin
      return convert (nvstr);
   end convert;

   function convert (nv : Textwide) return Enumtype
   is
      result : Enumtype;
      enum : String := convert (nv);
   begin
      result.enumeration := CT.SUS (enum);
      return result;
   end convert;

   function convert (nv : Textsuper) return String
   is
      nvstr : constant Wide_Wide_String :=
        SUWW.To_Wide_Wide_String (Source => nv);
   begin
      return ACC.To_String (Item => nvstr);
   end convert;

   function convert (nv : Textsuper) return Wide_String
   is
      nvstr : constant Wide_Wide_String :=
        SUWW.To_Wide_Wide_String (Source => nv);
   begin
      return ACC.To_Wide_String (Item => nvstr);
   end convert;

   function convert (nv : Textsuper) return Wide_Wide_String is
   begin
      return SUWW.To_Wide_Wide_String (Source => nv);
   end convert;

   function convert (nv : Textsuper) return Chain
   is
      nvstr : constant String := convert (nv);
   begin
      return convert (nvstr);
   end convert;

   function convert (nv : Textsuper) return AC.Time
   is
      nvstr : constant String := convert (nv);
   begin
      return convert (nvstr);
   end convert;

   function convert (nv : Textsuper) return Settype
   is
      nvstr : constant String := convert (nv);
   begin
      return convert (nvstr);
   end convert;

   function convert (nv : Textsuper) return Enumtype
   is
      result : Enumtype;
      enum : String := convert (nv);
   begin
      result.enumeration := CT.SUS (enum);
      return result;
   end convert;


   --------------------------------------
   -- CONVERT TIME TO ISO 8601 STRING  --
   --------------------------------------
   function convert (nv : AC.Time) return String is
   begin
      return ACF.Image (Date => nv);
   end convert;

   function convert (nv : AC.Time) return Wide_String
   is
      timestr : constant String := convert (nv);
   begin
      return ACC.To_Wide_String (Item => timestr);
   end convert;

   function convert (nv : AC.Time) return Wide_Wide_String
   is
      timestr : constant String := convert (nv);
   begin
      return ACC.To_Wide_Wide_String (Item => timestr);
   end convert;


   ---------------------------------------------------
   -- ENUMERATION - limited to string converstions  --
   ---------------------------------------------------
   function convert (nv : Enumtype) return String is
   begin
      return CT.USS (nv.enumeration);
   end convert;

   function convert (nv : Enumtype) return Wide_String
   is
      str : constant String := CT.USS (nv.enumeration);
   begin
      return ACC.To_Wide_String (Item => str);
   end convert;

   function convert (nv : Enumtype) return Wide_Wide_String
   is
      str : constant String := CT.USS (nv.enumeration);
   begin
      return ACC.To_Wide_Wide_String (Item => str);
   end convert;


   ------------------------
   --  CHAIN (OF BYTES)  --
   ------------------------
   function convert (nv : Chain) return NByte0 is
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

   function convert (nv : Chain) return NByte1 is
   begin
      if nv'Length > 1 then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      return nv (1);
   end convert;

   function convert (nv : Chain) return NByte2
   is
      use type NByte2;
      cn : Chain (1 .. 2) := (others => 0);
   begin
      if nv'Length > 2 then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      for n in 1 .. nv'Length loop
         cn (n) := nv (n);
      end loop;
      if Big_Endian then
         return
           NByte2 (BIT.Shift_Left (BIT.Unsigned_16 (cn (1)), 8)) +
           NByte2 (cn (2));
      else
         return
           NByte2 (BIT.Shift_Left (BIT.Unsigned_16 (cn (2)), 8)) +
           NByte2 (cn (1));
      end if;
   end convert;

   function convert (nv : Chain) return NByte3
   is
      use type NByte3;
      cn : Chain (1 .. 3) := (others => 0);
   begin
      if nv'Length > 3 then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      for n in 1 .. nv'Length loop
         cn (n) := nv (n);
      end loop;
      if Big_Endian then
         return
           NByte3 (BIT.Shift_Left (BIT.Unsigned_32 (cn (1)), 16)) +
           NByte3 (BIT.Shift_Left (BIT.Unsigned_16 (cn (2)), 8)) +
           NByte3 (cn (3));
      else
         return
           NByte3 (BIT.Shift_Left (BIT.Unsigned_32 (cn (3)), 16)) +
           NByte3 (BIT.Shift_Left (BIT.Unsigned_16 (cn (2)), 8)) +
           NByte3 (cn (1));
      end if;
   end convert;

   function convert (nv : Chain) return NByte4
   is
      use type NByte4;
      cn : Chain (1 .. 4) := (others => 0);
   begin
      if nv'Length > 4 then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      for n in 1 .. nv'Length loop
         cn (n) := nv (n);
      end loop;
      if Big_Endian then
         return
           NByte4 (BIT.Shift_Left (BIT.Unsigned_32 (cn (1)), 24)) +
           NByte4 (BIT.Shift_Left (BIT.Unsigned_32 (cn (2)), 16)) +
           NByte4 (BIT.Shift_Left (BIT.Unsigned_16 (cn (3)), 8)) +
           NByte4 (cn (4));
      else
         return
           NByte4 (BIT.Shift_Left (BIT.Unsigned_32 (cn (4)), 24)) +
           NByte4 (BIT.Shift_Left (BIT.Unsigned_32 (cn (3)), 16)) +
           NByte4 (BIT.Shift_Left (BIT.Unsigned_16 (cn (2)), 8)) +
           NByte4 (cn (1));
      end if;
   end convert;

   function convert (nv : Chain) return NByte8
   is
      use type NByte8;
      cn : Chain (1 .. 8) := (others => 0);
   begin
      if nv'Length > 8 then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      for n in 1 .. nv'Length loop
         cn (n) := nv (n);
      end loop;
      if Big_Endian then
         return
           NByte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (1)), 56)) +
           NByte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (2)), 48)) +
           NByte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (3)), 40)) +
           NByte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (4)), 32)) +
           NByte8 (BIT.Shift_Left (BIT.Unsigned_32 (cn (5)), 24)) +
           NByte8 (BIT.Shift_Left (BIT.Unsigned_32 (cn (6)), 16)) +
           NByte8 (BIT.Shift_Left (BIT.Unsigned_16 (cn (7)), 8)) +
           NByte8 (cn (8));
      else
         return
           NByte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (8)), 56)) +
           NByte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (7)), 48)) +
           NByte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (6)), 40)) +
           NByte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (5)), 32)) +
           NByte8 (BIT.Shift_Left (BIT.Unsigned_32 (cn (4)), 24)) +
           NByte8 (BIT.Shift_Left (BIT.Unsigned_32 (cn (3)), 16)) +
           NByte8 (BIT.Shift_Left (BIT.Unsigned_16 (cn (2)), 8)) +
           NByte8 (cn (1));
      end if;
   end convert;

   function convert (nv : Chain) return String
   is
      payload : String (nv'Range);
   begin
      for x in nv'Range loop
         payload (x) := Character'Val (nv (x));
      end loop;
      return payload;
   end convert;

   function convert (nv : Chain) return Wide_String
   is
      preview : String := convert (nv);
   begin
      return ACC.To_Wide_String (preview);
   end convert;

   function convert (nv : Chain) return Wide_Wide_String
   is
      preview : String := convert (nv);
   begin
      return ACC.To_Wide_Wide_String (preview);
   end convert;


   ---------------------------------------
   -- CONVERT SETS TO STRING (implode)  --
   ---------------------------------------
   function convert (nv : Settype) return String
   is
      len    : Natural := 0;
      nvlen  : Natural := nv'Length;
   begin
      if nvlen = 0 then
         return blankstring;
      end if;
      for x in 1 .. nvlen loop
         len := len + CT.len (nv (x).enumeration) + 1;
      end loop;
      declare
         cursor : Natural := 1;
         curend : Natural;
         result : String (1 .. len - 1) := (others => ',');
      begin
         for x in 1 .. nvlen loop
            curend := cursor - 1 + CT.len (nv (x).enumeration);
            result (cursor .. curend) := CT.USS (nv (x).enumeration);
            cursor := curend + 2;
         end loop;
         return result;
      end;
   end convert;

   function convert (nv : Settype) return Wide_String
   is
      preview : String := convert (nv);
   begin
      return ACC.To_Wide_String (preview);
   end convert;

   function convert (nv : Settype) return Wide_Wide_String
   is
      preview : String := convert (nv);
   begin
      return ACC.To_Wide_Wide_String (preview);
   end convert;


   ---------------------------
   --  convert to Ada Time  --
   ---------------------------
   function convert (nv : String) return AC.Time
   is
      len    : constant Natural  := nv'Length;
      NF     : constant Natural  := nv'First;
      year   : AC.Year_Number   := AC.Year_Number'First;
      month  : AC.Month_Number  := AC.Month_Number'First;
      day    : AC.Day_Number    := AC.Day_Number'First;
      hour   : ACF.Hour_Number   := ACF.Hour_Number'First;
      minute : ACF.Minute_Number := ACF.Minute_Number'First;
      second : ACF.Second_Number := ACF.Second_Number'First;
      HHMMSS : String (1 .. 8);
   begin
      --  Possible formats
      --  DATE      [10]: YYYY-MM-DD
      --  DATETIME  [19]: YYYY-MM-DD HH:MM:SS
      --  TIMESTAMP [19]: YYYY-MM-DD HH:MM:SS
      --  TIME      [08]: HH:MM:SS

      --  Handle HH:MM:SS formats
      case len is
         when  8 => HHMMSS := nv (NF .. NF + 7);
         when 19 => HHMMSS := nv (NF + 11 .. NF + 18);
         when others => null;
      end case;
      case len is
         when 8 | 19 =>
            hour   := ACF.Hour_Number   (Integer'Value (HHMMSS (1 .. 2)));
            minute := ACF.Minute_Number (Integer'Value (HHMMSS (4 .. 5)));
            second := ACF.Second_Number (Integer'Value (HHMMSS (7 .. 8)));
         when others => null;
      end case;

      --  Handle date formats
      case len is
         when 10 | 19 =>
            year  := AC.Year_Number  (Integer'Value (nv (NF .. NF + 3)));
            month := AC.Month_Number (Integer'Value (nv (NF + 5 .. NF + 6)));
            day   := AC.Day_Number   (Integer'Value (nv (NF + 8 .. NF + 9)));
         when others => null;
      end case;

      --  If this raises an exception, it probable means the date < 1901 or
      --  greater than 2099.  Turn this into a string time in that case.
      return ACF.Time_Of (Year   => year,
                          Month  => month,
                          Day    => day,
                          Hour   => hour,
                          Minute => minute,
                          Second => second);
   exception
      when others =>
         raise CONVERSION_FAILED
           with "String '" & nv & "' => Ada Time conversion failed";
   end convert;


   ---------------------------------
   --  convert string to Settype  --
   ---------------------------------
   function convert (nv : String; fixed : Natural := 0) return Settype
   is
      num_enums : Natural := 1;
      str       : constant String (1 .. nv'Length) := nv;
   begin
      for x in str'Range loop
         if str (x) = ',' then
            num_enums := num_enums + 1;
         end if;
      end loop;
      if fixed > 0 then
         num_enums := fixed;
      end if;
      declare
         result : Settype (1 .. num_enums) := (others => PARAM_IS_ENUM);
         cursor : Natural  := 1;
         curend : Natural  := 0;
         index  : Positive := 1;
      begin
         for x in str'Range loop
            if str (x) = ',' then
               result (index).enumeration := CT.SUS (str (cursor .. curend));
               index := index + 1;
               cursor := x + 1;
            end if;
            curend := curend + 1;
         end loop;
         result (index).enumeration := CT.SUS (str (cursor .. curend));
         return result;
      end;
   end convert;


end AdaBase.Results.Converters;
