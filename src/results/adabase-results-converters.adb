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
   function convert (nv : AD.nbyte0) return AD.nbyte1 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : AD.nbyte0) return AD.nbyte2 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : AD.nbyte0) return AD.nbyte3 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : AD.nbyte0) return AD.nbyte4 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : AD.nbyte0) return AD.nbyte8 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : AD.nbyte0) return AD.byte1 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : AD.nbyte0) return AD.byte2 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : AD.nbyte0) return AD.byte3 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : AD.nbyte0) return AD.byte4 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : AD.nbyte0) return AD.byte8 is
   begin
      case nv is
         when True  => return 1;
         when False => return 0;
      end case;
   end convert;

   function convert (nv : AD.nbyte0) return AD.real9 is
   begin
      case nv is
         when True  => return AD.real9 (1);
         when False => return AD.real9 (0);
      end case;
   end convert;

   function convert (nv : AD.nbyte0) return AD.real18 is
   begin
      case nv is
         when True  => return AD.real18 (1);
         when False => return AD.real18 (0);
      end case;
   end convert;

   function convert (nv : AD.nbyte0) return String is
   begin
      case nv is
         when True  => return "1";
         when False => return "0";
      end case;
   end convert;

   function convert (nv : AD.nbyte0) return Wide_String is
   begin
      case nv is
         when True  => return "1";
         when False => return "0";
      end case;
   end convert;

   function convert (nv : AD.nbyte0) return Wide_Wide_String is
   begin
      case nv is
         when True  => return "1";
         when False => return "0";
      end case;
   end convert;

   function convert (nv : AD.nbyte0) return AD.chain
   is
     result : AD.chain (1 .. 1) := (others => 0);
   begin
      if nv then
         result (1) := 1;
      end if;
      return result;
   end convert;


   ---------------------------
   --  CONVERT FROM NBYTE1  --
   ---------------------------
   function convert (nv : AD.nbyte1) return AD.nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte1) return AD.nbyte2 is
   begin
      return AD.nbyte2 (nv);
   end convert;

   function convert (nv : AD.nbyte1) return AD.nbyte3 is
   begin
      return AD.nbyte3 (nv);
   end convert;

   function convert (nv : AD.nbyte1) return AD.nbyte4 is
   begin
      return AD.nbyte4 (nv);
   end convert;

   function convert (nv : AD.nbyte1) return AD.nbyte8 is
   begin
      return AD.nbyte8 (nv);
   end convert;

   function convert (nv : AD.nbyte1) return AD.byte1
   is
      max : constant AD.nbyte1 := AD.nbyte1 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte1 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte1) return AD.byte2
   is
      max : constant AD.nbyte1 := AD.nbyte1 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte2 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte1) return AD.byte3
   is
      max : constant AD.nbyte1 := AD.nbyte1 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte3 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte1) return AD.byte4
   is
      max : constant AD.nbyte1 := AD.nbyte1 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte4 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte1) return AD.byte8
   is
      max : constant AD.nbyte1 := AD.nbyte1 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte1) return AD.real9 is
   begin
      return AD.real9 (nv);
   end convert;

   function convert (nv : AD.nbyte1) return AD.real18 is
   begin
      return AD.real18 (nv);
   end convert;

   function convert (nv : AD.nbyte1) return AD.chain
   is
      result : constant AD.chain (1 .. 1) := (1 => nv);
   begin
      return result;
   end convert;


   ---------------------------
   --  CONVERT FROM NBYTE2  --
   ---------------------------
   function convert (nv : AD.nbyte2) return AD.nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte2) return AD.nbyte1
   is
      max : constant AD.nbyte2 := AD.nbyte2 (AD.nbyte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte2) return AD.nbyte3 is
   begin
      return AD.nbyte3 (nv);
   end convert;

   function convert (nv : AD.nbyte2) return AD.nbyte4 is
   begin
      return AD.nbyte4 (nv);
   end convert;

   function convert (nv : AD.nbyte2) return AD.nbyte8 is
   begin
      return AD.nbyte8 (nv);
   end convert;

   function convert (nv : AD.nbyte2) return AD.byte1
   is
      max : constant AD.nbyte2 := AD.nbyte2 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte1 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte2) return AD.byte2
   is
      max : constant AD.nbyte2 := AD.nbyte2 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte2 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte2) return AD.byte3
   is
      max : constant AD.nbyte2 := AD.nbyte2 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte3 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte2) return AD.byte4
   is
      max : constant AD.nbyte2 := AD.nbyte2 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte4 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte2) return AD.byte8
   is
      max : constant AD.nbyte2 := AD.nbyte2 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte2) return AD.real9 is
   begin
      return AD.real9 (nv);
   end convert;

   function convert (nv : AD.nbyte2) return AD.real18 is
   begin
      return AD.real18 (nv);
   end convert;

   function convert (nv : AD.nbyte2) return AD.chain
   is
      use type AD.nbyte2;
      result : AD.chain (1 .. 2);
      block1 : constant AD.nbyte1 := AD.nbyte1 (nv and 16#FF#);
      block2 : constant AD.nbyte1 := AD.nbyte1
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
   function convert (nv : AD.nbyte3) return AD.nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte3) return AD.nbyte1
   is
      max : constant AD.nbyte3 := AD.nbyte3 (AD.nbyte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte3) return AD.nbyte2
   is
      max : constant AD.nbyte3 := AD.nbyte3 (AD.nbyte2'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte3) return AD.nbyte4 is
   begin
      return AD.nbyte4 (nv);
   end convert;

   function convert (nv : AD.nbyte3) return AD.nbyte8 is
   begin
      return AD.nbyte8 (nv);
   end convert;

   function convert (nv : AD.nbyte3) return AD.byte1
   is
      max : constant AD.nbyte3 := AD.nbyte3 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte1 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte3) return AD.byte2
   is
      max : constant AD.nbyte3 := AD.nbyte3 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte2 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte3) return AD.byte3
   is
      max : constant AD.nbyte3 := AD.nbyte3 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte3 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte3) return AD.byte4
   is
      max : constant AD.nbyte3 := AD.nbyte3 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte4 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte3) return AD.byte8
   is
      max : constant AD.nbyte3 := AD.nbyte3 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte3) return AD.real9 is
   begin
      return AD.real9 (nv);
   end convert;

   function convert (nv : AD.nbyte3) return AD.real18 is
   begin
      return AD.real18 (nv);
   end convert;

   function convert (nv : AD.nbyte3) return AD.chain
   is
      use type AD.nbyte3;
      result : AD.chain (1 .. 3);
      block1 : constant AD.nbyte1 := AD.nbyte1 (nv and 16#FF#);
      block2 : constant AD.nbyte1 := AD.nbyte1 (BIT.Shift_Right
                        (BIT.Unsigned_32 (nv and 16#FF00#), 8));
      block3 : constant AD.nbyte1 := AD.nbyte1 (BIT.Shift_Right
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
   function convert (nv : AD.nbyte4) return AD.nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte4) return AD.nbyte1
   is
      max : constant AD.nbyte4 := AD.nbyte4 (AD.nbyte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte4) return AD.nbyte2
   is
      max : constant AD.nbyte4 := AD.nbyte4 (AD.nbyte2'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte4) return AD.nbyte3
   is
      max : constant AD.nbyte4 := AD.nbyte4 (AD.nbyte3'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte4) return AD.nbyte8 is
   begin
      return AD.nbyte8 (nv);
   end convert;

   function convert (nv : AD.nbyte4) return AD.byte1
   is
      max : constant AD.nbyte4 := AD.nbyte4 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte1 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte4) return AD.byte2
   is
      max : constant AD.nbyte4 := AD.nbyte4 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte2 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte4) return AD.byte3
   is
      max : constant AD.nbyte4 := AD.nbyte4 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte3 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte4) return AD.byte4
   is
      max : constant AD.nbyte4 := AD.nbyte4 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte4 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte4) return AD.byte8
   is
      max : constant AD.nbyte4 := AD.nbyte4 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte4) return AD.real9 is
   begin
      return AD.real9 (nv);
   end convert;

   function convert (nv : AD.nbyte4) return AD.real18 is
   begin
      return AD.real18 (nv);
   end convert;

   function convert (nv : AD.nbyte4) return AD.chain
   is
      use type AD.nbyte4;
      result : AD.chain (1 .. 4);
      block1 : constant AD.nbyte1 := AD.nbyte1 (nv and 16#FF#);
      block2 : constant AD.nbyte1 := AD.nbyte1 (BIT.Shift_Right
                        (BIT.Unsigned_32 (nv and 16#FF00#), 8));
      block3 : constant AD.nbyte1 := AD.nbyte1 (BIT.Shift_Right
                        (BIT.Unsigned_32 (nv and 16#FF0000#), 16));
      block4 : constant AD.nbyte1 := AD.nbyte1 (BIT.Shift_Right
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
   function convert (nv : AD.nbyte8) return AD.nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte8) return AD.nbyte1
   is
      max : constant AD.nbyte8 := AD.nbyte8 (AD.nbyte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte8) return AD.nbyte2
   is
      max : constant AD.nbyte8 := AD.nbyte8 (AD.nbyte2'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte8) return AD.nbyte3
   is
      max : constant AD.nbyte8 := AD.nbyte8 (AD.nbyte3'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte8) return AD.nbyte4
   is
      max : constant AD.nbyte8 := AD.nbyte8 (AD.nbyte4'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte8) return AD.byte1
   is
      max : constant AD.nbyte8 := AD.nbyte8 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte1 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte8) return AD.byte2
   is
      max : constant AD.nbyte8 := AD.nbyte8 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte2 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte8) return AD.byte3
   is
      max : constant AD.nbyte8 := AD.nbyte8 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte3 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte8) return AD.byte4
   is
      max : constant AD.nbyte8 := AD.nbyte8 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte4 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte8) return AD.byte8
   is
      max : constant AD.nbyte8 := AD.nbyte8 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.nbyte8) return AD.real9 is
   begin
      return AD.real9 (nv);
   end convert;

   function convert (nv : AD.nbyte8) return AD.real18 is
   begin
      return AD.real18 (nv);
   end convert;


   ---------------------------
   --  CONVERT FROM BYTE1  --
   ---------------------------
   function convert (nv : AD.byte1) return AD.nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte1) return AD.nbyte1 is
   begin
      case nv is
         when 0 .. AD.byte1'Last => return AD.nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte1) return AD.nbyte2 is
   begin
      case nv is
         when 0 .. AD.byte1'Last => return AD.nbyte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte1) return AD.nbyte3 is
   begin
      case nv is
         when 0 .. AD.byte1'Last => return AD.nbyte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte1) return AD.nbyte4 is
   begin
      case nv is
         when 0 .. AD.byte1'Last => return AD.nbyte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte1) return AD.nbyte8 is
   begin
      case nv is
         when 0 .. AD.byte1'Last => return AD.nbyte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte1) return AD.byte2 is
   begin
      return AD.byte2 (nv);
   end convert;

   function convert (nv : AD.byte1) return AD.byte3 is
   begin
      return AD.byte3 (nv);
   end convert;

   function convert (nv : AD.byte1) return AD.byte4 is
   begin
      return AD.byte4 (nv);
   end convert;

   function convert (nv : AD.byte1) return AD.byte8 is
   begin
      return AD.byte8 (nv);
   end convert;

   function convert (nv : AD.byte1) return AD.real9 is
   begin
      return AD.real9 (nv);
   end convert;

   function convert (nv : AD.byte1) return AD.real18 is
   begin
      return AD.real18 (nv);
   end convert;


   ---------------------------
   --  CONVERT FROM BYTE2  --
   ---------------------------
   function convert (nv : AD.byte2) return AD.nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte2) return AD.nbyte1
   is
      max : constant AD.byte2 := AD.byte2 (AD.nbyte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte2) return AD.nbyte2 is
   begin
      case nv is
         when 0 .. AD.byte2'Last => return AD.nbyte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte2) return AD.nbyte3 is
   begin
      case nv is
         when 0 .. AD.byte2'Last => return AD.nbyte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte2) return AD.nbyte4 is
   begin
      case nv is
         when 0 .. AD.byte2'Last => return AD.nbyte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte2) return AD.nbyte8 is
   begin
      case nv is
         when 0 .. AD.byte2'Last => return AD.nbyte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte2) return AD.byte1
   is
      max : constant AD.byte2 := AD.byte2 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte2) return AD.byte3 is
   begin
      return AD.byte3 (nv);
   end convert;

   function convert (nv : AD.byte2) return AD.byte4 is
   begin
      return AD.byte4 (nv);
   end convert;

   function convert (nv : AD.byte2) return AD.byte8 is
   begin
      return AD.byte8 (nv);
   end convert;

   function convert (nv : AD.byte2) return AD.real9 is
   begin
      return AD.real9 (nv);
   end convert;

   function convert (nv : AD.byte2) return AD.real18 is
   begin
      return AD.real18 (nv);
   end convert;


   ---------------------------
   --  CONVERT FROM BYTE3  --
   ---------------------------
   function convert (nv : AD.byte3) return AD.nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte3) return AD.nbyte1
   is
      max : constant AD.byte3 := AD.byte3 (AD.nbyte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte3) return AD.nbyte2
   is
      max : constant AD.byte3 := AD.byte3 (AD.nbyte2'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte3) return AD.nbyte3 is
   begin
      case nv is
         when 0 .. AD.byte3'Last => return AD.nbyte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte3) return AD.nbyte4 is
   begin
      case nv is
         when 0 .. AD.byte3'Last => return AD.nbyte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte3) return AD.nbyte8 is
   begin
      case nv is
         when 0 .. AD.byte3'Last => return AD.nbyte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte3) return AD.byte1
   is
      max : constant AD.byte3 := AD.byte3 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte3) return AD.byte2
   is
      max : constant AD.byte3 := AD.byte3 (AD.byte2'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte3) return AD.byte4 is
   begin
      return AD.byte4 (nv);
   end convert;

   function convert (nv : AD.byte3) return AD.byte8 is
   begin
      return AD.byte8 (nv);
   end convert;

   function convert (nv : AD.byte3) return AD.real9 is
   begin
      return AD.real9 (nv);
   end convert;

   function convert (nv : AD.byte3) return AD.real18 is
   begin
      return AD.real18 (nv);
   end convert;


   ---------------------------
   --  CONVERT FROM BYTE4  --
   ---------------------------
   function convert (nv : AD.byte4) return AD.nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte4) return AD.nbyte1
   is
      max : constant AD.byte4 := AD.byte4 (AD.nbyte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte4) return AD.nbyte2
   is
      max : constant AD.byte4 := AD.byte4 (AD.nbyte2'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte4) return AD.nbyte3
   is
      max : constant AD.byte4 := AD.byte4 (AD.nbyte3'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte4) return AD.nbyte4 is
   begin
      case nv is
         when 0 .. AD.byte4'Last => return AD.nbyte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte4) return AD.nbyte8 is
   begin
      case nv is
         when 0 .. AD.byte4'Last => return AD.nbyte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte4) return AD.byte1
   is
      max : constant AD.byte4 := AD.byte4 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte4) return AD.byte2
   is
      max : constant AD.byte4 := AD.byte4 (AD.byte2'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte4) return AD.byte3
   is
      max : constant AD.byte4 := AD.byte4 (AD.byte3'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte4) return AD.byte8 is
   begin
      return AD.byte8 (nv);
   end convert;

   function convert (nv : AD.byte4) return AD.real9 is
   begin
      return AD.real9 (nv);
   end convert;

   function convert (nv : AD.byte4) return AD.real18 is
   begin
      return AD.real18 (nv);
   end convert;


   ---------------------------
   --  CONVERT FROM BYTE8  --
   ---------------------------
   function convert (nv : AD.byte8) return AD.nbyte0 is
   begin
      case nv is
         when 0      => return False;
         when 1      => return True;
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte8) return AD.nbyte1
   is
      max : constant AD.byte8 := AD.byte8 (AD.nbyte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte8) return AD.nbyte2
   is
      max : constant AD.byte8 := AD.byte8 (AD.nbyte2'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte8) return AD.nbyte3
   is
      max : constant AD.byte8 := AD.byte8 (AD.nbyte3'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte8) return AD.nbyte4
   is
      max : constant AD.byte8 := AD.byte8 (AD.nbyte4'Last);
   begin
      case nv is
         when 0 .. max => return AD.nbyte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte8) return AD.nbyte8 is
   begin
      case nv is
         when 0 .. AD.byte8'Last => return AD.nbyte8 (nv);
         when others   => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte8) return AD.byte1
   is
      max : constant AD.byte8 := AD.byte8 (AD.byte1'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte1 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte8) return AD.byte2
   is
      max : constant AD.byte8 := AD.byte8 (AD.byte2'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte2 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte8) return AD.byte3
   is
      max : constant AD.byte8 := AD.byte8 (AD.byte3'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte3 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte8) return AD.byte4
   is
      max : constant AD.byte8 := AD.byte8 (AD.byte4'Last);
   begin
      case nv is
         when 0 .. max => return AD.byte4 (nv);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.byte8) return AD.real9 is
   begin
      return AD.real9 (nv);
   end convert;

   function convert (nv : AD.byte8) return AD.real18 is
   begin
      return AD.real18 (nv);
   end convert;

   function convert (nv : AD.nbyte8) return AD.chain
   is
      use type AD.nbyte8;
      result : AD.chain (1 .. 8);
      b      : AD.chain (1 .. 8);
   begin
      b (1) := AD.nbyte1 (nv and 16#FF#);
      for s in 1 .. 7 loop
         declare
            use type BIT.Unsigned_64;
            shft : constant Natural := s * 8;
            mask : constant BIT.Unsigned_64 := BIT.Shift_Left (16#FF#, shft);
            slvr : constant BIT.Unsigned_64 := BIT.Unsigned_64 (nv) and mask;
         begin
            b (s + 1) := AD.nbyte1 (BIT.Shift_Right (slvr, shft));
         end;
      end loop;
      if Big_Endian then
         result := (b (8), b (7), b (6), b (5), b (4), b (3), b (2), b (1));
      else
         result := b;
      end if;
      return result;
   end convert;


   --------------------------
   --  CONVERT FROM REAL9  --
   --------------------------
   function convert (nv : AD.real9) return AD.real18 is
   begin
      return AD.real18 (nv);
   end convert;


   ---------------------------
   --  CONVERT FROM REAL18  --
   ---------------------------
   function convert (nv : AD.real18) return AD.real9 is
   begin
      return AD.real9 (nv);
   end convert;


   ---------------------------------
   --  CONVERT STRING TO BOOLEAN  --
   ---------------------------------
   function convert (nv : AD.textual) return AD.nbyte0
   is
      nvstr : constant String := AD.SU.To_String (Source => nv);
   begin
      if nvstr = "0" then
         return False;
      end if;
      if nvstr = "1" then
         return True;
      end if;
      raise CONVERSION_FAILED with "Tried to convert '" & nvstr & "'";
   end convert;

   function convert (nv : textwide) return AD.nbyte0
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
      raise CONVERSION_FAILED with "Tried to convert '" & nverr & "'";
   end convert;

   function convert (nv : textsuper) return AD.nbyte0
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
      raise CONVERSION_FAILED with "Tried to convert '" & nverr & "'";
   end convert;


   ---------------------------------
   --  CONVERT *STRING TO *STRING --
   ---------------------------------
   function convert (nv : AD.textual) return String is
   begin
      return AD.SU.To_String (nv);
   end convert;

   function convert (nv : AD.textual) return Wide_String
   is
      nvstr : constant String := AD.SU.To_String (nv);
   begin
      return ACC.To_Wide_String (Item => nvstr);
   end convert;

   function convert (nv : AD.textual) return Wide_Wide_String
   is
      nvstr : constant String := AD.SU.To_String (nv);
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
   function convert (nv : AD.enumtype) return AD.nbyte1
   is
      max : constant Natural := Natural (AD.nbyte1'Last);
   begin
      case nv.index is
         when 0 .. max => return AD.nbyte1 (nv.index);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.enumtype) return AD.nbyte2
   is
      max : constant Natural := Natural (AD.nbyte2'Last);
   begin
      case nv.index is
         when 0 .. max => return AD.nbyte2 (nv.index);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.enumtype) return AD.nbyte3
   is
      max : constant Natural := Natural (AD.nbyte3'Last);
   begin
      case nv.index is
         when 0 .. max => return AD.nbyte3 (nv.index);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.enumtype) return AD.nbyte4 is
   begin
      return AD.nbyte4 (nv.index);
   end convert;

   function convert (nv : AD.enumtype) return AD.nbyte8 is
   begin
      return AD.nbyte8 (nv.index);
   end convert;

   function convert (nv : AD.enumtype) return AD.byte1
   is
      max : constant Natural := Natural (AD.byte1'Last);
   begin
      case nv.index is
         when 0 .. max => return AD.byte1 (nv.index);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.enumtype) return AD.byte2
   is
      max : constant Natural := Natural (AD.byte2'Last);
   begin
      case nv.index is
         when 0 .. max => return AD.byte2 (nv.index);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.enumtype) return AD.byte3
   is
      max : constant Natural := Natural (AD.byte3'Last);
   begin
      case nv.index is
         when 0 .. max => return AD.byte3 (nv.index);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.enumtype) return AD.byte4
   is
      max : constant Natural := Natural (AD.byte4'Last);
   begin
      case nv.index is
         when 0 .. max => return AD.byte4 (nv.index);
         when others => raise TARGET_TYPE_TOO_NARROW;
      end case;
   end convert;

   function convert (nv : AD.enumtype) return AD.byte8 is
   begin
      return AD.byte8 (nv.index);
   end convert;

   function convert (nv : AD.enumtype) return String is
   begin
      return AD.SU.To_String (Source => nv.enumeration);
   end convert;

   function convert (nv : AD.enumtype) return Wide_String
   is
      str : constant String := AD.SU.To_String (Source => nv.enumeration);
   begin
      return ACC.To_Wide_String (Item => str);
   end convert;

   function convert (nv : AD.enumtype) return Wide_Wide_String
   is
      str : constant String := AD.SU.To_String (Source => nv.enumeration);
   begin
      return ACC.To_Wide_Wide_String (Item => str);
   end convert;

   ------------------------
   --  CHAIN (OF BYTES)  --
   ------------------------
   function convert (nv : AD.chain) return AD.nbyte1 is
   begin
      if nv'Length > 1 then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      return nv (1);
   end convert;

   function convert (nv : AD.chain) return AD.nbyte2
   is
      use type AD.nbyte2;
      cn : AD.chain (1 .. 2) := (others => 0);
   begin
      if nv'Length > 2 then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      for n in 1 .. nv'Length loop
         cn (n) := nv (n);
      end loop;
      if Big_Endian then
         return
           AD.nbyte2 (BIT.Shift_Left (BIT.Unsigned_16 (cn (1)), 8)) +
           AD.nbyte2 (cn (2));
      else
         return
           AD.nbyte2 (BIT.Shift_Left (BIT.Unsigned_16 (cn (2)), 8)) +
           AD.nbyte2 (cn (1));
      end if;
   end convert;

   function convert (nv : AD.chain) return AD.nbyte3
   is
      use type AD.nbyte3;
      cn : AD.chain (1 .. 3) := (others => 0);
   begin
      if nv'Length > 3 then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      for n in 1 .. nv'Length loop
         cn (n) := nv (n);
      end loop;
      if Big_Endian then
         return
           AD.nbyte3 (BIT.Shift_Left (BIT.Unsigned_32 (cn (1)), 16)) +
           AD.nbyte3 (BIT.Shift_Left (BIT.Unsigned_16 (cn (2)), 8)) +
           AD.nbyte3 (cn (3));
      else
         return
           AD.nbyte3 (BIT.Shift_Left (BIT.Unsigned_32 (cn (3)), 16)) +
           AD.nbyte3 (BIT.Shift_Left (BIT.Unsigned_16 (cn (2)), 8)) +
           AD.nbyte3 (cn (1));
      end if;
   end convert;

   function convert (nv : AD.chain) return AD.nbyte4
   is
      use type AD.nbyte4;
      cn : AD.chain (1 .. 4) := (others => 0);
   begin
      if nv'Length > 4 then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      for n in 1 .. nv'Length loop
         cn (n) := nv (n);
      end loop;
      if Big_Endian then
         return
           AD.nbyte4 (BIT.Shift_Left (BIT.Unsigned_32 (cn (1)), 24)) +
           AD.nbyte4 (BIT.Shift_Left (BIT.Unsigned_32 (cn (2)), 16)) +
           AD.nbyte4 (BIT.Shift_Left (BIT.Unsigned_16 (cn (3)), 8)) +
           AD.nbyte4 (cn (4));
      else
         return
           AD.nbyte4 (BIT.Shift_Left (BIT.Unsigned_32 (cn (4)), 24)) +
           AD.nbyte4 (BIT.Shift_Left (BIT.Unsigned_32 (cn (3)), 16)) +
           AD.nbyte4 (BIT.Shift_Left (BIT.Unsigned_16 (cn (2)), 8)) +
           AD.nbyte4 (cn (1));
      end if;
   end convert;

   function convert (nv : AD.chain) return AD.nbyte8
   is
      use type AD.nbyte8;
      cn : AD.chain (1 .. 8) := (others => 0);
   begin
      if nv'Length > 8 then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      for n in 1 .. nv'Length loop
         cn (n) := nv (n);
      end loop;
      if Big_Endian then
         return
           AD.nbyte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (1)), 56)) +
           AD.nbyte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (2)), 48)) +
           AD.nbyte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (3)), 40)) +
           AD.nbyte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (4)), 32)) +
           AD.nbyte8 (BIT.Shift_Left (BIT.Unsigned_32 (cn (5)), 24)) +
           AD.nbyte8 (BIT.Shift_Left (BIT.Unsigned_32 (cn (6)), 16)) +
           AD.nbyte8 (BIT.Shift_Left (BIT.Unsigned_16 (cn (7)), 8)) +
           AD.nbyte8 (cn (8));
      else
         return
           AD.nbyte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (8)), 56)) +
           AD.nbyte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (7)), 48)) +
           AD.nbyte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (6)), 40)) +
           AD.nbyte8 (BIT.Shift_Left (BIT.Unsigned_64 (cn (5)), 32)) +
           AD.nbyte8 (BIT.Shift_Left (BIT.Unsigned_32 (cn (4)), 24)) +
           AD.nbyte8 (BIT.Shift_Left (BIT.Unsigned_32 (cn (3)), 16)) +
           AD.nbyte8 (BIT.Shift_Left (BIT.Unsigned_16 (cn (2)), 8)) +
           AD.nbyte8 (cn (1));
      end if;
   end convert;


end AdaBase.Results.Converters;
