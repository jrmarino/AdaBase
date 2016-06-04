--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Strings;
with Ada.Characters.Conversions;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body AdaBase.Results.Converters is

   package AS  renames Ada.Strings;
   package ACC renames Ada.Characters.Conversions;

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
     result : Chain (1 .. 1) := (1 => 0);
   begin
      if nv then
         result (1) := 1;
      end if;
      return result;
   end convert;

   function convert (nv : NByte0) return Bits
   is
     result : Bits (0 .. 0) := (0 => 0);
   begin
      if nv then
         result (0) := 1;
      end if;
      return result;
   end convert;

   function cv2utf8 (nv : NByte0) return Text_UTF8 is
   begin
      case nv is
         when True  => return "1";
         when False => return "0";
      end case;
   end cv2utf8;


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
      if nvstr = "0" or else nvstr = "f" then
         return False;
      end if;
      if nvstr = "1" or else nvstr = "t" then
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

   function convert (nv : String) return Wide_String is
   begin
      return ACC.To_Wide_String (nv);
   end convert;

   function convert (nv : String) return Wide_Wide_String is
   begin
      return ACC.To_Wide_Wide_String (nv);
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

   function convert (nv : String; fixed : Natural := 0) return Bits
   is
      num_bits : Natural := CT.len (nv);
   begin
      if fixed > num_bits then
         num_bits := fixed;
      end if;
      declare
         longvs : String (1 .. num_bits) := (others => '0');
         result : Bits (0 .. num_bits - 1) := (others => 0);
         arrow  : Natural := result'First;
         rfirst : Natural := 1 + longvs'Length - nv'Length;
      begin
         longvs (rfirst .. longvs'Last) := nv;
         for x in longvs'Range loop
            case longvs (x) is
               when '0' => null;
               when '1' => result (arrow) := 1;
               when others =>
                  raise CONVERSION_FAILED
                    with "String => bits, '" & nv (x) & "' is not a valid bit";
            end case;
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

   function convert (nv : Textual) return Bits is
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

   function convert (nv : Textwide) return Bits
   is
      nvstr : constant String := convert (nv);
   begin
      return convert (nvstr);
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

   function convert (nv : Textsuper) return Bits
   is
      nvstr : constant String := convert (nv);
   begin
      return convert (nvstr);
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

   function convert (nv : Chain) return Bits
   is
      num_bits : Natural := nv'Length * 8;
      result   : Bits (0 .. num_bits - 1) := (others => 0);
      counter  : Natural := 0;
      submask  : constant array (0 .. 7) of NByte1 := (2 ** 0, 2 ** 1,
                                                       2 ** 2, 2 ** 3,
                                                       2 ** 4, 2 ** 5,
                                                       2 ** 6, 2 ** 7);
   begin
      for link in nv'Range loop
         for x in submask'Range loop
            if (nv (link) and submask (x)) > 0 then
               result (counter) := 1;
            end if;
            counter := counter + 1;
         end loop;
      end loop;
      return result;
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


   -------------------------
   --  CONVERT FROM BITS  --
   -------------------------
   function convert (nv : Bits) return NByte0 is
   begin
      if nv'Length > 1 then
         raise TARGET_TYPE_TOO_NARROW;
      end if;
      return nv (nv'First) = 1;
   end convert;

   function convert (nv : Bits) return String
   is
      result : String (1 .. nv'Length) := (others => '0');
      arrow  : Natural := result'Last;
   begin
      for x in nv'Range loop
         if nv (x) = 1 then
            result (arrow) := '1';
         end if;
         arrow := arrow - 1;
      end loop;
      return result;
   end convert;

   function convert (nv : Bits) return Chain
   is
      num_segments : Positive := 1 + ((nv'Length - 1) / 8);
      result  : Chain (1 .. num_segments) := (others => 0);
      link    : Positive := 1;
      counter : Natural := 0;
   begin
      for x in nv'Range loop
         if nv (x) > 0 then
            result (link) := result (link) + (2 ** counter);
         end if;
         counter := counter + 1;
         if counter = 8 then
            counter := 0;
            link := link + 1;
         end if;
      end loop;
      return result;
   end convert;

   function convert (nv : Bits) return Wide_String
   is
      bitstring : constant String := convert (nv);
   begin
      return ACC.To_Wide_String (Item => bitstring);
   end convert;

   function convert (nv : Bits) return Wide_Wide_String
   is
      bitstring : constant String := convert (nv);
   begin
      return ACC.To_Wide_Wide_String (Item => bitstring);
   end convert;


   ----------------------------------------
   --  Convert all data types to Texual  --
   ----------------------------------------
   function convert (nv : NByte0) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : NByte1) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : NByte2) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : NByte3) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : NByte4) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : NByte8) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : Byte1) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : Byte2) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : Byte3) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : Byte4) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : Byte8) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : Real9) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : Real18) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : Textwide) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : Textsuper) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : AC.Time) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : Enumtype) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : Bits) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : Chain) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   function convert (nv : Settype) return Textual
   is
      hold : String := convert (nv);
   begin
      return CT.SUS (hold);
   end convert;

   ---------------------------------------
   --  Convert some data types to UTF8  --
   ---------------------------------------
   function cv2utf8 (nv : String) return Text_UTF8
   is
      WWS : Wide_Wide_String := ACC.To_Wide_Wide_String (nv);
   begin
      return SUTF.Wide_Wide_Strings.Encode (Item => WWS);
   end cv2utf8;

   function cvu2str (nv : Text_UTF8) return String
   is
      WWS : Wide_Wide_String := SUTF.Wide_Wide_Strings.Decode (Item => nv);
   begin
      return ACC.To_String (WWS);
   end cvu2str;

   function cvu2str (nv : Text_UTF8) return Wide_String
   is
      WWS : Wide_Wide_String := SUTF.Wide_Wide_Strings.Decode (Item => nv);
   begin
      return ACC.To_Wide_String (WWS);
   end cvu2str;

   function cvu2str (nv : Text_UTF8) return Wide_Wide_String is
   begin
      return SUTF.Wide_Wide_Strings.Decode (Item => nv);
   end cvu2str;

   function cvu2str (nv : Textual) return Textual
   is
      holdutf8 : Text_UTF8 := CT.USS (nv);
      holdstr  : String := cvu2str (holdutf8);
   begin
      return CT.SUS (holdstr);
   end cvu2str;

   function cv2utf8 (nv : Textual) return Text_UTF8
   is
      hold : String := CT.USS (nv);
   begin
      return cv2utf8 (hold);
   end cv2utf8;

   function cv2utf8 (nv : Textwide) return Text_UTF8
   is
      hold : String := convert (nv);
   begin
      return cv2utf8 (hold);
   end cv2utf8;

   function cv2utf8 (nv : Textsuper) return Text_UTF8
   is
      hold : String := convert (nv);
   begin
      return cv2utf8 (hold);
   end cv2utf8;

   function cv2utf8 (nv : AC.Time) return Text_UTF8
   is
      hold : String := convert (nv);
   begin
      return cv2utf8 (hold);
   end cv2utf8;

   function cv2utf8 (nv : Enumtype) return Text_UTF8
   is
      hold : String := convert (nv);
   begin
      return cv2utf8 (hold);
   end cv2utf8;

   function cv2utf8 (nv : Settype) return Text_UTF8
   is
      hold : String := convert (nv);
   begin
      return cv2utf8 (hold);
   end cv2utf8;

end AdaBase.Results.Converters;
