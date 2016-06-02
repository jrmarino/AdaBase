--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with CommonText;
with Spatial_Data;
with Ada.Calendar.Formatting;
with Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.UTF_Encoding;

package AdaBase.Results is

   package CT   renames CommonText;
   package AC   renames Ada.Calendar;
   package ACF  renames Ada.Calendar.Formatting;
   package SUW  renames Ada.Strings.Wide_Unbounded;
   package SUWW renames Ada.Strings.Wide_Wide_Unbounded;
   package SUTF renames Ada.Strings.UTF_Encoding;
   package SPAT  renames Spatial_Data;

   subtype Textual   is CT.Text;
   subtype Textwide  is SUW.Unbounded_Wide_String;
   subtype Textsuper is SUWW.Unbounded_Wide_Wide_String;
   subtype Text_UTF8 is SUTF.UTF_8_String;

   TARGET_TYPE_TOO_NARROW : exception;
   CONVERSION_FAILED      : exception;
   UNSUPPORTED_CONVERSION : exception;
   COLUMN_DOES_NOT_EXIST  : exception;
   CONSTRUCTOR_DO_NOT_USE : exception;

   -------------------------------------------
   --  Supported Field Types (Standardized) --
   -------------------------------------------

   type Bit1   is mod 2 ** 1;
   type NByte1 is mod 2 ** 8;
   type NByte2 is mod 2 ** 16;
   type NByte3 is mod 2 ** 24;
   type NByte4 is mod 2 ** 32;
   type NByte8 is mod 2 ** 64;
   type Byte8  is range -2 ** 63 .. 2 ** 63 - 1;
   type Byte4  is range -2 ** 31 .. 2 ** 31 - 1;
   type Byte3  is range -2 ** 23 .. 2 ** 23 - 1;
   type Byte2  is range -2 ** 15 .. 2 ** 15 - 1;
   type Byte1  is range -2 **  7 .. 2 **  7 - 1;
   type Real9  is digits 9;
   type Real18 is digits 18;

   subtype NByte0 is Boolean;

   type Enumtype is record enumeration : Textual; end record;
   type Settype is array (Positive range <>) of Enumtype;
   type Chain   is array (Positive range <>) of NByte1;
   type Bits    is array (Natural range <>)  of Bit1;

   type NByte0_Access   is access all NByte0;
   type NByte1_Access   is access all NByte1;
   type NByte2_Access   is access all NByte2;
   type NByte3_Access   is access all NByte3;
   type NByte4_Access   is access all NByte4;
   type NByte8_Access   is access all NByte8;
   type Byte1_Access    is access all Byte1;
   type Byte2_Access    is access all Byte2;
   type Byte3_Access    is access all Byte3;
   type Byte4_Access    is access all Byte4;
   type Byte8_Access    is access all Byte8;
   type Real9_Access    is access all Real9;
   type Real18_Access   is access all Real18;
   type Str1_Access     is access all Textual;
   type Str2_Access     is access all Textwide;
   type Str4_Access     is access all Textsuper;
   type Time_Access     is access all AC.Time;
   type Chain_Access    is access all Chain;
   type Enum_Access     is access all Enumtype;
   type Settype_Access  is access all Settype;
   type Geometry_Access is access all SPAT.Geometry;
   type Bits_Access     is access all Bits;
   type S_UTF8_Access   is access all Text_UTF8;

   Blank_String   : constant Textual   := CT.blank;
   Blank_WString  : constant Textwide  := SUW.Null_Unbounded_Wide_String;
   Blank_WWString : constant Textsuper :=
                       SUWW.Null_Unbounded_Wide_Wide_String;

   ------------------------------------------------
   --  CONSTANTS FOR PARAMETER TYPE DEFINITIONS  --
   ------------------------------------------------
   PARAM_IS_BOOLEAN   : constant NByte0 := False;
   PARAM_IS_NBYTE_1   : constant NByte1 := 0;
   PARAM_IS_NBYTE_2   : constant NByte2 := 0;
   PARAM_IS_NBYTE_3   : constant NByte3 := 0;
   PARAM_IS_NBYTE_4   : constant NByte4 := 0;
   PARAM_IS_NBYTE_8   : constant NByte8 := 0;
   PARAM_IS_BYTE_1    : constant Byte1 := 0;
   PARAM_IS_BYTE_2    : constant Byte2 := 0;
   PARAM_IS_BYTE_3    : constant Byte3 := 0;
   PARAM_IS_BYTE_4    : constant Byte4 := 0;
   PARAM_IS_BYTE_8    : constant Byte8 := 0;
   PARAM_IS_REAL_9    : constant Real9  := 0.0;
   PARAM_IS_REAL_18   : constant Real18 := 0.0;
   PARAM_IS_CHAIN     : constant Chain := (1 .. 1 => 0);
   PARAM_IS_BITS      : constant Bits  := (0 .. 0 => 0);
   PARAM_IS_ENUM      : constant Enumtype  := (enumeration => Blank_String);
   PARAM_IS_SET       : constant Settype   := (1 .. 1 => (PARAM_IS_ENUM));
   PARAM_IS_TEXTUAL   : constant Textual   := Blank_String;
   PARAM_IS_TEXTWIDE  : constant Textwide  := Blank_WString;
   PARAM_IS_TEXTSUPER : constant Textsuper := Blank_WWString;
   PARAM_IS_TEXT_UTF8 : constant Text_UTF8 := blankstring;
   PARAM_IS_GEOMETRY  : constant SPAT.Geometry :=
                        SPAT.initialize_as_point ((0.0, 0.0));
   PARAM_IS_TIMESTAMP : constant AC.Time :=
                        ACF.Time_Of (Year       => AC.Year_Number'First,
                                     Month      => AC.Month_Number'First,
                                     Day        => AC.Day_Number'First,
                                     Hour       => ACF.Hour_Number'First,
                                     Minute     => ACF.Minute_Number'First,
                                     Second     => ACF.Second_Number'First,
                                     Sub_Second => ACF.Second_Duration'First);

end AdaBase.Results;
