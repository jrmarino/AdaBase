--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package Spatial_Data.Well_Known_Binary is

   type WKB_Byte  is mod 2 ** 8;
   type WKB_Chain is array (Positive range <>) of WKB_Byte;

   WKB_INVALID : exception;

   function Translate_WKB (WKBinary : String) return Geometry;
   function Construct_WKB (shape : Geometry) return String;
   function produce_WKT (WKBinary : CT.Text) return String;

private

   type WKB_exponent    is mod 2 ** 11;
   type WKB_Identifier  is mod 2 ** 12;
   type WKB_Hex32       is mod 2 ** 32;
   type WKB_IEEE754_Hex is mod 2 ** 64;
   type WKB_Endianness is (big_endian, little_endian);
   subtype WKB_Identifier_Chain is WKB_Chain (1 .. 4);
   subtype WKB_Double_Precision_Chain is WKB_Chain (1 .. 8);
   subtype WKB_Shape_Point_Chain is WKB_Chain (1 .. 16);

   function convert (nv : String) return WKB_Chain;
   function decode_endianness (value : WKB_Byte) return WKB_Endianness;
   function decode_hex32      (direction : WKB_Endianness;
                               value : WKB_Identifier_Chain) return WKB_Hex32;
   function decode_identifier (direction : WKB_Endianness;
                               value : WKB_Identifier_Chain)
                               return WKB_Identifier;
   function get_collection_type (identifier : WKB_Identifier)
                                 return Collection_Type;
--   function decode_number (direction : WKB_Endianness;
--                           value : WKB_Double_Precision_Chain)
--                           return WKB_IEEE754_Hex;
--   function convert_to_IEEE754 (hex : WKB_IEEE754_Hex) return Geometric_Real;
   function convert_to_IEEE754 (direction : WKB_Endianness;
                                chain     : WKB_Double_Precision_Chain)
                                return Geometric_Real;

   function handle_point (direction : WKB_Endianness;
                          payload   : WKB_Shape_Point_Chain)
                          return Geometric_Point;

end Spatial_Data.Well_Known_Binary;
