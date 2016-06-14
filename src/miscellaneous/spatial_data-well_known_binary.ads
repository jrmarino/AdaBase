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
   function convert_to_IEEE754 (direction : WKB_Endianness;
                                chain     : WKB_Double_Precision_Chain)
                                return Geometric_Real;

   function handle_coordinate (direction : WKB_Endianness;
                               payload : WKB_Chain;
                               marker : in out Natural)
                               return Geometric_Real;

   function handle_new_point (payload : WKB_Chain;
                              marker : in out Natural)
                              return Geometric_Point;

   function handle_linestring (payload : WKB_Chain;
                               marker : in out Natural)
                               return Geometric_Line_String;

   function handle_polyrings  (direction : WKB_Endianness;
                               payload   : WKB_Chain;
                               marker    : in out Natural)
                               return Geometric_Ring;

   function handle_polygon (payload : WKB_Chain;
                            marker : in out Natural)
                            return Geometric_Polygon;

   procedure handle_unit_collection (flavor  : Collection_Type;
                                     payload : WKB_Chain;
                                     marker : in out Natural;
                                     collection : in out Geometry);

   function round_to_16_digits (FP : Geometric_Real) return Geometric_Real;

end Spatial_Data.Well_Known_Binary;
