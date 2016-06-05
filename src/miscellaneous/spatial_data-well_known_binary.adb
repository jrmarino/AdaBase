--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body Spatial_Data.Well_Known_Binary is

   -------------------
   --  produce_WKT  --
   -------------------
   function produce_WKT (WKBinary : CT.Text) return String
   is
      shape : Geometry := Translate_WKB (CT.USS (WKBinary));
   begin
      return Well_Known_Text (shape);
   end produce_WKT;


   ---------------------
   --  Translate_WKB  --
   ---------------------
   function Translate_WKB (WKBinary : String) return Geometry
   is
      binary   : WKB_Chain := convert (WKBinary);
      chainlen : Natural := binary'Length;
      required : Natural := 21;
   begin
      if chainlen < 21 then
         goto crash;
      end if;
      declare
         function entity_count return Natural;
         endianness : constant WKB_Endianness :=
                      decode_endianness (binary (1));
         ID_chain   : constant WKB_Identifier_Chain := binary (2 .. 5);
         Identity   : constant WKB_Identifier :=
                      decode_identifier (direction => endianness,
                                         value     => ID_chain);
         col_type   : constant Collection_Type :=
                      get_collection_type (Identity);
         product    : Geometry;
         marker     : Natural;
         entities   : Natural;

         function entity_count return Natural is
         begin
            return Natural (decode_hex32 (endianness, binary (6 .. 9)));
         end entity_count;
      begin
         case col_type is
            when unset | single_circle | single_infinite_line =>
               return product; -- unset
            when single_point =>
               return initialize_as_point
                 (handle_point (endianness, binary (6 .. 21)));
            when single_line_string =>
               entities := entity_count;
               required := 9 + (entities * 16);
               if required > chainlen or else entities < 2 then
                  goto crash;
               end if;
               declare
                  LS : Geometric_Line_String (1 .. entities);
               begin
                  marker := 10;
                  for x in 1 .. entities loop
                     LS (x) := handle_point (endianness,
                                             binary (marker .. marker + 15));
                     marker := marker + 16;
                  end loop;
                  product := initialize_as_line_string (LS);
               end;
               return product;
            when multi_point =>
               entities := entity_count;
               required := 9 + (entities * 21);
               if required > chainlen then
                  goto crash;
               end if;
               declare
                  pt_endian   : WKB_Endianness :=
                                decode_endianness (binary (10));
                  first_point : Geometric_Point :=
                                handle_point (endianness, binary (15 .. 30));
               begin
                  product := initialize_as_point (first_point);
               end;
               marker := 31;
               for x in 2 .. entities loop
                  declare
                     pt_endian  : WKB_Endianness :=
                                  decode_endianness (binary (marker));
                     next_point : Geometric_Point := handle_point
                       (endianness, binary (marker + 5 .. marker + 20));
                  begin
                     append_point (product, next_point);
                     marker := marker + 21;
                  end;
               end loop;
               return product;
            when single_polygon => null;
            when multi_line_string => null;
            when multi_polygon => null;
            when heterogeneous => null;
         end case;
      end;
      <<crash>>
      raise WKB_INVALID
        with "Chain is smaller than required" & required'Img & " links";
   end Translate_WKB;


   ---------------------
   --  Construct_WKB  --
   ---------------------
   function Construct_WKB (shape : Geometry) return String is
   begin
      return "IMPLEMENT, PLEASE";
   end Construct_WKB;


   --------------------
   --  handle_point  --
   --------------------
   function handle_point (direction : WKB_Endianness;
                          payload   : WKB_Shape_Point_Chain)
                          return Geometric_Point
   is
      X : Geometric_Real;
      Y : Geometric_Real;
   begin
      X := convert_to_IEEE754 (direction, payload (1 .. 8));
      Y := convert_to_IEEE754 (direction, payload (9 .. 16));
      return (X, Y);
   end handle_point;


   -------------------------
   --  decode_endianness  --
   -------------------------
   function decode_endianness (value : WKB_Byte) return WKB_Endianness is
   begin
      case value is
         when 0 => return big_endian;
         when 1 => return little_endian;
         when others =>
            raise WKB_INVALID
              with "Endian byte value is" & value'Img;
      end case;
   end decode_endianness;


   --------------------
   --  decode_hex32  --
   --------------------
   function decode_hex32 (direction : WKB_Endianness;
                          value : WKB_Identifier_Chain) return WKB_Hex32
   is
      result : WKB_Hex32 := 0;
      mask : array (1 .. 4) of WKB_Hex32 := (2 ** 0, 2 ** 8, 2 ** 16, 2 ** 24);
   begin
      case direction is
         when little_endian =>
            result := (WKB_Hex32 (value (1)) * mask (1)) +
                      (WKB_Hex32 (value (2)) * mask (2)) +
                      (WKB_Hex32 (value (3)) * mask (3)) +
                      (WKB_Hex32 (value (4)) * mask (4));
         when big_endian =>
            result := (WKB_Hex32 (value (4)) * mask (1)) +
                      (WKB_Hex32 (value (3)) * mask (2)) +
                      (WKB_Hex32 (value (2)) * mask (3)) +
                      (WKB_Hex32 (value (1)) * mask (4));
      end case;
      return result;
   end decode_hex32;


   -------------------------
   --  decode_identifier  --
   -------------------------
   function decode_identifier (direction : WKB_Endianness;
                               value : WKB_Identifier_Chain)
                               return WKB_Identifier
   is
      result : WKB_Hex32 := decode_hex32 (direction, value);
   begin
      if result > WKB_Hex32 (WKB_Identifier'Last) then
         raise WKB_INVALID
           with "Identifier value is way too high:" & result'Img;
      end if;
      return WKB_Identifier (result);
   end decode_identifier;


   ---------------------------
   --  get_collection_type  --
   ---------------------------
   function get_collection_type (identifier : WKB_Identifier)
                                 return Collection_Type is
   begin
      case identifier is
         when 18 .. 999 | 1018 .. 1999 | 2018 .. 2999 | 3018 .. 4095 =>
            raise WKB_INVALID
              with "Identifier does not map to any known geometry shape: " &
              identifier'Img;
         when 1000 .. 1017 =>
            raise WKB_INVALID
              with "3D (Z) shapes are not supported at this time: " &
              identifier'Img;
         when 2000 .. 2017 =>
            raise WKB_INVALID
              with "2D + M shapes are not supported at this time: " &
              identifier'Img;
         when 3000 .. 3017 =>
            raise WKB_INVALID
              with "4D (ZM) shapes are not supported at this time: " &
              identifier'Img;
         when 0 | 8 .. 17 =>
            raise WKB_INVALID
              with "This particular 2D shape is not yet supported: " &
              identifier'Img;
         when 1 => return single_point;
         when 2 => return single_line_string;
         when 3 => return single_polygon;
         when 4 => return multi_point;
         when 5 => return multi_line_string;
         when 6 => return multi_polygon;
         when 7 => return heterogeneous;
      end case;
   end get_collection_type;


   ---------------------
   --  decode_number  --
   ---------------------
--     function decode_number (direction : WKB_Endianness;
--                             value : WKB_Double_Precision_Chain)
--                             return WKB_IEEE754_Hex
--     is
--        result : WKB_IEEE754_Hex := 0;
--        mask : array (1 .. 8) of WKB_IEEE754_Hex :=
--          (2 **  0, 2 **  8, 2 ** 16, 2 ** 24,
--           2 ** 32, 2 ** 40, 2 ** 48, 2 ** 56);
--     begin
--        case direction is
--           when little_endian =>
--              result := (WKB_IEEE754_Hex (value (1)) * mask (1)) +
--                        (WKB_IEEE754_Hex (value (2)) * mask (2)) +
--                        (WKB_IEEE754_Hex (value (3)) * mask (3)) +
--                        (WKB_IEEE754_Hex (value (4)) * mask (4)) +
--                        (WKB_IEEE754_Hex (value (5)) * mask (5)) +
--                        (WKB_IEEE754_Hex (value (6)) * mask (6)) +
--                        (WKB_IEEE754_Hex (value (7)) * mask (7)) +
--                        (WKB_IEEE754_Hex (value (8)) * mask (8));
--           when big_endian =>
--              result := (WKB_IEEE754_Hex (value (8)) * mask (1)) +
--                        (WKB_IEEE754_Hex (value (7)) * mask (2)) +
--                        (WKB_IEEE754_Hex (value (6)) * mask (3)) +
--                        (WKB_IEEE754_Hex (value (5)) * mask (4)) +
--                        (WKB_IEEE754_Hex (value (4)) * mask (5)) +
--                        (WKB_IEEE754_Hex (value (3)) * mask (6)) +
--                        (WKB_IEEE754_Hex (value (2)) * mask (7)) +
--                        (WKB_IEEE754_Hex (value (1)) * mask (8));
--        end case;
--        return result;
--     end decode_number;


   --------------------------
   --  convert_to_IEEE754  --
   --------------------------
--     function convert_to_IEEE754 (hex : WKB_IEEE754_Hex) return Geometric_Real
--     is
--        sign_mask : WKB_IEEE754_Hex := 2 ** 63;
--        work_mask : WKB_IEEE754_Hex;
--        exponent  : WKB_exponent := 0;
--        fraction  : Geometric_Real := 0.0;
--        power_res : Geometric_Real;
--        result    : Geometric_Real;
--        factor    : Geometric_Real;
--        marker    : Integer := -1;
--     begin
--        if (hex and sign_mask) > 0 then
--           --  Negative sign
--           factor := -1.0;
--        else
--           factor := 1.0;
--        end if;
--        for x in 52 .. 62 loop
--           work_mask := 2 ** x;
--           if (hex and work_mask) > 0 then
--              exponent := exponent + (2 ** (x - 52));
--           end if;
--        end loop;
--        for x in reverse 0 .. 51 loop
--           work_mask := 2 ** x;
--           if (hex and work_mask) > 0 then
--              fraction := fraction + (2.0 ** marker);
--           end if;
--           marker := marker - 1;
--        end loop;
--        case exponent is
--           when 2047 =>
--              raise WKB_INVALID
--                with "Infinity/NAN";
--           when 0 =>
--              --  denormalized
--              power_res := 2.0 ** (-1022);
--              result := factor * fraction * power_res;
--           when 1 .. 2046 =>
--              --  normalized
--              power_res := 2.0 ** (Natural (exponent) - 1023);
--              result := factor * (1.0 + fraction) * power_res;
--        end case;
--        return result;
--     end convert_to_IEEE754;


   --------------------------
   --  convert_to_IEEE754  --
   --------------------------
   function convert_to_IEEE754 (direction : WKB_Endianness;
                                chain     : WKB_Double_Precision_Chain)
                                return Geometric_Real
   is
      function slice (link : Positive; bitpos : Natural; exp : Natural)
                      return WKB_exponent;
      function frack (link : Positive; bitpos : Natural; exp : Integer)
                      return Geometric_Real;

      our_chain : WKB_Double_Precision_Chain;
      byte_mask : constant array (0 .. 7) of WKB_Byte := (2 ** 0, 2 ** 1,
                                                          2 ** 2, 2 ** 3,
                                                          2 ** 4, 2 ** 5,
                                                          2 ** 6, 2 ** 7);

      function slice (link : Positive; bitpos : Natural; exp : Natural)
                      return WKB_exponent is
      begin
         if (our_chain (link) and byte_mask (bitpos)) > 0 then
            return 2 ** exp;
         end if;
         return 0;
      end slice;

      function frack (link : Positive; bitpos : Natural; exp : Integer)
                      return Geometric_Real is
      begin
         if (our_chain (link) and byte_mask (bitpos)) > 0 then
            return 2.0 ** exp;
         end if;
         return 0.0;
      end frack;

      sign_mask : constant WKB_Byte := byte_mask (7);
      exponent  : WKB_exponent := 0;
      fraction  : Geometric_Real := 0.0;
      power_res : Geometric_Real;
      result    : Geometric_Real;
      factor    : Geometric_Real;
      marker    : Integer;

   begin
      case direction is
         when big_endian    => our_chain := chain;
         when little_endian =>
            our_chain (1) := chain (8);
            our_chain (2) := chain (7);
            our_chain (3) := chain (6);
            our_chain (4) := chain (5);
            our_chain (5) := chain (4);
            our_chain (6) := chain (3);
            our_chain (7) := chain (2);
            our_chain (8) := chain (1);
      end case;
      if (our_chain (1) and sign_mask) > 0 then
         --  Negative sign
         factor := -1.0;
      else
         factor := 1.0;
      end if;
      exponent :=
        slice (link => 2, bitpos => 4, exp => 0) +  --  bit 52
        slice (link => 2, bitpos => 5, exp => 1) +
        slice (link => 2, bitpos => 6, exp => 2) +
        slice (link => 2, bitpos => 7, exp => 3) +
        slice (link => 1, bitpos => 0, exp => 4) +
        slice (link => 1, bitpos => 1, exp => 5) +
        slice (link => 1, bitpos => 2, exp => 6) +
        slice (link => 1, bitpos => 3, exp => 7) +
        slice (link => 1, bitpos => 4, exp => 8) +
        slice (link => 1, bitpos => 5, exp => 9) +
        slice (link => 1, bitpos => 6, exp => 10);   -- bit 62

      fraction :=
        frack (link => 2, bitpos => 3, exp => -1) +
        frack (link => 2, bitpos => 2, exp => -2) +
        frack (link => 2, bitpos => 1, exp => -3) +
        frack (link => 2, bitpos => 0, exp => -4);

      marker := -5;
      for link in 3 .. 8 loop
         for bitpos in reverse 0 .. 7 loop
            fraction := fraction + frack (link, bitpos, marker);
            marker := marker - 1;
         end loop;
      end loop;

      case exponent is
         when 2047 =>
            raise WKB_INVALID
              with "Infinity/NAN";
         when 0 =>
            --  denormalized
            power_res := 2.0 ** (-1022);
            result := factor * fraction * power_res;
         when 1 .. 2046 =>
            --  normalized
            power_res := 2.0 ** Integer (Natural (exponent) - 1023);
            result := factor * (1.0 + fraction) * power_res;
      end case;
      return round_to_16_digits (result);
   end convert_to_IEEE754;


   --------------------------
   --  round_to_16_digits  --
   --------------------------
   function round_to_16_digits (FP : Geometric_Real) return Geometric_Real
   is
      type Int64 is range -2 ** 63 .. 2 ** 63 - 1;
      --  Image always in form:
      --  [sign/space][digit][dot][17 digits]E[sign][2..3 digits]
      resimage : String := Geometric_Real'Image (FP);
      dot : Natural := CT.pinpoint (resimage, ".");
      exp : Natural := CT.pinpoint (resimage, "E");
      dec : String := resimage (resimage'First .. dot - 1) &
                      resimage (dot + 1 .. exp - 1);
      nagative : constant Boolean := (resimage (resimage'First) = '-');
      halfpump : constant Int64 := 50;
      vessel   : Int64;
   begin
      if nagative then
         vessel := Int64'Value (dec) - halfpump;
      else
         vessel := Int64'Value (dec) + halfpump;
      end if;
      declare
         decimage : String := Int64'Image (vessel);
      begin
         return Geometric_Real'Value
           (decimage (decimage'First .. decimage'First + 1) & '.' &
              decimage (decimage'First + 2 .. decimage'Last - 2) &
              resimage (exp .. resimage'Last));
      end;
   end round_to_16_digits;


   ---------------
   --  convert  --
   ---------------
   function convert (nv : String) return WKB_Chain
   is
      Chainlen : Natural := nv'Length;
      result : WKB_Chain (1 .. Chainlen) := (others => 0);
      arrow  : Natural := result'First;
   begin
      for x in nv'Range loop
         result (arrow) := WKB_Byte (Character'Pos (nv (x)));
         arrow := arrow + 1;
      end loop;
      return result;
   end convert;


end Spatial_Data.Well_Known_Binary;
