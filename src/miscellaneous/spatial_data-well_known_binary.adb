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
   begin
      if chainlen < 21 then
         raise WKB_INVALID
           with "Chain is smaller than required to accommodate a point)";
      end if;
      declare
         function entity_count return Natural;
         marker     : Natural := binary'First;
         endianness : constant WKB_Endianness :=
                      decode_endianness (binary (marker));
         ID_chain   : constant WKB_Identifier_Chain :=
                      binary (marker + 1 .. marker + 4);
         Identity   : constant WKB_Identifier :=
                      decode_identifier (direction => endianness,
                                         value     => ID_chain);
         col_type   : constant Collection_Type :=
                      get_collection_type (Identity);
         product    : Geometry;
         entities   : Natural;

         function entity_count return Natural is
         begin
            return Natural (decode_hex32
                            (endianness, binary (marker + 5 .. marker + 8)));
         end entity_count;
      begin
         case col_type is
            when unset =>
               return product; -- unset
            when single_point |
                 single_line_string |
                 single_polygon |
                 multi_point |
                 multi_line_string |
                 multi_polygon =>
               handle_unit_collection (flavor     => col_type,
                                       payload    => binary,
                                       marker     => marker,
                                       collection => product);
            when heterogeneous =>
               entities := entity_count;
               marker := marker + 9;
               for entity in 1 .. entities loop
                  handle_unit_collection (flavor     => col_type,
                                          payload    => binary,
                                          marker     => marker,
                                          collection => product);
               end loop;
         end case;
         return product;
      end;
   end Translate_WKB;


   ---------------------
   --  Construct_WKB  --
   ---------------------
   function Construct_WKB (shape : Geometry) return String is
   begin
      return "IMPLEMENT, PLEASE";
   end Construct_WKB;


   ------------------------------
   --  handle_unit_collection  --
   ------------------------------
   procedure handle_unit_collection (flavor  : Collection_Type;
                                     payload : WKB_Chain;
                                     marker : in out Natural;
                                     collection : in out Geometry)
   is
      function entity_count return Natural;
      procedure attach (anything : Geometry);
      endianness : constant WKB_Endianness :=
                   decode_endianness (payload (marker));
      ID_chain   : constant WKB_Identifier_Chain :=
                   payload (marker + 1 .. marker + 4);
      Identity   : constant WKB_Identifier :=
                   decode_identifier (direction => endianness,
                           value     => ID_chain);
      col_type   : constant Collection_Type :=
                   get_collection_type (Identity);
      entities   : Natural;
      initialize_first : constant Boolean := (collection.contents = unset);

      function entity_count return Natural is
      begin
         return Natural (decode_hex32 (endianness,
                         payload (marker + 5 .. marker + 8)));
      end entity_count;

      procedure attach (anything : Geometry) is
      begin
         if initialize_first then
            if flavor = heterogeneous then
               collection := initialize_as_collection (anything);
            else
               collection := anything;
            end if;
         else
            augment_collection (collection, anything);
         end if;
      end attach;
   begin
      case col_type is
         when unset =>
            raise WKB_INVALID
              with "Handle_Unit_collection: Should never happen";
         when single_point =>
            declare
               pt : Geometric_Point := handle_new_point (payload, marker);
            begin
               attach (initialize_as_point (pt));
            end;
         when multi_point =>
            entities := entity_count;
            marker := marker + 9;
            declare
               --  required to have at least one point
               pt1 : Geometric_Point := handle_new_point (payload, marker);
               element : Geometry := initialize_as_multi_point (pt1);
            begin
               for x in 2 .. entities loop
                  augment_multi_point (element,
                                       handle_new_point (payload, marker));
               end loop;
               attach (element);
            end;
         when single_line_string =>
            declare
               LS : Geometric_Line_String :=
                    handle_linestring (payload, marker);
            begin
               attach (initialize_as_line (LS));
            end;
         when multi_line_string =>
            entities := entity_count;
            marker := marker + 9;
            --  Required to have at least one linestring
            declare
               LS : Geometric_Line_String :=
                    handle_linestring (payload, marker);
               element : Geometry := initialize_as_multi_line (LS);
            begin
               for additional_LS in 2 .. entities loop
                  augment_multi_line (element,
                                      handle_linestring (payload, marker));
               end loop;
               attach (element);
            end;
         when single_polygon =>
            declare
               PG : Geometric_Polygon := handle_polygon (payload, marker);
            begin
               attach (initialize_as_polygon (PG));
            end;
         when multi_polygon =>
            entities := entity_count;
            marker := marker + 9;
            --  Required to have at least one polygon
            declare
               PG : Geometric_Polygon := handle_polygon (payload, marker);
               element : Geometry := initialize_as_multi_polygon (PG);
            begin
               for additional_Poly in 2 .. entities loop
                  augment_multi_polygon (element,
                                         handle_polygon (payload, marker));
               end loop;
               attach (element);
            end;
         when heterogeneous =>
            raise WKB_INVALID
              with "collection inside collection not yet implemented";
      end case;
   end handle_unit_collection;


   -------------------------
   --  handle_coordinate  --
   -------------------------
   function handle_coordinate (direction : WKB_Endianness;
                               payload : WKB_Chain;
                               marker : in out Natural)
                               return Geometric_Real
   is
      Z : Geometric_Real;
   begin
      Z := convert_to_IEEE754 (direction, payload (marker .. marker + 7));
      marker := marker + 8;
      return Z;
   end handle_coordinate;


   ------------------------
   --  handle_new_point  --
   ------------------------
   function handle_new_point (payload : WKB_Chain;
                              marker : in out Natural) return Geometric_Point
   is
      pt_endian : WKB_Endianness := decode_endianness (payload (marker));
      X : Geometric_Real;
      Y : Geometric_Real;
   begin
      marker := marker + 5;
      X := handle_coordinate (pt_endian, payload, marker);
      Y := handle_coordinate (pt_endian, payload, marker);
      return (X, Y);
   end handle_new_point;


   -------------------------
   --  handle_linestring  --
   -------------------------
   function handle_linestring (payload : WKB_Chain; marker : in out Natural)
                               return Geometric_Line_String
   is
      ls_endian : WKB_Endianness := decode_endianness (payload (marker));
      num_points : Natural :=  Natural (decode_hex32 (ls_endian,
                                        payload (marker + 5 .. marker + 8)));
      LS : Geometric_Line_String (1 .. num_points);
      X : Geometric_Real;
      Y : Geometric_Real;
   begin
      marker := marker + 9;
      for pt in 1 .. num_points loop
         X := handle_coordinate (ls_endian, payload, marker);
         Y := handle_coordinate (ls_endian, payload, marker);
         LS (pt) := (X, Y);
      end loop;
      return LS;
   end handle_linestring;


   ------------------------
   --  handle_polyrings  --
   ------------------------
   function handle_polyrings  (direction : WKB_Endianness;
                               payload   : WKB_Chain;
                               marker    : in out Natural)
                               return Geometric_Ring
   is
      num_points : Natural :=  Natural (decode_hex32 (direction,
                                        payload (marker .. marker + 3)));
      ring : Geometric_Ring (1 .. num_points);
      X : Geometric_Real;
      Y : Geometric_Real;
   begin
      marker := marker + 4;
      for pt in 1 .. num_points loop
         X := handle_coordinate (direction, payload, marker);
         Y := handle_coordinate (direction, payload, marker);
         ring (pt) := (X, Y);
      end loop;
      return ring;
   end handle_polyrings;


   ----------------------
   --  handle_polygon  --
   ----------------------
   function handle_polygon (payload : WKB_Chain;
                            marker : in out Natural)
                            return Geometric_Polygon
   is
      midway_polygon : Geometric_Polygon;
      poly_endian : WKB_Endianness := decode_endianness (payload (marker));
      num_rings : Natural := Natural (decode_hex32 (poly_endian,
                                      payload (marker + 5 .. marker + 8)));
      --  There must be at least one ring (exterior
   begin
      marker := marker + 9;
      midway_polygon := start_polygon
        (outer_ring => handle_polyrings (direction => poly_endian,
                                         payload   => payload,
                                         marker    => marker));
      for x in 2 .. num_rings loop
         append_inner_ring
           (polygon    => midway_polygon,
            inner_ring => handle_polyrings (direction => poly_endian,
                                            payload   => payload,
                                            marker    => marker));
      end loop;

      return midway_polygon;
   end handle_polygon;


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
      fracked   : Boolean := False;
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
            fracked := True;
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
      negative  : Boolean;

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
         negative := True;
         factor := -1.0;
      else
         negative := False;
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

      if not fracked and then exponent = 0 then
         if negative then
            return -0.0;
         else
            return 0.0;
         end if;
      end if;

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
