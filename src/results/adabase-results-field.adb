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

with AdaBase.Results.Converters;

package body AdaBase.Results.Field is

   package ARC renames AdaBase.Results.Converters;


   -----------------
   --  as_nbyte0  --
   -----------------
   function as_nbyte0 (field : map_field) return AD.nbyte0
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return field.native.v00;
         when ft_nbyte1    => return ARC.convert (field.native.v01);
         when ft_nbyte2    => return ARC.convert (field.native.v02);
         when ft_nbyte3    => return ARC.convert (field.native.v03);
         when ft_nbyte4    => return ARC.convert (field.native.v04);
         when ft_nbyte8    => return ARC.convert (field.native.v05);
         when ft_byte1     => return ARC.convert (field.native.v06);
         when ft_byte2     => return ARC.convert (field.native.v07);
         when ft_byte3     => return ARC.convert (field.native.v08);
         when ft_byte4     => return ARC.convert (field.native.v09);
         when ft_byte8     => return ARC.convert (field.native.v10);
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when ft_real9     |
              ft_real18    |
              ft_timestamp |
              ft_chain     |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_nbyte0;

   -----------------
   --  as_nbyte1  --
   -----------------
   function as_nbyte1 (field : map_field) return AD.nbyte1
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return ARC.convert (field.native.v00);
         when ft_nbyte1    => return field.native.v01;
         when ft_nbyte2    => return ARC.convert (field.native.v02);
         when ft_nbyte3    => return ARC.convert (field.native.v03);
         when ft_nbyte4    => return ARC.convert (field.native.v04);
         when ft_nbyte8    => return ARC.convert (field.native.v05);
         when ft_byte1     => return ARC.convert (field.native.v06);
         when ft_byte2     => return ARC.convert (field.native.v07);
         when ft_byte3     => return ARC.convert (field.native.v08);
         when ft_byte4     => return ARC.convert (field.native.v09);
         when ft_byte8     => return ARC.convert (field.native.v10);
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when ft_real9     |
              ft_real18    |
              ft_timestamp |
              ft_chain     |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_nbyte1;


   -----------------
   --  as_nbyte2  --
   -----------------
   function as_nbyte2 (field : map_field) return AD.nbyte2
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return ARC.convert (field.native.v00);
         when ft_nbyte1    => return ARC.convert (field.native.v01);
         when ft_nbyte2    => return field.native.v02;
         when ft_nbyte3    => return ARC.convert (field.native.v03);
         when ft_nbyte4    => return ARC.convert (field.native.v04);
         when ft_nbyte8    => return ARC.convert (field.native.v05);
         when ft_byte1     => return ARC.convert (field.native.v06);
         when ft_byte2     => return ARC.convert (field.native.v07);
         when ft_byte3     => return ARC.convert (field.native.v08);
         when ft_byte4     => return ARC.convert (field.native.v09);
         when ft_byte8     => return ARC.convert (field.native.v10);
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when ft_real9     |
              ft_real18    |
              ft_timestamp |
              ft_chain     |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_nbyte2;


   -----------------
   --  as_nbyte3  --
   -----------------
   function as_nbyte3 (field : map_field) return AD.nbyte3
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return ARC.convert (field.native.v00);
         when ft_nbyte1    => return ARC.convert (field.native.v01);
         when ft_nbyte2    => return ARC.convert (field.native.v02);
         when ft_nbyte3    => return field.native.v03;
         when ft_nbyte4    => return ARC.convert (field.native.v04);
         when ft_nbyte8    => return ARC.convert (field.native.v05);
         when ft_byte1     => return ARC.convert (field.native.v06);
         when ft_byte2     => return ARC.convert (field.native.v07);
         when ft_byte3     => return ARC.convert (field.native.v08);
         when ft_byte4     => return ARC.convert (field.native.v09);
         when ft_byte8     => return ARC.convert (field.native.v10);
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when ft_real9     |
              ft_real18    |
              ft_timestamp |
              ft_chain     |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_nbyte3;


   -----------------
   --  as_nbyte4  --
   -----------------
   function as_nbyte4 (field : map_field) return AD.nbyte4
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return ARC.convert (field.native.v00);
         when ft_nbyte1    => return ARC.convert (field.native.v01);
         when ft_nbyte2    => return ARC.convert (field.native.v02);
         when ft_nbyte3    => return ARC.convert (field.native.v03);
         when ft_nbyte4    => return field.native.v04;
         when ft_nbyte8    => return ARC.convert (field.native.v05);
         when ft_byte1     => return ARC.convert (field.native.v06);
         when ft_byte2     => return ARC.convert (field.native.v07);
         when ft_byte3     => return ARC.convert (field.native.v08);
         when ft_byte4     => return ARC.convert (field.native.v09);
         when ft_byte8     => return ARC.convert (field.native.v10);
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when ft_real9     |
              ft_real18    |
              ft_timestamp |
              ft_chain     |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_nbyte4;


   -----------------
   --  as_nbyte8  --
   -----------------
   function as_nbyte8 (field : map_field) return AD.nbyte8
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return ARC.convert (field.native.v00);
         when ft_nbyte1    => return ARC.convert (field.native.v01);
         when ft_nbyte2    => return ARC.convert (field.native.v02);
         when ft_nbyte3    => return ARC.convert (field.native.v03);
         when ft_nbyte4    => return ARC.convert (field.native.v04);
         when ft_nbyte8    => return field.native.v05;
         when ft_byte1     => return ARC.convert (field.native.v06);
         when ft_byte2     => return ARC.convert (field.native.v07);
         when ft_byte3     => return ARC.convert (field.native.v08);
         when ft_byte4     => return ARC.convert (field.native.v09);
         when ft_byte8     => return ARC.convert (field.native.v10);
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when ft_real9     |
              ft_real18    |
              ft_timestamp |
              ft_chain     |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_nbyte8;


   ----------------
   --  as_byte1  --
   ----------------
   function as_byte1 (field : map_field) return AD.byte1
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return ARC.convert (field.native.v00);
         when ft_nbyte1    => return ARC.convert (field.native.v01);
         when ft_nbyte2    => return ARC.convert (field.native.v02);
         when ft_nbyte3    => return ARC.convert (field.native.v03);
         when ft_nbyte4    => return ARC.convert (field.native.v04);
         when ft_nbyte8    => return ARC.convert (field.native.v05);
         when ft_byte1     => return field.native.v06;
         when ft_byte2     => return ARC.convert (field.native.v07);
         when ft_byte3     => return ARC.convert (field.native.v08);
         when ft_byte4     => return ARC.convert (field.native.v09);
         when ft_byte8     => return ARC.convert (field.native.v10);
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when ft_real9     |
              ft_real18    |
              ft_timestamp |
              ft_chain     |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_byte1;


   ----------------
   --  as_byte2  --
   ----------------
   function as_byte2 (field : map_field) return AD.byte2
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return ARC.convert (field.native.v00);
         when ft_nbyte1    => return ARC.convert (field.native.v01);
         when ft_nbyte2    => return ARC.convert (field.native.v02);
         when ft_nbyte3    => return ARC.convert (field.native.v03);
         when ft_nbyte4    => return ARC.convert (field.native.v04);
         when ft_nbyte8    => return ARC.convert (field.native.v05);
         when ft_byte1     => return ARC.convert (field.native.v06);
         when ft_byte2     => return field.native.v07;
         when ft_byte3     => return ARC.convert (field.native.v08);
         when ft_byte4     => return ARC.convert (field.native.v09);
         when ft_byte8     => return ARC.convert (field.native.v10);
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when ft_real9     |
              ft_real18    |
              ft_timestamp |
              ft_chain     |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_byte2;


   ----------------
   --  as_byte3  --
   ----------------
   function as_byte3 (field : map_field) return AD.byte3
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return ARC.convert (field.native.v00);
         when ft_nbyte1    => return ARC.convert (field.native.v01);
         when ft_nbyte2    => return ARC.convert (field.native.v02);
         when ft_nbyte3    => return ARC.convert (field.native.v03);
         when ft_nbyte4    => return ARC.convert (field.native.v04);
         when ft_nbyte8    => return ARC.convert (field.native.v05);
         when ft_byte1     => return ARC.convert (field.native.v06);
         when ft_byte2     => return ARC.convert (field.native.v07);
         when ft_byte3     => return field.native.v08;
         when ft_byte4     => return ARC.convert (field.native.v09);
         when ft_byte8     => return ARC.convert (field.native.v10);
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when ft_real9     |
              ft_real18    |
              ft_timestamp |
              ft_chain     |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_byte3;


   ----------------
   --  as_byte4  --
   ----------------
   function as_byte4 (field : map_field) return AD.byte4
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return ARC.convert (field.native.v00);
         when ft_nbyte1    => return ARC.convert (field.native.v01);
         when ft_nbyte2    => return ARC.convert (field.native.v02);
         when ft_nbyte3    => return ARC.convert (field.native.v03);
         when ft_nbyte4    => return ARC.convert (field.native.v04);
         when ft_nbyte8    => return ARC.convert (field.native.v05);
         when ft_byte1     => return ARC.convert (field.native.v06);
         when ft_byte2     => return ARC.convert (field.native.v07);
         when ft_byte3     => return ARC.convert (field.native.v08);
         when ft_byte4     => return field.native.v09;
         when ft_byte8     => return ARC.convert (field.native.v10);
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when ft_real9     |
              ft_real18    |
              ft_timestamp |
              ft_chain     |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_byte4;


   ----------------
   --  as_byte8  --
   ----------------
   function as_byte8 (field : map_field) return AD.byte8
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return ARC.convert (field.native.v00);
         when ft_nbyte1    => return ARC.convert (field.native.v01);
         when ft_nbyte2    => return ARC.convert (field.native.v02);
         when ft_nbyte3    => return ARC.convert (field.native.v03);
         when ft_nbyte4    => return ARC.convert (field.native.v04);
         when ft_nbyte8    => return ARC.convert (field.native.v05);
         when ft_byte1     => return ARC.convert (field.native.v06);
         when ft_byte2     => return ARC.convert (field.native.v07);
         when ft_byte3     => return ARC.convert (field.native.v08);
         when ft_byte4     => return ARC.convert (field.native.v09);
         when ft_byte8     => return field.native.v10;
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when ft_real9     |
              ft_real18    |
              ft_timestamp |
              ft_chain     |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_byte8;


   ----------------
   --  as_real9  --
   ----------------
   function as_real9 (field : map_field) return AD.real9
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return ARC.convert (field.native.v00);
         when ft_nbyte1    => return ARC.convert (field.native.v01);
         when ft_nbyte2    => return ARC.convert (field.native.v02);
         when ft_nbyte3    => return ARC.convert (field.native.v03);
         when ft_nbyte4    => return ARC.convert (field.native.v04);
         when ft_nbyte8    => return ARC.convert (field.native.v05);
         when ft_byte1     => return ARC.convert (field.native.v06);
         when ft_byte2     => return ARC.convert (field.native.v07);
         when ft_byte3     => return ARC.convert (field.native.v08);
         when ft_byte4     => return ARC.convert (field.native.v09);
         when ft_byte8     => return ARC.convert (field.native.v10);
         when ft_real9     => return field.native.v11;
         when ft_real18    => return ARC.convert (field.native.v12);
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when ft_timestamp |
              ft_chain     |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_real9;


   -----------------
   --  as_real18  --
   -----------------
   function as_real18 (field : map_field) return AD.real18
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return ARC.convert (field.native.v00);
         when ft_nbyte1    => return ARC.convert (field.native.v01);
         when ft_nbyte2    => return ARC.convert (field.native.v02);
         when ft_nbyte3    => return ARC.convert (field.native.v03);
         when ft_nbyte4    => return ARC.convert (field.native.v04);
         when ft_nbyte8    => return ARC.convert (field.native.v05);
         when ft_byte1     => return ARC.convert (field.native.v06);
         when ft_byte2     => return ARC.convert (field.native.v07);
         when ft_byte3     => return ARC.convert (field.native.v08);
         when ft_byte4     => return ARC.convert (field.native.v09);
         when ft_byte8     => return ARC.convert (field.native.v10);
         when ft_real9     => return ARC.convert (field.native.v11);
         when ft_real18    => return field.native.v12;
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when ft_timestamp |
              ft_chain     |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_real18;


   -----------------
   --  as_string  --
   -----------------
   function as_string (field : map_field) return String
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return ARC.convert (field.native.v00);
         when ft_nbyte1    => return ARC.convert (field.native.v01);
         when ft_nbyte2    => return ARC.convert (field.native.v02);
         when ft_nbyte3    => return ARC.convert (field.native.v03);
         when ft_nbyte4    => return ARC.convert (field.native.v04);
         when ft_nbyte8    => return ARC.convert (field.native.v05);
         when ft_byte1     => return ARC.convert (field.native.v06);
         when ft_byte2     => return ARC.convert (field.native.v07);
         when ft_byte3     => return ARC.convert (field.native.v08);
         when ft_byte4     => return ARC.convert (field.native.v09);
         when ft_byte8     => return ARC.convert (field.native.v10);
         when ft_real9     => return ARC.convert (field.native.v11);
         when ft_real18    => return ARC.convert (field.native.v12);
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when ft_timestamp => return ARC.convert (field.native.v16);
         when ft_enumtype  => return ARC.convert (field.native.v18);
         when ft_chain     |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_string;


   ------------------
   --  as_wstring  --
   ------------------
   function as_wstring (field : map_field) return Wide_String
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return ARC.convert (field.native.v00);
         when ft_nbyte1    => return ARC.convert (field.native.v01);
         when ft_nbyte2    => return ARC.convert (field.native.v02);
         when ft_nbyte3    => return ARC.convert (field.native.v03);
         when ft_nbyte4    => return ARC.convert (field.native.v04);
         when ft_nbyte8    => return ARC.convert (field.native.v05);
         when ft_byte1     => return ARC.convert (field.native.v06);
         when ft_byte2     => return ARC.convert (field.native.v07);
         when ft_byte3     => return ARC.convert (field.native.v08);
         when ft_byte4     => return ARC.convert (field.native.v09);
         when ft_byte8     => return ARC.convert (field.native.v10);
         when ft_real9     => return ARC.convert (field.native.v11);
         when ft_real18    => return ARC.convert (field.native.v12);
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when ft_timestamp => return ARC.convert (field.native.v16);
         when ft_enumtype  => return ARC.convert (field.native.v18);
         when ft_chain     |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_wstring;


   -------------------
   --  as_wwstring  --
   -------------------
   function as_wwstring (field : map_field) return Wide_Wide_String
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return ARC.convert (field.native.v00);
         when ft_nbyte1    => return ARC.convert (field.native.v01);
         when ft_nbyte2    => return ARC.convert (field.native.v02);
         when ft_nbyte3    => return ARC.convert (field.native.v03);
         when ft_nbyte4    => return ARC.convert (field.native.v04);
         when ft_nbyte8    => return ARC.convert (field.native.v05);
         when ft_byte1     => return ARC.convert (field.native.v06);
         when ft_byte2     => return ARC.convert (field.native.v07);
         when ft_byte3     => return ARC.convert (field.native.v08);
         when ft_byte4     => return ARC.convert (field.native.v09);
         when ft_byte8     => return ARC.convert (field.native.v10);
         when ft_real9     => return ARC.convert (field.native.v11);
         when ft_real18    => return ARC.convert (field.native.v12);
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when ft_timestamp => return ARC.convert (field.native.v16);
         when ft_enumtype  => return ARC.convert (field.native.v18);
         when ft_chain     |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_wwstring;


   -----------------
   --  as_time  --
   -----------------
   function as_time (field : map_field) return AC.Time
   is
      --  Looks like nothing can be converted to a time type so far
   begin
      case field.native.datatype is
         when ft_timestamp => return field.native.v16;
         when others => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_time;


   ----------------
   --  as_chain  --
   ----------------
   function as_chain (field : map_field) return AD.chain
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return ARC.convert (field.native.v00);
         when ft_nbyte1    => return ARC.convert (field.native.v01);
         when ft_nbyte2    => return ARC.convert (field.native.v02);
         when ft_nbyte3    => return ARC.convert (field.native.v03);
         when ft_nbyte4    => return ARC.convert (field.native.v04);
         when ft_nbyte8    => return ARC.convert (field.native.v05);
         when ft_chain     => return field.native.v17.all;
         when others => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_chain;


   -------------------
   --  as_enumtype  --
   -------------------
   function as_enumtype (field : map_field) return AD.enumtype
   is
   begin
      case field.native.datatype is
         when ft_enumtype  => return field.native.v18;
         when others => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_enumtype;


   -----------------
   --  as_settype  --
   -----------------
   function as_settype (field : map_field) return AD.settype
   is
   begin
      case field.native.datatype is
         when ft_settype   => return field.native.v19.all;
         when others => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_settype;


   ---------------
   --  is_null  --
   ---------------
   function is_null (field : map_field) return Boolean is
   begin
      return field.explicit_null;
   end is_null;


   -------------------
   --  native_type  --
   -------------------
   function native_type (field : map_field) return field_types is
   begin
      return field.native.datatype;
   end native_type;


end AdaBase.Results.Field;
