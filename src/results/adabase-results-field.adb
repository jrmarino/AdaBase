--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Results.Converters;

package body AdaBase.Results.Field is

   package ARC renames AdaBase.Results.Converters;


   -----------------
   --  as_nbyte0  --
   -----------------
   function as_nbyte0 (field : Std_Field) return NByte0
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
         when ft_chain     =>
            declare
               cadena : Chain := ARC.convert (field.native.v17);
            begin
               return ARC.convert (cadena);
            end;
         when ft_real9     |
              ft_real18    |
              ft_timestamp |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_nbyte0;

   -----------------
   --  as_nbyte1  --
   -----------------
   function as_nbyte1 (field : Std_Field) return NByte1
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
         when ft_chain     =>
            declare
               cadena : Chain := ARC.convert (field.native.v17);
            begin
               return ARC.convert (cadena);
            end;
         when ft_real9     |
              ft_real18    |
              ft_timestamp |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_nbyte1;


   -----------------
   --  as_nbyte2  --
   -----------------
   function as_nbyte2 (field : Std_Field) return NByte2
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
         when ft_chain     =>
            declare
               cadena : Chain := ARC.convert (field.native.v17);
            begin
               return ARC.convert (cadena);
            end;
         when ft_real9     |
              ft_real18    |
              ft_timestamp |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_nbyte2;


   -----------------
   --  as_nbyte3  --
   -----------------
   function as_nbyte3 (field : Std_Field) return NByte3
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
         when ft_chain     =>
            declare
               cadena : Chain := ARC.convert (field.native.v17);
            begin
               return ARC.convert (cadena);
            end;
         when ft_real9     |
              ft_real18    |
              ft_timestamp |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_nbyte3;


   -----------------
   --  as_nbyte4  --
   -----------------
   function as_nbyte4 (field : Std_Field) return NByte4
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
         when ft_chain     =>
            declare
               cadena : Chain := ARC.convert (field.native.v17);
            begin
               return ARC.convert (cadena);
            end;
         when ft_real9     |
              ft_real18    |
              ft_timestamp |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_nbyte4;


   -----------------
   --  as_nbyte8  --
   -----------------
   function as_nbyte8 (field : Std_Field) return NByte8
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
         when ft_chain     =>
            declare
               cadena : Chain := ARC.convert (field.native.v17);
            begin
               return ARC.convert (cadena);
            end;
         when ft_real9     |
              ft_real18    |
              ft_timestamp |
              ft_enumtype  |
              ft_settype   => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_nbyte8;


   ----------------
   --  as_byte1  --
   ----------------
   function as_byte1 (field : Std_Field) return Byte1
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
   function as_byte2 (field : Std_Field) return Byte2
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
   function as_byte3 (field : Std_Field) return Byte3
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
   function as_byte4 (field : Std_Field) return Byte4
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
   function as_byte8 (field : Std_Field) return Byte8
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
   function as_real9 (field : Std_Field) return Real9
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
   function as_real18 (field : Std_Field) return Real18
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
   function as_string (field : Std_Field) return String
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
         when ft_chain     => return ARC.convert (field.native.v17);
         when ft_enumtype  => return ARC.convert (field.native.v18);
         when ft_settype   => return ARC.convert (field.native.v19);
      end case;
   end as_string;


   ------------------
   --  as_wstring  --
   ------------------
   function as_wstring (field : Std_Field) return Wide_String
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
         when ft_chain     => return ARC.convert (field.native.v17);
         when ft_enumtype  => return ARC.convert (field.native.v18);
         when ft_settype   => return ARC.convert (field.native.v19);
      end case;
   end as_wstring;


   -------------------
   --  as_wwstring  --
   -------------------
   function as_wwstring (field : Std_Field) return Wide_Wide_String
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
         when ft_chain     => return ARC.convert (field.native.v17);
         when ft_enumtype  => return ARC.convert (field.native.v18);
         when ft_settype   => return ARC.convert (field.native.v19);
      end case;
   end as_wwstring;


   -----------------
   --  as_time  --
   -----------------
   function as_time (field : Std_Field) return AC.Time
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
   function as_chain (field : Std_Field) return Chain
   is
   begin
      case field.native.datatype is
         when ft_nbyte0    => return ARC.convert (field.native.v00);
         when ft_nbyte1    => return ARC.convert (field.native.v01);
         when ft_nbyte2    => return ARC.convert (field.native.v02);
         when ft_nbyte3    => return ARC.convert (field.native.v03);
         when ft_nbyte4    => return ARC.convert (field.native.v04);
         when ft_nbyte8    => return ARC.convert (field.native.v05);
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when ft_chain     => return ARC.convert (field.native.v17);
         when others => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_chain;


   -------------------
   --  as_enumtype  --
   -------------------
   function as_enumtype (field : Std_Field) return Enumtype
   is
   begin
      case field.native.datatype is
         when ft_enumtype  => return field.native.v18;
         when ft_textual   => return ARC.convert (field.native.v13);
         when ft_widetext  => return ARC.convert (field.native.v14);
         when ft_supertext => return ARC.convert (field.native.v15);
         when others => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_enumtype;


   -----------------
   --  as_settype  --
   -----------------
   function as_settype (field : Std_Field) return Settype
   is
   begin
      case field.native.datatype is
         when ft_settype   => return ARC.convert (field.native.v19);
         when others => raise UNSUPPORTED_CONVERSION;
      end case;
   end as_settype;


   ---------------
   --  is_null  --
   ---------------
   function is_null (field : Std_Field) return Boolean is
   begin
      return field.explicit_null;
   end is_null;


   -------------------
   --  native_type  --
   -------------------
   function native_type (field : Std_Field) return field_types is
   begin
      return field.native.datatype;
   end native_type;


   ------------------------------
   --  CREATE STANDARD FIELDS  --
   ------------------------------
   function spawn_field (data : Variant; null_data : Boolean := False)
                         return Std_Field
   is
      result : Std_Field;
   begin
      result.set (data => data, exnull => null_data);
      return result;
   end spawn_field;

   function spawn_field (binob : Chain) return Std_Field
   is
      result  : Std_Field;
      chainstr : constant String := ARC.convert (binob);
   begin
      result.set (data => (ft_chain, CT.SUS (chainstr)), exnull => False);
      return result;
   end spawn_field;

   function spawn_field (enumset : String) return Std_Field
   is
      result  : Std_Field;
   begin
      result.set (data => (ft_settype, CT.SUS (enumset)), exnull => False);
      return result;
   end spawn_field;


   procedure set (field : out Std_Field; data : Variant; exnull : Boolean) is
   begin
      case data.datatype is
         when ft_nbyte0    => field.native := (ft_nbyte0, data.v00);
         when ft_nbyte1    => field.native := (ft_nbyte1, data.v01);
         when ft_nbyte2    => field.native := (ft_nbyte2, data.v02);
         when ft_nbyte3    => field.native := (ft_nbyte3, data.v03);
         when ft_nbyte4    => field.native := (ft_nbyte4, data.v04);
         when ft_nbyte8    => field.native := (ft_nbyte8, data.v05);
         when ft_byte1     => field.native := (ft_byte1, data.v06);
         when ft_byte2     => field.native := (ft_byte2, data.v07);
         when ft_byte3     => field.native := (ft_byte3, data.v08);
         when ft_byte4     => field.native := (ft_byte4, data.v09);
         when ft_byte8     => field.native := (ft_byte8, data.v10);
         when ft_real9     => field.native := (ft_real9, data.v11);
         when ft_real18    => field.native := (ft_real18, data.v12);
         when ft_textual   => field.native := (ft_textual, data.v13);
         when ft_widetext  => field.native := (ft_widetext, data.v14);
         when ft_supertext => field.native := (ft_supertext, data.v15);
         when ft_timestamp => field.native := (ft_timestamp, data.v16);
         when ft_chain     => field.native := (ft_chain, data.v17);
         when ft_enumtype  => field.native := (ft_enumtype, data.v18);
         when ft_settype   => field.native := (ft_settype, data.v19);
      end case;
      field.explicit_null := exnull;
   end set;


end AdaBase.Results.Field;
