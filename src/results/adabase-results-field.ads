---  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package AdaBase.Results.Field is

   type Std_Field is tagged private;
   type Field_Access is access Std_Field;

   type Variant (datatype : field_types := ft_nbyte8) is
      record
         case datatype is
            when ft_nbyte0    => v00 : Boolean;
            when ft_nbyte1    => v01 : NByte1;
            when ft_nbyte2    => v02 : NByte2;
            when ft_nbyte3    => v03 : NByte3;
            when ft_nbyte4    => v04 : NByte4;
            when ft_nbyte8    => v05 : NByte8;
            when ft_byte1     => v06 : Byte1;
            when ft_byte2     => v07 : Byte2;
            when ft_byte3     => v08 : Byte3;
            when ft_byte4     => v09 : Byte4;
            when ft_byte8     => v10 : Byte8;
            when ft_real9     => v11 : Real9;
            when ft_real18    => v12 : Real18;
            when ft_textual   => v13 : Textual;
            when ft_widetext  => v14 : Textwide;
            when ft_supertext => v15 : Textsuper;
            when ft_timestamp => v16 : AC.Time;
            when ft_chain     => v17 : Textual;
            when ft_enumtype  => v18 : Enumtype;
            when ft_settype   => v19 : Textual;
            when ft_bits      => v20 : Textual;
            when ft_utf8      => v21 : Textual;
         end case;
      end record;

   function as_nbyte0   (field : Std_Field) return NByte0;
   function as_nbyte1   (field : Std_Field) return NByte1;
   function as_nbyte2   (field : Std_Field) return NByte2;
   function as_nbyte3   (field : Std_Field) return NByte3;
   function as_nbyte4   (field : Std_Field) return NByte4;
   function as_nbyte8   (field : Std_Field) return NByte8;

   function as_byte1    (field : Std_Field) return Byte1;
   function as_byte2    (field : Std_Field) return Byte2;
   function as_byte3    (field : Std_Field) return Byte3;
   function as_byte4    (field : Std_Field) return Byte4;
   function as_byte8    (field : Std_Field) return Byte8;

   function as_real9    (field : Std_Field) return Real9;
   function as_real18   (field : Std_Field) return Real18;

   function as_string   (field : Std_Field) return String;
   function as_wstring  (field : Std_Field) return Wide_String;
   function as_wwstring (field : Std_Field) return Wide_Wide_String;

   function as_time     (field : Std_Field) return AC.Time;
   function as_chain    (field : Std_Field) return Chain;
   function as_enumtype (field : Std_Field) return Enumtype;
   function as_settype  (field : Std_Field) return Settype;
   function as_bits     (field : Std_Field) return Bits;
   function as_utf8     (field : Std_Field) return Text_UTF8;

   function is_null     (field : Std_Field) return Boolean;
   function native_type (field : Std_Field) return field_types;

   function spawn_field (data : Variant; null_data : Boolean := False)
                         return Std_Field;
   function spawn_field (binob : Chain) return Std_Field;
   function spawn_field (enumset : String) return Std_Field;
   function spawn_bits_field (bitstring : String) return Std_Field;
   function spawn_null_field (data_type : field_types) return Std_Field;

private

   type Std_Field is tagged record
      native        : Variant;
      explicit_null : Boolean := False;
   end record;

   procedure set (field : out Std_Field; data : Variant; exnull : Boolean);

end AdaBase.Results.Field;
