---  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package AdaBase.Results.Field is

   type Std_Field is tagged private;
   type field_access is access Std_Field;

   type variant (datatype : field_types := ft_nbyte8) is
      record
         case datatype is
            when ft_nbyte0    => v00 : Boolean;
            when ft_nbyte1    => v01 : nbyte1;
            when ft_nbyte2    => v02 : nbyte2;
            when ft_nbyte3    => v03 : nbyte3;
            when ft_nbyte4    => v04 : nbyte4;
            when ft_nbyte8    => v05 : nbyte8;
            when ft_byte1     => v06 : byte1;
            when ft_byte2     => v07 : byte2;
            when ft_byte3     => v08 : byte3;
            when ft_byte4     => v09 : byte4;
            when ft_byte8     => v10 : byte8;
            when ft_real9     => v11 : real9;
            when ft_real18    => v12 : real18;
            when ft_textual   => v13 : textual;
            when ft_widetext  => v14 : textwide;
            when ft_supertext => v15 : textsuper;
            when ft_timestamp => v16 : AC.Time;
            when ft_chain     => v17 : textual;
            when ft_enumtype  => v18 : enumtype;
            when ft_settype   => v19 : textual;
         end case;
      end record;

   function as_nbyte0   (field : Std_Field) return nbyte0;
   function as_nbyte1   (field : Std_Field) return nbyte1;
   function as_nbyte2   (field : Std_Field) return nbyte2;
   function as_nbyte3   (field : Std_Field) return nbyte3;
   function as_nbyte4   (field : Std_Field) return nbyte4;
   function as_nbyte8   (field : Std_Field) return nbyte8;

   function as_byte1    (field : Std_Field) return byte1;
   function as_byte2    (field : Std_Field) return byte2;
   function as_byte3    (field : Std_Field) return byte3;
   function as_byte4    (field : Std_Field) return byte4;
   function as_byte8    (field : Std_Field) return byte8;

   function as_real9    (field : Std_Field) return real9;
   function as_real18   (field : Std_Field) return real18;

   function as_string   (field : Std_Field) return String;
   function as_wstring  (field : Std_Field) return Wide_String;
   function as_wwstring (field : Std_Field) return Wide_Wide_String;

   function as_time     (field : Std_Field) return AC.Time;
   function as_chain    (field : Std_Field) return chain;
   function as_enumtype (field : Std_Field) return enumtype;
   function as_settype  (field : Std_Field) return settype;

   function is_null     (field : Std_Field) return Boolean;
   function native_type (field : Std_Field) return field_types;

   function spawn_field (data : variant; null_data : Boolean := False)
                         return Std_Field;
   function spawn_field (binob : chain) return Std_Field;
   function spawn_field (enumset : String) return Std_Field;

private

   type Std_Field is tagged record
      native        : variant;
      explicit_null : Boolean := False;
   end record;

   procedure set (field : out Std_Field; data : variant; exnull : Boolean);

end AdaBase.Results.Field;
