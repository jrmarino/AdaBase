---  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package AdaBase.Results.Field is

   type std_field is tagged private;
   type field_access is access std_field;

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
            when ft_chain     => v17 : chain_access;
            when ft_enumtype  => v18 : enumtype;
            when ft_settype   => v19 : settype_access;
         end case;
      end record;

   function as_nbyte0   (field : std_field) return nbyte0;
   function as_nbyte1   (field : std_field) return nbyte1;
   function as_nbyte2   (field : std_field) return nbyte2;
   function as_nbyte3   (field : std_field) return nbyte3;
   function as_nbyte4   (field : std_field) return nbyte4;
   function as_nbyte8   (field : std_field) return nbyte8;

   function as_byte1    (field : std_field) return byte1;
   function as_byte2    (field : std_field) return byte2;
   function as_byte3    (field : std_field) return byte3;
   function as_byte4    (field : std_field) return byte4;
   function as_byte8    (field : std_field) return byte8;

   function as_real9    (field : std_field) return real9;
   function as_real18   (field : std_field) return real18;

   function as_string   (field : std_field) return String;
   function as_wstring  (field : std_field) return Wide_String;
   function as_wwstring (field : std_field) return Wide_Wide_String;

   function as_time     (field : std_field) return AC.Time;
   function as_chain    (field : std_field) return chain;
   function as_enumtype (field : std_field) return enumtype;
   function as_settype  (field : std_field) return settype;

   function is_null     (field : std_field) return Boolean;
   function native_type (field : std_field) return field_types;

   function spawn_field (data : variant; null_data : Boolean := False)
                         return field_access;
   function spawn_field (binob : chain) return field_access;
   function spawn_field (enumset : settype) return field_access;

private

   type std_field is tagged record
      native        : variant;
      explicit_null : Boolean := False;
   end record;

   procedure set (field : out std_field; data : variant; exnull : Boolean);

end AdaBase.Results.Field;
