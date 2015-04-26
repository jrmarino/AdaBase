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


package AdaBase.Results.Field is

   type std_field is tagged private;
   type field_access is access std_field;

   type variant (datatype : field_types := ft_nbyte8) is
      record
         case datatype is
            when ft_nbyte0    => v00 : Boolean;
            when ft_nbyte1    => v01 : AD.nbyte1;
            when ft_nbyte2    => v02 : AD.nbyte2;
            when ft_nbyte3    => v03 : AD.nbyte3;
            when ft_nbyte4    => v04 : AD.nbyte4;
            when ft_nbyte8    => v05 : AD.nbyte8;
            when ft_byte1     => v06 : AD.byte1;
            when ft_byte2     => v07 : AD.byte2;
            when ft_byte3     => v08 : AD.byte3;
            when ft_byte4     => v09 : AD.byte4;
            when ft_byte8     => v10 : AD.byte8;
            when ft_real9     => v11 : AD.real9;
            when ft_real18    => v12 : AD.real18;
            when ft_textual   => v13 : AD.textual;
            when ft_widetext  => v14 : textwide;
            when ft_supertext => v15 : textsuper;
            when ft_timestamp => v16 : AC.Time;
            when ft_chain     => v17 : AD.chain_access;
            when ft_enumtype  => v18 : AD.enumtype;
            when ft_settype   => v19 : AD.settype_access;
         end case;
      end record;

   function as_nbyte0   (field : std_field) return AD.nbyte0;
   function as_nbyte1   (field : std_field) return AD.nbyte1;
   function as_nbyte2   (field : std_field) return AD.nbyte2;
   function as_nbyte3   (field : std_field) return AD.nbyte3;
   function as_nbyte4   (field : std_field) return AD.nbyte4;
   function as_nbyte8   (field : std_field) return AD.nbyte8;

   function as_byte1    (field : std_field) return AD.byte1;
   function as_byte2    (field : std_field) return AD.byte2;
   function as_byte3    (field : std_field) return AD.byte3;
   function as_byte4    (field : std_field) return AD.byte4;
   function as_byte8    (field : std_field) return AD.byte8;

   function as_real9    (field : std_field) return AD.real9;
   function as_real18   (field : std_field) return AD.real18;

   function as_string   (field : std_field) return String;
   function as_wstring  (field : std_field) return Wide_String;
   function as_wwstring (field : std_field) return Wide_Wide_String;

   function as_time     (field : std_field) return AC.Time;
   function as_chain    (field : std_field) return AD.chain;
   function as_enumtype (field : std_field) return AD.enumtype;
   function as_settype  (field : std_field) return AD.settype;

   function is_null     (field : std_field) return Boolean;
   function native_type (field : std_field) return field_types;

   function spawn_field (data : variant; null_data : Boolean := False)
                         return field_access;

   function spawn_field (binob : AD.chain) return field_access;

   function spawn_field (enumset : AD.settype) return field_access;

private

   type std_field is tagged record
      native        : variant;
      explicit_null : Boolean := False;
   end record;

   procedure set (field : out std_field; data : variant; exnull : Boolean);

end AdaBase.Results.Field;
