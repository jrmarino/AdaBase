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

with AdaBase.DataTypes;
with Ada.Calendar;
with Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;

package AdaBase.Results is

   package AD   renames AdaBase.DataTypes;
   package AC   renames Ada.Calendar;
   package SUW  renames Ada.Strings.Wide_Unbounded;
   package SUWW renames Ada.Strings.Wide_Wide_Unbounded;

   type field_types is (ft_nbyte0, ft_nbyte1, ft_nbyte2, ft_nbyte3, ft_nbyte4,
                        ft_nbyte8, ft_byte1, ft_byte2, ft_byte3, ft_byte4,
                        ft_byte8, ft_real9, ft_real18, ft_textual,
                        ft_widetext, ft_supertext, ft_timestamp,
                        ft_chain, ft_enumtype, ft_settype);
   subtype textwide  is SUW.Unbounded_Wide_String;
   subtype textsuper is SUWW.Unbounded_Wide_Wide_String;

   type variant (datatype : field_types) is
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

   TARGET_TYPE_TOO_NARROW : exception;
   CONVERSION_FAILED      : exception;

end AdaBase.Results;
