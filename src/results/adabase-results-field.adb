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

package body AdaBase.Results.Field is




   function as_byte1 (field : map_field) return AD.byte1
   is
      use type AD.byte1;
   begin
      case field.native.datatype is
         when ft_nbyte0 =>
            if field.native.v00 then
               return 1;
            else
               return 0;
            end if;
         when ft_byte1 => return field.native.v06;
         when ft_byte2 =>
            if field.native.v07 < AD.byte1'First or else
              field.native.v07 > AD.byte1'Last
            then
               raise TARGET_TYPE_TOO_NARROW;
            else
               return AD.byte1 (field.native.v07);
            end if;
         when others => return 0;
      end case;
   end as_byte1;


end AdaBase.Results.Field;
