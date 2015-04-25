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

   type map_field is abstract tagged limited private;

   function as_nbyte0   (field : map_field) return AD.nbyte0;
   function as_nbyte1   (field : map_field) return AD.nbyte1;
   function as_nbyte2   (field : map_field) return AD.nbyte2;
   function as_nbyte3   (field : map_field) return AD.nbyte3;
   function as_nbyte4   (field : map_field) return AD.nbyte4;
   function as_nbyte8   (field : map_field) return AD.nbyte8;

   function as_byte1    (field : map_field) return AD.byte1;
   function as_byte2    (field : map_field) return AD.byte2;
   function as_byte3    (field : map_field) return AD.byte3;
   function as_byte4    (field : map_field) return AD.byte4;
   function as_byte8    (field : map_field) return AD.byte8;

   function as_real9    (field : map_field) return AD.real9;
   function as_real18   (field : map_field) return AD.real18;

   function as_string   (field : map_field) return String;
   function as_wstring  (field : map_field) return Wide_String;
   function as_wwstring (field : map_field) return Wide_Wide_String;

   function as_time     (field : map_field) return AC.Time;
   function as_chain    (field : map_field) return AD.chain;
   function as_enumtype (field : map_field) return AD.enumtype;
   function as_settype  (field : map_field) return AD.settype;

   function is_null     (field : map_field) return Boolean;
   function native_type (field : map_field) return field_types;

private

   type map_field is abstract tagged limited record
      native        : variant;
      explicit_null : Boolean := False;
   end record;

end AdaBase.Results.Field;
