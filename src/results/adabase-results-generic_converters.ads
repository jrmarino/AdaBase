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

package AdaBase.Results.Generic_Converters is

   generic
      type IntType is (<>);
   function convertstr (nv : String) return IntType;

   generic
      type RealType is digits <>;
   function convertst2 (nv : String) return RealType;

   generic
      type IntType is (<>);
   function convertst3 (nv : Wide_String) return IntType;

   generic
      type RealType is digits <>;
   function convertst4 (nv : Wide_String) return RealType;

   generic
      type IntType is (<>);
   function convertst5 (nv : Wide_Wide_String) return IntType;

   generic
      type RealType is digits <>;
   function convertst6 (nv : Wide_Wide_String) return RealType;

   generic
      type IntType is (<>);
   function convert2str1 (nv : IntType) return AD.textual;

   generic
      type IntType is (<>);
   function convert2str2 (nv : IntType) return textwide;

   generic
      type IntType is (<>);
   function convert2str3 (nv : IntType) return textsuper;

   generic
      type RealType is digits <>;
   function convert3str1 (nv : RealType) return AD.textual;

   generic
      type RealType is digits <>;
   function convert3str2 (nv : RealType) return textwide;

   generic
      type RealType is digits <>;
   function convert3str3 (nv : RealType) return textsuper;

private

   function ctrim (raw : String) return AD.textual;
   function wtrim (raw : Wide_String) return textwide;
   function strim (raw : Wide_Wide_String) return textsuper;

end AdaBase.Results.Generic_Converters;
