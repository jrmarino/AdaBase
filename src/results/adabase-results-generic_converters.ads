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
   function convertstr (nv : textual) return IntType;

   generic
      type RealType is digits <>;
   function convertst2 (nv : textual) return RealType;

   generic
      type IntType is (<>);
   function convertst3 (nv : textwide) return IntType;

   generic
      type RealType is digits <>;
   function convertst4 (nv : textwide) return RealType;

   generic
      type IntType is (<>);
   function convertst5 (nv : textsuper) return IntType;

   generic
      type RealType is digits <>;
   function convertst6 (nv : textsuper) return RealType;

   generic
      type IntType is (<>);
   function convert2str1 (nv : IntType) return String;

   generic
      type IntType is (<>);
   function convert2str2 (nv : IntType) return Wide_String;

   generic
      type IntType is (<>);
   function convert2str3 (nv : IntType) return Wide_Wide_String;

   generic
      type RealType is digits <>;
   function convert3str1 (nv : RealType) return String;

   generic
      type RealType is digits <>;
   function convert3str2 (nv : RealType) return Wide_String;

   generic
      type RealType is digits <>;
   function convert3str3 (nv : RealType) return Wide_Wide_String;

private

   function ctrim (raw : String) return String;
   function wtrim (raw : String) return Wide_String;
   function strim (raw : String) return Wide_Wide_String;

end AdaBase.Results.Generic_Converters;
