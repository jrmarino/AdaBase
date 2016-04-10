--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

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

   generic
      type IntType is (<>);
   function convert4str (nv : String) return IntType;

   generic
      type RealType is digits <>;
   function convert4st2 (nv : String) return RealType;

private

   function ctrim (raw : String) return String;
   function wtrim (raw : String) return Wide_String;
   function strim (raw : String) return Wide_Wide_String;

end AdaBase.Results.Generic_Converters;
