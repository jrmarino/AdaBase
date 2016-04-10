--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

private
with Ada.Finalization;

package AdaBase.Statement is

   pragma Pure;

   type Base_Pure is abstract tagged limited private;

private
   package FIN renames Ada.Finalization;

   type Base_Pure is abstract new FIN.Limited_Controlled with
      record
         null;
      end record;

end AdaBase.Statement;
