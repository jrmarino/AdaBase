--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

private
with Ada.Finalization;

package AdaBase.Connection is

   pragma Pure;

   type Base_Pure is abstract tagged private;

private

   package FIN renames Ada.Finalization;

   type Base_Pure is abstract new FIN.Controlled with null record;

end AdaBase.Connection;
