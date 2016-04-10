--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Text_IO;

package body AdaBase.Logger.Base.Screen is

   package TIO renames Ada.Text_IO;

   overriding
   procedure reaction (listener : Screen_Logger)
   is
      use type SU.Unbounded_String;
   begin
      if listener.error_msg = blank then
         TIO.Put_Line (File => TIO.Standard_Output,
                       Item => SU.To_String (listener.composite));
      else
         TIO.Put_Line (File => TIO.Standard_Error,
                       Item => SU.To_String (listener.composite));
      end if;
   end reaction;

end AdaBase.Logger.Base.Screen;
