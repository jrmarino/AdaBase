--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Text_IO.Unbounded_IO;

package body AdaBase.Logger.Base.Screen is

   package TIO renames Ada.Text_IO;
   package UIO renames Ada.Text_IO.Unbounded_IO;

   overriding
   procedure reaction (listener : Screen_Logger) is
   begin
      if listener.is_error then
         UIO.Put_Line (File => TIO.Standard_Error,
                       Item => listener.composite);
      else
         UIO.Put_Line (File => TIO.Standard_Output,
                       Item => listener.composite);
      end if;
   end reaction;

end AdaBase.Logger.Base.Screen;
