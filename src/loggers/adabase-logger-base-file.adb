--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Text_IO;

package body AdaBase.Logger.Base.File is

   package TIO renames Ada.Text_IO;

   overriding
   procedure reaction (listener : File_Logger)
   is
      use type logtext;
      output : TIO.File_Type;
   begin
      if listener.filepath = blank then
         return;
      end if;

      begin
         TIO.Open (File => output,
                   Mode => TIO.Append_File,
                   Name => SU.To_String (listener.filepath));
      exception
         when TIO.Name_Error =>
            TIO.Create (File => output,
                        Mode => TIO.Out_File,
                        Name => SU.To_String (listener.filepath));
      end;

      TIO.Put_Line (File => output,
                   Item => SU.To_String (listener.composite));

      TIO.Close (File => output);

   end reaction;

end AdaBase.Logger.Base.File;
