--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Text_IO.Unbounded_IO;

package body AdaBase.Logger.Base.File is

   package TIO renames Ada.Text_IO;
   package UIO renames Ada.Text_IO.Unbounded_IO;

   overriding
   procedure reaction (listener : File_Logger)
   is
      output : TIO.File_Type;
   begin
      if CT.IsBlank (listener.filepath) then
         return;
      end if;

      begin
         TIO.Open (File => output,
                   Mode => TIO.Append_File,
                   Name => CT.USS (listener.filepath));
      exception
         when TIO.Name_Error =>
            TIO.Create (File => output,
                        Mode => TIO.Out_File,
                        Name => CT.USS (listener.filepath));
      end;

      UIO.Put_Line (File => output,
                    Item => listener.composite);

      TIO.Close (File => output);

   end reaction;


   --------------------
   --  set_filepath  --
   --------------------
   procedure set_filepath (listener : out File_Logger; filename : String) is
   begin
      listener.filepath := CT.SUS (filename);
   end set_filepath;

end AdaBase.Logger.Base.File;
