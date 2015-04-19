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

with Ada.Text_IO;

package body AdaBase.Logger.Base.File is

   package TIO renames Ada.Text_IO;

   overriding
   procedure reaction (listener : File_Logger)
   is
      use type AD.textual;
      output : TIO.File_Type;
   begin
      if listener.filepath = AD.blank then
         return;
      end if;

      begin
         TIO.Open (File => output,
                   Mode => TIO.Append_File,
                   Name => AD.SU.To_String (listener.filepath));
      exception
         when TIO.Name_Error =>
            TIO.Create (File => output,
                        Mode => TIO.Out_File,
                        Name => AD.SU.To_String (listener.filepath));
      end;

      TIO.Put_Line (File => output,
                   Item => AD.SU.To_String (listener.composite));

      TIO.Close (File => output);

   end reaction;

end AdaBase.Logger.Base.File;
