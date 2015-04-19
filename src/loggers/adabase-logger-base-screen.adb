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

package body AdaBase.Logger.Base.Screen is

   package TIO renames Ada.Text_IO;

   overriding
   procedure reaction (listener : Screen_Logger)
   is
      use type AD.SU.Unbounded_String;
   begin
      if listener.error_msg = AD.blank then
         TIO.Put_Line (File => TIO.Standard_Output,
                       Item => AD.SU.To_String (listener.composite));
      else
         TIO.Put_Line (File => TIO.Standard_Error,
                       Item => AD.SU.To_String (listener.composite));
      end if;
   end reaction;

end AdaBase.Logger.Base.Screen;
