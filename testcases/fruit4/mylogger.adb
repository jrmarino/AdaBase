with Ada.Text_IO.Unbounded_IO;

package body MyLogger is

   package UIO renames Ada.Text_IO.Unbounded_IO;
   package TIO renames Ada.Text_IO;

   overriding
   procedure reaction (listener : CustomLogger) is
   begin
      if listener.is_error then
         TIO.Put_Line ("## SQLSTATE: " & listener.sqlstate);
         TIO.Put_Line ("##   Driver:"  & listener.error_code'Img &
                                    "(" & listener.driver'Img & ")");
         TIO.Put      ("##    Error: "); UIO.Put_Line (listener.error_msg);
         TIO.Put_Line ("##    Phase: " & listener.category'Img);
      else
         TIO.Put_Line ("##    Phase: " & listener.category'Img);
         TIO.Put      ("##  message: "); UIO.Put_Line (listener.message);
      end if;
   end reaction;

end MyLogger;
