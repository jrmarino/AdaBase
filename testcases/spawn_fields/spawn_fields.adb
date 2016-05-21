with AdaBase.Results.Field;
with AdaBase.Results.Converters;
with Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Wide_Text_IO;

procedure Spawn_Fields is
   package TIO renames Ada.Text_IO;
   package WIO renames Ada.Wide_Text_IO;
   package IIO renames Ada.Integer_Text_IO;
   package SU  renames Ada.Strings.Unbounded;
   package AR  renames AdaBase.Results;
   package ARC renames AdaBase.Results.Converters;

   SF : AR.Field.Std_Field :=
        AR.Field.spawn_field (binob => (50, 15, 4, 8));

   BR : AR.Field.Std_Field :=
        AR.Field.spawn_field (data =>
        (datatype => AdaBase.ft_textual,
              v13 => SU.To_Unbounded_String ("Baltimore Ravens")));

   myset : AR.Settype (1 .. 3) :=
               ((enumeration => SU.To_Unbounded_String ("hockey")),
                (enumeration => SU.To_Unbounded_String ("baseball")),
                (enumeration => SU.To_Unbounded_String ("tennis")));

   ST : AR.Field.Std_Field :=
        AR.Field.spawn_field (enumset => ARC.convert (myset));

   chain_len : Natural := SF.as_chain'Length;

begin
   TIO.Put_Line ("Chain #1 length:" & chain_len'Img);
   TIO.Put_Line ("Chain #1   type: " & SF.native_type'Img);
   for x in 1 .. chain_len loop
      TIO.Put ("  block" & x'Img & " value:" & SF.as_chain (x)'Img);
      IIO.Put (Item => Natural (SF.as_chain (x)), Base => 16);
      TIO.Put_Line ("");
   end loop;
   TIO.Put ("Chain #1 converted to 4-byte unsigned integer:" &
             SF.as_nbyte4'Img & "  ");
   IIO.Put (Item => Natural (SF.as_nbyte4), Base => 16);
   TIO.Put_Line ("");
   TIO.Put_Line ("");

   WIO.Put_Line ("Convert BR field to wide string: " & BR.as_wstring);
   TIO.Put_Line ("Convert ST settype to string:    " & ST.as_string);
   TIO.Put_Line ("Length of ST set:               " & myset'Length'Img);
end Spawn_Fields;
