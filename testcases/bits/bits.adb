with AdaBase;
with CommonText;
with Connect;
with Ada.Text_IO;
with AdaBase.Results.Sets;
with AdaBase.Results.Converters;

procedure Bits is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ARS renames AdaBase.Results.Sets;
   package ARC renames AdaBase.Results.Converters;
   package CT  renames CommonText;

   cmd : constant String := "SELECT bit_type FROM all_types " &
                            "WHERE id_nbyte3 = 1";

begin

   CON.connect_database;

   declare
      stmt : CON.Stmt_Type := CON.DR.query (sql => cmd);
      row  : ARS.Datarow;
   begin
      if stmt.successful then
         row := stmt.fetch_next;
         TIO.Put_Line ("type   : " & row.column (1).native_type'Img);
         TIO.Put_Line ("string : " & row.column (1).as_string);
         declare
            mybits  : AdaBase.Results.Bits := row.column (1).as_bits;
            mychain : AdaBase.Results.Chain := row.column (1).as_chain;
         begin
            for x in mybits'Range loop
               TIO.Put_Line ("bit " & CT.zeropad (x, 2) & " :" &
                             mybits (x)'Img);
            end loop;
            for x in mychain'Range loop
               TIO.Put_Line ("chain link" & x'Img & " : " &
                   CT.zeropad (Natural (mychain (x)), 2));
            end loop;
         end;
         TIO.Put_Line ("nbyte2 : " & row.column (1).as_nbyte2'Img);
         TIO.Put_Line ("nbyte3 : " & row.column (1).as_nbyte3'Img);
         TIO.Put_Line ("nbyte4 : " & row.column (1).as_nbyte4'Img);
         TIO.Put_Line ("nbyte8 : " & row.column (1).as_nbyte8'Img);

      end if;
   end;

   CON.DR.disconnect;

   declare
      small_bits : AdaBase.Results.Bits (0 .. 6) := (1, 0, 1, 1, 0, 1, 0);
      one_bit    : AdaBase.Results.Bits (0 .. 0) := (0 => 1);
      one_bitstr : String := ARC.convert (one_bit);
      my_bytestr : String := ARC.convert (small_bits);
      my_byte    : AdaBase.Results.NByte1 := ARC.convert (small_bits);
   begin
      TIO.Put_Line ("====================================");
      TIO.Put_Line ("one bit as boolean : " & one_bitstr);
      TIO.Put_Line ("nbyte1 :" & my_byte'Img);
      TIO.Put_Line ("nbyte1 : " & my_bytestr);
   end;

end Bits;
