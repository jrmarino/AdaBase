with AdaBase;
with Connect;
with CommonText;
with Ada.Text_IO;
with Ada.Calendar.Formatting;
with AdaBase.Results.Sets;
with AdaBase.Logger.Facility;
with Interfaces;

procedure All_Types is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package AR  renames AdaBase.Results;
   package ARS renames AdaBase.Results.Sets;
   package CT  renames CommonText;
   package ALF renames AdaBase.Logger.Facility;
   package CAL renames Ada.Calendar;
   package CFM renames Ada.Calendar.Formatting;

   package Byte_Io is new Ada.Text_Io.Modular_Io (Interfaces.Unsigned_8);

   type halfbyte is mod 2 ** 4;

   procedure dump_result;
   function halfbyte_to_hex (value : halfbyte) return Character;
   function convert_chain (chain : AR.chain) return String;
   function convert_set (set : AR.settype) return String;
   function pad (S : String; Slen : Natural) return String;
   function pad (S : String; Slen : Natural) return String
   is
      field : String (1 .. Slen) := (others => ' ');
      len   : Natural := S'Length;
   begin
      field (1 .. len) := S;
      return field;
   end pad;

   function halfbyte_to_hex (value : halfbyte) return Character
   is
      zero     : constant Natural := Character'Pos ('0');
      alpham10 : constant Natural := Character'Pos ('A') - 10;
   begin
      case value is
         when 0 .. 9 => return Character'Val (zero + Natural (value));
         when others => return Character'Val (alpham10 + Natural (value));
      end case;
   end halfbyte_to_hex;

   function convert_chain (chain : AR.chain) return String
   is
      use type AR.nbyte1;
      blocks    : constant Natural := chain'Length;
      mask_ones : constant AR.nbyte1 := 16#0F#;
      work_4bit : halfbyte;
      result    : String (1 .. blocks * 3 - 1) := (others => ' ');
      index     : Natural := 0;
      fullbyte  : Interfaces.Unsigned_8;
   begin
      for x in Positive range 1 .. blocks loop
         index := index + 1;
         fullbyte := Interfaces.Unsigned_8 (chain (x));
         fullbyte := Interfaces.Shift_Right (fullbyte, 4);
         work_4bit := halfbyte (fullbyte);
         result (index) := halfbyte_to_hex (work_4bit);
         index := index + 1;
         work_4bit := halfbyte (chain (x) and mask_ones);
         result (index) := halfbyte_to_hex (work_4bit);
         index := index + 1;
      end loop;
      if blocks = 0 then
         return "(empty)";
      end if;
      return result;
   end convert_chain;

   function convert_set (set : AR.settype) return String
   is
      result : CT.Text;
   begin
      for x in set'Range loop
         if not CT.IsBlank (set (x).enumeration) then
            if x > set'First then
               CT.SU.Append (result, ",");
            end if;
            CT.SU.Append (result, set (x).enumeration);
         end if;
      end loop;
      return CT.USS (result);
   end convert_set;

   procedure dump_result
   is
      row     : ARS.DataRow_Access;
      numcols : constant Natural := CON.STMT.column_count;
   begin
      loop
         exit when not CON.STMT.fetch_next (row);
         for c in Natural range 1 .. numcols loop
            TIO.Put (CT.zeropad (c, 2) & ". ");
            TIO.Put (pad (CON.STMT.column_name (c), 16));
            TIO.Put (pad (CON.STMT.column_native_type (c)'Img, 15));
            case CON.STMT.column_native_type (c) is
               when AdaBase.ft_chain =>
                  TIO.Put_Line (convert_chain (row.column (c).as_chain));
               when others =>
                  TIO.Put_Line (row.column (c).as_string);
            end case;
         end loop;
      end loop;
      TIO.Put_Line ("");
   end dump_result;

   sql1 : constant String := "SELECT nbyte0, nbyte1, nbyte2, id_nbyte3, " &
                             "nbyte4, nbyte8, byte1, byte2, byte3, byte4, " &
                             "byte5, real9, real18, exact_decimal, " &
                             "bit_type, my_date, my_datetime, my_timestamp, " &
                             "my_time, my_year, fixed_string, " &
                             "variable_string, my_tinytext, my_text, " &
                             "my_mediumtext, my_longtext, " &
                             "enumtype, settype, " &
                             "my_binary, my_varbinary, " &
                             "my_tinyblob, my_mediumblob, " &
                             "my_blob, my_longblob " &
                             "FROM all_types WHERE id_nbyte3 = 1";
   sql2 : constant String := "INSERT INTO all_types VALUES (:id_nbyte3, " &
                             ":nbyte0, :nbyte1, :nbyte2, :nbyte4, :nbyte8, " &
                             ":byte1, :byte2, :byte3, :byte4, :byte8, " &
                             ":real9, :real18, :exact, :bit, :date, " &
                             ":datetime, :timestamp, :time, :year, :fixed, " &
                             ":varstring, :tinytext, :text, :medtext, " &
                             ":longtext, :binary, :varbin, :tinyblob, " &
                             ":medblob, :blob, :longblob, :enumtype, " &
                             ":settype)";
begin

   CON.DR.command_standard_logger (device => ALF.file, action => ALF.attach);

   declare
   begin
      CON.connect_database;
   exception
      when others =>
         TIO.Put_Line ("database connect failed.");
         return;
   end;

   begin
      CON.STMT := CON.DR.query (sql1);
      if CON.STMT.successful then
         TIO.Put_Line ("Dumping Result from direct statement ...");
         dump_result;
      end if;
   end;

   begin
      CON.STMT := CON.DR.prepare (sql1);
      if CON.STMT.execute then
         TIO.Put_Line ("Dumping Result from prepared statement ...");
         dump_result;
      else
         TIO.Put_Line ("statement execution failed");
      end if;
   end;

   declare
      v_nbyte0 : aliased AR.nbyte0;
      v_nbyte1 : aliased AR.nbyte1;
      v_nbyte2 : aliased AR.nbyte2;
      v_nbyte3 : aliased AR.nbyte3;
      v_nbyte4 : aliased AR.nbyte4;
      v_nbyte8 : aliased AR.nbyte8;
      v_byte1  : aliased AR.byte1;
      v_byte2  : aliased AR.byte2;
      v_byte3  : aliased AR.byte3;
      v_byte4  : aliased AR.byte4;
      v_byte8  : aliased AR.byte8;
      v_exact  : aliased AR.real9;
      v_real9  : aliased AR.real9;
      v_real18 : aliased AR.real18;
      v_text1  : aliased AR.textual;
      v_text2  : aliased AR.textual;
      v_text3  : aliased AR.textual;
      v_text4  : aliased AR.textual;
      v_text5  : aliased AR.textual;
      v_text6  : aliased AR.textual;
      v_time1  : aliased AR.AC.Time;
      v_time2  : aliased AR.AC.Time;
      v_time3  : aliased AR.AC.Time;
      v_time4  : aliased AR.AC.Time;
      v_year   : aliased AR.nbyte2;
      v_bit    : aliased AR.chain := (1 .. 2 => 0);
      v_chain1 : aliased AR.chain := (1 .. 4 => 0);
      v_chain2 : aliased AR.chain := (1 .. 6 => 0);
      v_chain3 : aliased AR.chain := (1 .. 16 => 0);
      v_chain4 : aliased AR.chain := (1 .. 16 => 0);
      v_chain5 : aliased AR.chain := (1 .. 16 => 0);
      v_chain6 : aliased AR.chain := (1 .. 16 => 0);
      v_enum   : aliased AR.enumtype;
      v_set    : aliased AR.settype := (1 .. 6 => (CT.blank, 0));
   begin
      CON.STMT := CON.DR.prepare (sql1);
      if CON.STMT.execute then
         CON.STMT.bind (1, v_nbyte0'Unchecked_Access);
         CON.STMT.bind (2, v_nbyte1'Unchecked_Access);
         CON.STMT.bind (3, v_nbyte2'Unchecked_Access);
         CON.STMT.bind (4, v_nbyte3'Unchecked_Access);
         CON.STMT.bind (5, v_nbyte4'Unchecked_Access);
         CON.STMT.bind (6, v_nbyte8'Unchecked_Access);
         CON.STMT.bind (7,  v_byte1'Unchecked_Access);
         CON.STMT.bind (8,  v_byte2'Unchecked_Access);
         CON.STMT.bind (9,  v_byte3'Unchecked_Access);
         CON.STMT.bind (10, v_byte4'Unchecked_Access);
         CON.STMT.bind (11, v_byte8'Unchecked_Access);
         CON.STMT.bind (12, v_real9'Unchecked_Access);
         CON.STMT.bind (13, v_real18'Unchecked_Access);
         CON.STMT.bind (14, v_exact'Unchecked_Access);
         CON.STMT.bind (15, v_bit'Unchecked_Access);
         CON.STMT.bind (16, v_time1'Unchecked_Access);
         CON.STMT.bind (17, v_time2'Unchecked_Access);
         CON.STMT.bind (18, v_time3'Unchecked_Access);
         CON.STMT.bind (19, v_time4'Unchecked_Access);
         CON.STMT.bind (20, v_year'Unchecked_Access);
         CON.STMT.bind (21, v_text1'Unchecked_Access);
         CON.STMT.bind (22, v_text2'Unchecked_Access);
         CON.STMT.bind (23, v_text3'Unchecked_Access);
         CON.STMT.bind (24, v_text4'Unchecked_Access);
         CON.STMT.bind (25, v_text5'Unchecked_Access);
         CON.STMT.bind (26, v_text6'Unchecked_Access);
         CON.STMT.bind (27, v_enum'Unchecked_Access);
         CON.STMT.bind (28, v_set'Unchecked_Access);
         CON.STMT.bind (29, v_chain1'Unchecked_Access);
         CON.STMT.bind (30, v_chain2'Unchecked_Access);
         CON.STMT.bind (31, v_chain3'Unchecked_Access);
         CON.STMT.bind (32, v_chain4'Unchecked_Access);
         CON.STMT.bind (33, v_chain5'Unchecked_Access);
         CON.STMT.bind (34, v_chain6'Unchecked_Access);
         TIO.Put_Line ("Dumping Result from PS/Bound fetch ...");
         loop
            exit when not CON.STMT.fetch_bound;
            TIO.Put_Line (" 1. nbyte0          " & v_nbyte0'Img);
            TIO.Put_Line (" 2. nbyte1          " & v_nbyte1'Img);
            TIO.Put_Line (" 3. nbyte2          " & v_nbyte2'Img);
            TIO.Put_Line (" 4. nbyte3          " & v_nbyte3'Img);
            TIO.Put_Line (" 5. nbyte4          " & v_nbyte4'Img);
            TIO.Put_Line (" 6. nbyte8          " & v_nbyte8'Img);
            TIO.Put_Line (" 7. byte1           " & v_byte1'Img);
            TIO.Put_Line (" 8. byte2           " & v_byte2'Img);
            TIO.Put_Line (" 9. byte3           " & v_byte3'Img);
            TIO.Put_Line ("10. byte4           " & v_byte4'Img);
            TIO.Put_Line ("11. byte8           " & v_byte8'Img);
            TIO.Put_Line ("12. real9           " & v_real9'Img);
            TIO.Put_Line ("13. real18          " & v_real18'Img);
            TIO.Put_Line ("14. exact           " & v_exact'Img);
            TIO.Put_Line ("15. bits            " & convert_chain (v_bit));
            TIO.Put_Line ("16. date            " & CFM.Image (v_time1));
            TIO.Put_Line ("17. datetime        " & CFM.Image (v_time2));
            TIO.Put_Line ("18. timestamp       " & CFM.Image (v_time3));
            TIO.Put_Line ("19. time            " & CFM.Image (v_time4));
            TIO.Put_Line ("20. year            " & v_year'Img);
            TIO.Put_Line ("21. fixed string    " & CT.USS (v_text1));
            TIO.Put_Line ("22. variable string " & CT.USS (v_text2));
            TIO.Put_Line ("23. tinytext        " & CT.USS (v_text3));
            TIO.Put_Line ("24. text            " & CT.USS (v_text4));
            TIO.Put_Line ("25. medium text     " & CT.USS (v_text5));
            TIO.Put_Line ("26. long text       " & CT.USS (v_text6));
            TIO.Put_Line ("27. enum            " & CT.USS (v_enum.enumeration));
            TIO.Put_Line ("28. settype         " & convert_set (v_set));
            TIO.Put_Line ("29. binary          " & convert_chain (v_chain1));
            TIO.Put_Line ("30. varbinary       " & convert_chain (v_chain2));
            TIO.Put_Line ("31. tiny blob       " & convert_chain (v_chain3));
            TIO.Put_Line ("32. medium blob     " & convert_chain (v_chain4));
            TIO.Put_Line ("33. blob            " & convert_chain (v_chain5));
            TIO.Put_Line ("34. long blob       " & convert_chain (v_chain6));
         end loop;
      end if;
   end;

   declare
      numrows : AdaBase.AffectedRows;
   begin
      numrows := CON.DR.execute ("DELETE FROM all_types WHERE id_nbyte3 > 8");
      if Natural (numrows) > 0 then
         CON.DR.commit;
      end if;
   end;

   declare
      v_nbyte0 : aliased AR.nbyte0  := False;
      v_nbyte1 : aliased AR.nbyte1  := 22;
      v_nbyte2 : aliased AR.nbyte2  := 5800;
      v_nbyte3 : aliased AR.nbyte3  := 9;
      v_nbyte4 : aliased AR.nbyte4  := AR.nbyte4 (2 ** 20);
      v_nbyte8 : aliased AR.nbyte8  := AR.nbyte8 (2 ** 24 + 1);
      v_byte1  : aliased AR.byte1   := AR.byte1 (-2);
      v_byte2  : aliased AR.byte2   := AR.byte2 (-132);
      v_byte3  : aliased AR.byte3   := AR.byte3 (-8000000);
      v_byte4  : aliased AR.byte4   := 24;
      v_byte8  : aliased AR.byte8   := 128;
      v_exact  : aliased AR.real9   := 7.32;
      v_real9  : aliased AR.real9   := 999.01234;
      v_real18 : aliased AR.real18  := 99999.01234567890123456789;
      v_text1  : aliased AR.textual := CT.SUS ("Popeye");
      v_text2  : aliased AR.textual := CT.SUS ("Daredevel");
      v_text3  : aliased AR.textual := CT.SUS ("The Punisher");
      v_text4  : aliased AR.textual := CT.SUS ("Electra");
      v_text5  : aliased AR.textual := CT.SUS ("Iron Man");
      v_text6  : aliased AR.textual := CT.SUS ("Bruce Banner");
      v_time1  : aliased AR.AC.Time := CAL.Time_Of (1995, 2, 14);
      v_time2  : aliased AR.AC.Time := CAL.Time_Of (1998, 3, 17, 7020.0);
      v_time3  : aliased AR.AC.Time := CAL.Time_Of (2005, 4, 20, 6000.253);
      v_time4  : aliased AR.AC.Time := CAL.Time_Of (1901, 1, 1, 13000.0);
      v_year   : aliased AR.nbyte2  := 1992;
      v_bit    : aliased AR.chain   := (5, 127);
      v_chain1 : aliased AR.chain   := (12, 44, 65, 240);
      v_chain2 : aliased AR.chain   := (97, 99, 102);
      v_chain3 : aliased AR.chain   := (1, 0, 20, 37, 10);
      v_chain4 : aliased AR.chain   := (200, 232, 98, 100, 77, 82);
      v_chain5 : aliased AR.chain   := (50, 12, 2, 4, 99, 255, 27);
      v_chain6 : aliased AR.chain   := (0, 0, 0, 0, 1, 2, 3, 4);
      v_enum   : aliased AR.enumtype := (CT.SUS ("pink"), 0);
      v_set    : aliased AR.settype :=
                 ((CT.SUS ("red"), 0), (CT.SUS ("green"), 0));
   begin
      CON.STMT := CON.DR.prepare (sql2);
      CON.STMT.assign ("nbyte0", v_nbyte0'Unchecked_Access);
      CON.STMT.assign ("nbyte1", v_nbyte1'Unchecked_Access);
      CON.STMT.assign ("nbyte2", v_nbyte2'Unchecked_Access);
      CON.STMT.assign ("id_nbyte3", v_nbyte3'Unchecked_Access);
      CON.STMT.assign ("nbyte4", v_nbyte4'Unchecked_Access);
      CON.STMT.assign ("nbyte8", v_nbyte8'Unchecked_Access);
      CON.STMT.assign ("byte1",  v_byte1'Unchecked_Access);
      CON.STMT.assign ("byte2",  v_byte2'Unchecked_Access);
      CON.STMT.assign ("byte3",  v_byte3'Unchecked_Access);
      CON.STMT.assign ("byte4", v_byte4'Unchecked_Access);
      CON.STMT.assign ("byte8", v_byte8'Unchecked_Access);
      CON.STMT.assign ("real9", v_real9'Unchecked_Access);
      CON.STMT.assign ("real18", v_real18'Unchecked_Access);
      CON.STMT.assign ("exact", v_exact'Unchecked_Access);
      CON.STMT.assign ("bit", v_bit'Unchecked_Access);
      CON.STMT.assign ("date", v_time1'Unchecked_Access);
      CON.STMT.assign ("datetime", v_time2'Unchecked_Access);
      CON.STMT.assign ("timestamp", v_time3'Unchecked_Access);
      CON.STMT.assign ("time", v_time4'Unchecked_Access);
      CON.STMT.assign ("year", v_year'Unchecked_Access);
      CON.STMT.assign ("fixed", v_text1'Unchecked_Access);
      CON.STMT.assign ("varstring", v_text2'Unchecked_Access);
      CON.STMT.assign ("tinytext", v_text3'Unchecked_Access);
      CON.STMT.assign ("text", v_text4'Unchecked_Access);
      CON.STMT.assign ("medtext", v_text5'Unchecked_Access);
      CON.STMT.assign ("longtext", v_text6'Unchecked_Access);
      CON.STMT.assign ("enumtype", v_enum'Unchecked_Access);
      CON.STMT.assign ("settype", v_set'Unchecked_Access);
      CON.STMT.assign ("binary", v_chain1'Unchecked_Access);
      CON.STMT.assign ("varbin", v_chain2'Unchecked_Access);
      CON.STMT.assign ("tinyblob", v_chain3'Unchecked_Access);
      CON.STMT.assign ("medblob", v_chain4'Unchecked_Access);
      CON.STMT.assign ("blob", v_chain5'Unchecked_Access);
      CON.STMT.assign ("longblob", v_chain6'Unchecked_Access);
      TIO.Put_Line ("");
      if CON.STMT.execute then
         TIO.Put_Line ("Inserted" & CON.STMT.rows_affected'Img & " row(s)");
         v_nbyte3 := 11;
         v_nbyte0 := True;
         v_text1 := CT.SUS ("Wolverine");
         v_enum.enumeration := CT.SUS ("blue");
         if CON.STMT.execute then
            TIO.Put_Line ("Inserted" & CON.STMT.rows_affected'Img & " row(s)");
            CON.DR.commit;
         else
            TIO.Put_Line (CON.STMT.last_driver_message);
            CON.DR.rollback;
         end if;
      else
         TIO.Put_Line (CON.STMT.last_driver_message);
      end if;
   end;

   CON.DR.disconnect;

end All_Types;
