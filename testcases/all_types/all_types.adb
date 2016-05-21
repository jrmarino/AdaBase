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

   stmt_acc : CON.Stmt_Type_access;

   procedure dump_result;
   function halfbyte_to_hex (value : halfbyte) return Character;
   function convert_chain (chain : AR.Chain) return String;
   function convert_set (set : AR.Settype) return String;
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

   function convert_chain (chain : AR.Chain) return String
   is
      use type AR.NByte1;
      blocks    : constant Natural := chain'Length;
      mask_ones : constant AR.NByte1 := 16#0F#;
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

   function convert_set (set : AR.Settype) return String
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
      row     : ARS.Datarow;
      numcols : constant Natural := stmt_acc.column_count;
   begin
      loop
         row := stmt_acc.fetch_next;
         exit when row.data_exhausted;
         for c in Natural range 1 .. numcols loop
            TIO.Put (CT.zeropad (c, 2) & ". ");
            TIO.Put (pad (stmt_acc.column_name (c), 16));
            TIO.Put (pad (stmt_acc.column_native_type (c)'Img, 15));
            case stmt_acc.column_native_type (c) is
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
   sql3 : constant String := "INSERT INTO all_types (id_nbyte3, nbyte0, " &
                             "nbyte1, byte2, byte4, nbyte8, real9, real18, " &
                             "exact_decimal, my_date, my_timestamp, " &
                             "my_time, my_year, my_tinytext, enumtype, " &
                             "settype, my_varbinary, my_blob) VALUES " &
                             "(?,?, ?,?,?,?,?,?, ?,?,?, ?,?,?,?, ?,?,?)";

begin

   CON.DR.command_standard_logger (device => ALF.file, action => ALF.attach);

   CON.connect_database;

   declare
      stmt : aliased CON.Stmt_Type := CON.DR.query (sql1);
   begin
      if stmt.successful then
         stmt_acc := stmt'Unchecked_Access;
         TIO.Put_Line ("Dumping Result from direct statement ...");
         dump_result;
      end if;
   end;

   declare
      stmt : aliased CON.Stmt_Type := CON.DR.prepare (sql1);
   begin
      if stmt.execute then
         stmt_acc := stmt'Unchecked_Access;
         TIO.Put_Line ("Dumping Result from prepared statement ...");
         dump_result;
      else
         TIO.Put_Line ("statement execution failed");
      end if;
   end;

   declare
      v_nbyte0 : aliased AR.NByte0;
      v_nbyte1 : aliased AR.NByte1;
      v_nbyte2 : aliased AR.NByte2;
      v_nbyte3 : aliased AR.NByte3;
      v_nbyte4 : aliased AR.NByte4;
      v_nbyte8 : aliased AR.NByte8;
      v_byte1  : aliased AR.Byte1;
      v_byte2  : aliased AR.Byte2;
      v_byte3  : aliased AR.Byte3;
      v_byte4  : aliased AR.Byte4;
      v_byte8  : aliased AR.Byte8;
      v_exact  : aliased AR.Real9;
      v_real9  : aliased AR.Real9;
      v_real18 : aliased AR.Real18;
      v_text1  : aliased AR.Textual;
      v_text2  : aliased AR.Textual;
      v_text3  : aliased AR.Textual;
      v_text4  : aliased AR.Textual;
      v_text5  : aliased AR.Textual;
      v_text6  : aliased AR.Textual;
      v_time1  : aliased AR.AC.Time;
      v_time2  : aliased AR.AC.Time;
      v_time3  : aliased AR.AC.Time;
      v_time4  : aliased AR.AC.Time;
      v_year   : aliased AR.NByte2;
      v_bit    : aliased AR.Chain := (1 .. 2 => 0);
      v_chain1 : aliased AR.Chain := (1 .. 4 => 0);
      v_chain2 : aliased AR.Chain := (1 .. 6 => 0);
      v_chain3 : aliased AR.Chain := (1 .. 16 => 0);
      v_chain4 : aliased AR.Chain := (1 .. 16 => 0);
      v_chain5 : aliased AR.Chain := (1 .. 16 => 0);
      v_chain6 : aliased AR.Chain := (1 .. 16 => 0);
      v_enum   : aliased AR.Enumtype;
      v_set    : aliased AR.Settype := (1 .. 6 => (AR.PARAM_IS_ENUM));
      stmt : CON.Stmt_Type := CON.DR.prepare (sql1);
   begin
      if stmt.execute then
         stmt.bind (1, v_nbyte0'Unchecked_Access);
         stmt.bind (2, v_nbyte1'Unchecked_Access);
         stmt.bind (3, v_nbyte2'Unchecked_Access);
         stmt.bind (4, v_nbyte3'Unchecked_Access);
         stmt.bind (5, v_nbyte4'Unchecked_Access);
         stmt.bind (6, v_nbyte8'Unchecked_Access);
         stmt.bind (7,  v_byte1'Unchecked_Access);
         stmt.bind (8,  v_byte2'Unchecked_Access);
         stmt.bind (9,  v_byte3'Unchecked_Access);
         stmt.bind (10, v_byte4'Unchecked_Access);
         stmt.bind (11, v_byte8'Unchecked_Access);
         stmt.bind (12, v_real9'Unchecked_Access);
         stmt.bind (13, v_real18'Unchecked_Access);
         stmt.bind (14, v_exact'Unchecked_Access);
         stmt.bind (15, v_bit'Unchecked_Access);
         stmt.bind (16, v_time1'Unchecked_Access);
         stmt.bind (17, v_time2'Unchecked_Access);
         stmt.bind (18, v_time3'Unchecked_Access);
         stmt.bind (19, v_time4'Unchecked_Access);
         stmt.bind (20, v_year'Unchecked_Access);
         stmt.bind (21, v_text1'Unchecked_Access);
         stmt.bind (22, v_text2'Unchecked_Access);
         stmt.bind (23, v_text3'Unchecked_Access);
         stmt.bind (24, v_text4'Unchecked_Access);
         stmt.bind (25, v_text5'Unchecked_Access);
         stmt.bind (26, v_text6'Unchecked_Access);
         stmt.bind (27, v_enum'Unchecked_Access);
         stmt.bind (28, v_set'Unchecked_Access);
         stmt.bind (29, v_chain1'Unchecked_Access);
         stmt.bind (30, v_chain2'Unchecked_Access);
         stmt.bind (31, v_chain3'Unchecked_Access);
         stmt.bind (32, v_chain4'Unchecked_Access);
         stmt.bind (33, v_chain5'Unchecked_Access);
         stmt.bind (34, v_chain6'Unchecked_Access);
         TIO.Put_Line ("Dumping Result from PS/Bound fetch ...");
         loop
            exit when not stmt.fetch_bound;
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
      numrows : AdaBase.Affected_Rows;
   begin
      numrows := CON.DR.execute ("DELETE FROM all_types WHERE id_nbyte3 > 8");
      CON.DR.commit;
   end;

   declare
      v_nbyte0 : aliased AR.NByte0  := False;
      v_nbyte1 : aliased AR.NByte1  := 22;
      v_nbyte2 : aliased AR.NByte2  := 5800;
      v_nbyte3 : aliased AR.NByte3  := 9;
      v_nbyte4 : aliased AR.NByte4  := AR.NByte4 (2 ** 20);
      v_nbyte8 : aliased AR.NByte8  := AR.NByte8 (2 ** 24 + 1);
      v_byte1  : aliased AR.Byte1   := AR.Byte1 (-2);
      v_byte2  : aliased AR.Byte2   := AR.Byte2 (-132);
      v_byte3  : aliased AR.Byte3   := AR.Byte3 (-8000000);
      v_byte4  : aliased AR.Byte4   := 24;
      v_byte8  : aliased AR.Byte8   := 128;
      v_exact  : aliased AR.Real9   := 7.32;
      v_real9  : aliased AR.Real9   := 999.01234;
      v_real18 : aliased AR.Real18  := 99999.01234567890123456789;
      v_text1  : aliased AR.Textual := CT.SUS ("Popeye");
      v_text2  : aliased AR.Textual := CT.SUS ("Daredevel");
      v_text3  : aliased AR.Textual := CT.SUS ("The Punisher");
      v_text4  : aliased AR.Textual := CT.SUS ("Electra");
      v_text5  : aliased AR.Textual := CT.SUS ("Iron Man");
      v_text6  : aliased AR.Textual := CT.SUS ("Bruce Banner");
      v_time1  : aliased AR.AC.Time := CFM.Time_Of (1995, 2, 14);
      v_time2  : aliased AR.AC.Time := CFM.Time_Of (1998, 3, 17, 6, 7, 8);
      v_time3  : aliased AR.AC.Time := CFM.Time_Of (2005, 4, 20, 1, 32, 0);
      v_time4  : aliased AR.AC.Time := CFM.Time_Of (1901, 1, 1, 4, 57, 50);
      v_year   : aliased AR.NByte2  := 1992;
      v_bit    : aliased AR.Chain   := (5, 127);
      v_chain1 : aliased AR.Chain   := (12, 44, 65, 240);
      v_chain2 : aliased AR.Chain   := (97, 99, 102);
      v_chain3 : aliased AR.Chain   := (1, 0, 20, 37, 10);
      v_chain4 : aliased AR.Chain   := (200, 232, 98, 100, 77, 82);
      v_chain5 : aliased AR.Chain   := (50, 12, 2, 4, 99, 255, 27);
      v_chain6 : aliased AR.Chain   := (0, 0, 0, 0, 1, 2, 3, 4);
      v_enum   : aliased AR.Enumtype := (enumeration => CT.SUS ("pink"));
      v_set    : aliased AR.Settype := ((enumeration => CT.SUS ("red")),
                                        (enumeration => CT.SUS ("green")));
      v_set2   : AR.Settype :=         ((enumeration => CT.SUS ("yellow")),
                                        (enumeration => CT.SUS ("white")),
                                        (enumeration => CT.SUS ("red")));
      v_chain7 : AR.Chain := (65, 66, 67, 68);
      v_chain8 : AR.Chain := (97, 98, 99, 100, 101);

      stmt : CON.Stmt_Type := CON.DR.prepare (sql2);
   begin
      stmt.assign ("nbyte0", v_nbyte0'Unchecked_Access);
      stmt.assign ("nbyte1", v_nbyte1'Unchecked_Access);
      stmt.assign ("nbyte2", v_nbyte2'Unchecked_Access);
      stmt.assign ("id_nbyte3", v_nbyte3'Unchecked_Access);
      stmt.assign ("nbyte4", v_nbyte4'Unchecked_Access);
      stmt.assign ("nbyte8", v_nbyte8'Unchecked_Access);
      stmt.assign ("byte1",  v_byte1'Unchecked_Access);
      stmt.assign ("byte2",  v_byte2'Unchecked_Access);
      stmt.assign ("byte3",  v_byte3'Unchecked_Access);
      stmt.assign ("byte4", v_byte4'Unchecked_Access);
      stmt.assign ("byte8", v_byte8'Unchecked_Access);
      stmt.assign ("real9", v_real9'Unchecked_Access);
      stmt.assign ("real18", v_real18'Unchecked_Access);
      stmt.assign ("exact", v_exact'Unchecked_Access);
      stmt.assign ("bit", v_bit'Unchecked_Access);
      stmt.assign ("date", v_time1'Unchecked_Access);
      stmt.assign ("datetime", v_time2'Unchecked_Access);
      stmt.assign ("timestamp", v_time3'Unchecked_Access);
      stmt.assign ("time", v_time4'Unchecked_Access);
      stmt.assign ("year", v_year'Unchecked_Access);
      stmt.assign ("fixed", v_text1'Unchecked_Access);
      stmt.assign ("varstring", v_text2'Unchecked_Access);
      stmt.assign ("tinytext", v_text3'Unchecked_Access);
      stmt.assign ("text", v_text4'Unchecked_Access);
      stmt.assign ("medtext", v_text5'Unchecked_Access);
      stmt.assign ("longtext", v_text6'Unchecked_Access);
      stmt.assign ("enumtype", v_enum'Unchecked_Access);
      stmt.assign ("settype", v_set'Unchecked_Access);
      stmt.assign ("binary", v_chain1'Unchecked_Access);
      stmt.assign ("varbin", v_chain2'Unchecked_Access);
      stmt.assign ("tinyblob", v_chain3'Unchecked_Access);
      stmt.assign ("medblob", v_chain4'Unchecked_Access);
      stmt.assign ("blob", v_chain5'Unchecked_Access);
      stmt.assign ("longblob", v_chain6'Unchecked_Access);
      TIO.Put_Line ("");
      if stmt.execute then
         TIO.Put_Line ("Inserted" & stmt.rows_affected'Img & " row(s)");
         v_nbyte3 := 11;
         v_nbyte0 := True;
         v_text1 := CT.SUS ("Wolverine");
         v_enum.enumeration := CT.SUS ("blue");
         if stmt.execute then
            TIO.Put_Line ("Inserted" & stmt.rows_affected'Img & " row(s)");
            v_nbyte3 := 15;
            stmt.assign ("settype", v_set2);
            stmt.assign ("binary", v_chain7);
            stmt.assign ("varbin", v_chain8);
            v_exact := 187.93;
            if stmt.execute then
               TIO.Put_Line ("Inserted" & stmt.rows_affected'Img &
                             " row(s)");
               CON.DR.commit;
            else
               TIO.Put_Line (stmt.last_driver_message);
               CON.DR.rollback;
            end if;
         else
            TIO.Put_Line (stmt.last_driver_message);
            CON.DR.rollback;
         end if;
      else
         TIO.Put_Line (stmt.last_driver_message);
      end if;
   end;


   declare
      values : constant String := "20|1|150|-10|-90000|3200100|87.2341|" &
        "15555.213792831213|875.44|2014-10-20|2000-03-25 15:15:00|" &
        "20:18:13|1986|AdaBase is so cool!|green|yellow,black|" &
        " 0123|456789ABC.,z[]";
      stmt : CON.Stmt_Type := CON.DR.prepare (sql3);
   begin
      --  This has to be done only once after the prepare command
      --  Set the type for each parameter (required for at least MySQL)
      stmt.assign (1,  AR.PARAM_IS_NBYTE_3);
      stmt.assign (2,  AR.PARAM_IS_BOOLEAN);
      stmt.assign (3,  AR.PARAM_IS_NBYTE_1);
      stmt.assign (4,  AR.PARAM_IS_BYTE_2);
      stmt.assign (5,  AR.PARAM_IS_BYTE_4);
      stmt.assign (6,  AR.PARAM_IS_NBYTE_8);
      stmt.assign (7,  AR.PARAM_IS_REAL_9);
      stmt.assign (8,  AR.PARAM_IS_REAL_18);
      stmt.assign (9,  AR.PARAM_IS_REAL_9);
      stmt.assign (10, AR.PARAM_IS_TIMESTAMP);
      stmt.assign (11, AR.PARAM_IS_TIMESTAMP);
      stmt.assign (12, AR.PARAM_IS_TIMESTAMP);
      stmt.assign (13, AR.PARAM_IS_NBYTE_2);
      stmt.assign (14, AR.PARAM_IS_TEXTUAL);
      stmt.assign (15, AR.PARAM_IS_ENUM);
      stmt.assign (16, AR.PARAM_IS_SET);
      stmt.assign (17, AR.PARAM_IS_CHAIN);
      stmt.assign (18, AR.PARAM_IS_CHAIN);

      if stmt.execute (values) then
         TIO.Put_Line ("Inserted" & stmt.rows_affected'Img & " row(s)");
         CON.DR.commit;
      else
         TIO.Put_Line ("statement execution failed");
      end if;
   end;

   CON.DR.disconnect;

end All_Types;
