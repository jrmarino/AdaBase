with AdaBase;
with Connect;
with CommonText;
with Ada.Text_IO;
with Ada.Calendar;
with AdaBase.Results.Sets;
with Interfaces;

procedure Execute_Dynabound is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package AR  renames AdaBase.Results;
   package ARS renames AdaBase.Results.Sets;
   package CT  renames CommonText;
   package CAL renames Ada.Calendar;

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
         TIO.Put_Line ("");
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

   cols : constant String := "id_nbyte3, nbyte0, " &
                             "nbyte1, byte2, byte4, nbyte8, real9, real18, " &
                             "exact_decimal, my_date, my_timestamp, " &
                             "my_time, my_year, my_tinytext, enumtype, " &
                             "settype, my_varbinary, my_blob";
   sql3 : constant String := "INSERT INTO all_types (" & cols & ") VALUES " &
                             "(?,?, ?,?,?,?,?,?, ?,?,?, ?,?,?,?, ?,?,?)";
   sql1 : constant String := "SELECT " & cols & " FROM all_types " &
                             "WHERE id_nbyte3 > 8";

begin

   declare
   begin
      CON.connect_database;
   exception
      when others =>
         TIO.Put_Line ("database connect failed.");
         return;
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
      vals1 : constant String := "20|1|150|-10|-90000|3200100|87.2341|" &
        "15555.213792831213|875.44|2014-10-20|2000-03-25 15:15:00|" &
        "20:18:13|1986|AdaBase is so cool!|green|yellow,black|" &
        " 0123|456789ABC.,z[]";
      vals2 : constant String := "25;0;200;25;22222;50;4.84324982;" &
        "9234963.123235987;15.79;1910-11-05;2030-12-25 11:59:59;" &
        "04:00:45;1945;This is what it sounds like when doves cry;" &
        "red;blue,white;Q|ER;01234" & Character'Val (0) &
        Character'Val (10) & "789";
      good : Boolean := True;
   begin
      CON.STMT := CON.DR.prepare (sql3);
      --  This has to be done only once after the prepare command
      --  Set the type for each parameter (required for at least MySQL)
      CON.STMT.assign (1,  AR.PARAM_IS_NBYTE_3);
      CON.STMT.assign (2,  AR.PARAM_IS_BOOLEAN);
      CON.STMT.assign (3,  AR.PARAM_IS_NBYTE_1);
      CON.STMT.assign (4,  AR.PARAM_IS_BYTE_2);
      CON.STMT.assign (5,  AR.PARAM_IS_BYTE_4);
      CON.STMT.assign (6,  AR.PARAM_IS_NBYTE_8);
      CON.STMT.assign (7,  AR.PARAM_IS_REAL_9);
      CON.STMT.assign (8,  AR.PARAM_IS_REAL_18);
      CON.STMT.assign (9,  AR.PARAM_IS_REAL_9);
      CON.STMT.assign (10, AR.PARAM_IS_TIMESTAMP);
      CON.STMT.assign (11, AR.PARAM_IS_TIMESTAMP);
      CON.STMT.assign (12, AR.PARAM_IS_TIMESTAMP);
      CON.STMT.assign (13, AR.PARAM_IS_NBYTE_2);
      CON.STMT.assign (14, AR.PARAM_IS_TEXTUAL);
      CON.STMT.assign (15, AR.PARAM_IS_ENUM);
      CON.STMT.assign (16, AR.PARAM_IS_SET);
      CON.STMT.assign (17, AR.PARAM_IS_CHAIN);
      CON.STMT.assign (18, AR.PARAM_IS_CHAIN);

      good := CON.STMT.execute (vals1);
      if good then
         good := CON.STMT.execute (parameters => vals2, delimiter => ';');
      end if;
      if good then
         CON.DR.commit;
      else
         TIO.Put_Line ("statement execution failed");
         CON.DR.rollback;
      end if;
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
      end if;
   end;

   TIO.Put_Line ("Note slight differences in real9 and real18 field values");
   TIO.Put_Line ("due to rounding differences inherent in the different");
   TIO.Put_Line ("retrieval mechanisms of direct and prep stmt results.");

   CON.DR.disconnect;

end Execute_Dynabound;
