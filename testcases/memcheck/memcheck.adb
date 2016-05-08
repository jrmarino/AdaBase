with AdaBase.Results.Sets;
with Connect;
with CommonText;
with Ada.Text_IO;
with Ada.Command_Line;

procedure Memcheck is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package CLI renames Ada.Command_Line;
   package AR  renames AdaBase.Results;
   package CT  renames CommonText;

   procedure list_hockey_teams3;
   procedure list_hockey_teams (row : AR.Sets.DataRow);
   procedure list_hockey_teams (row : AR.Sets.DataRow) is
   begin
      TIO.Put_Line (row.column ("city").as_string & " " &
                    row.column ("mascot").as_string & " (" &
                    row.column ("abbreviation").as_string & ")");
   end list_hockey_teams;

   testnumber : Integer := 1;
   cycles : Integer;
   Ch : Character;

   city, mascot, abbr : aliased AR.textual;
   procedure list_hockey_teams3 is
   begin
      TIO.Put_Line (CT.USS (abbr) & ": " & CT.USS (city) & " " &
                    CT.USS (mascot));
   end list_hockey_teams3;

begin

   if CLI.Argument_Count < 1 then
      TIO.Put_Line ("Please input number of times to execute");
      return;
   else
      cycles := Integer'Value (CLI.Argument (1));
   end if;

   if CLI.Argument_Count >= 2 then
      testnumber := Integer'Value (CLI.Argument (2));
   end if;

   begin
      CON.connect_database;
   exception
      when others =>
         TIO.Put_Line ("database connect failed.");
         return;
   end;

   for x in Integer range 1 .. cycles loop
      if testnumber = 1 then
         declare
            stmt : CON.Stmt_Type :=
               CON.DR.prepare_select
                        (tables    => "nhl_teams",
                        null_sort  => AdaBase.nulls_first,
                        columns    => "*");
         begin
            if stmt.execute then
               stmt.iterate (process => list_hockey_teams'Access);
            end if;
            TIO.Put_Line ("T1: Finished iteration" & x'Img);
         end;
      elsif testnumber = 2 then
         declare
            stmt : CON.Stmt_Type :=
               CON.DR.query_select
                        (tables    => "nhl_teams",
                        null_sort  => AdaBase.nulls_first,
                        columns    => "*");
         begin
            stmt.iterate (process => list_hockey_teams'Access);
            TIO.Put_Line ("T2: Finished iteration" & x'Img);
         end;
      elsif testnumber = 3 then
         declare
            stmt : CON.Stmt_Type := CON.DR.query ("SELECT * FROM nhl_teams");
         begin
            stmt.bind (2, abbr'Unchecked_Access);
            stmt.bind (3, city'Unchecked_Access);
            stmt.bind (4, mascot'Unchecked_Access);
            stmt.iterate (process => list_hockey_teams3'Access);
            TIO.Put_Line ("T3: Finished iteration" & x'Img);
         end;
      elsif testnumber = 4 then
         declare
            stmt : CON.Stmt_Type := CON.DR.prepare ("SELECT * FROM nhl_teams");
         begin
            if stmt.execute then
               stmt.bind (2, abbr'Unchecked_Access);
               stmt.bind (3, city'Unchecked_Access);
               stmt.bind (4, mascot'Unchecked_Access);
               stmt.iterate (process => list_hockey_teams3'Access);
            end if;
            TIO.Put_Line ("T4: Finished iteration" & x'Img);
         end;
      end if;
   end loop;
   TIO.Put_Line ("press any key");
   TIO.Get_Immediate (Ch);

   CON.DR.disconnect;

end Memcheck;
