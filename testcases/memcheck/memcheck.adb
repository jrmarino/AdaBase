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

   procedure list_hockey_teams (row : AR.Sets.DataRow_Access);
   procedure list_hockey_teams (row : AR.Sets.DataRow_Access) is
   begin
      TIO.Put_Line (row.column ("city").as_string & " " &
                    row.column ("mascot").as_string & " (" &
                    row.column ("abbreviation").as_string & ")");
   end list_hockey_teams;

   cycles : Integer;
   Ch : Character;
begin

   if CLI.Argument_Count < 1 then
      TIO.Put_Line ("Please input number of times to execute");
      return;
   else
      cycles := Integer'Value (CLI.Argument (1));
   end if;

   begin
      CON.connect_database;
   exception
      when others =>
         TIO.Put_Line ("database connect failed.");
         return;
   end;

   for x in Integer range 1 .. cycles loop
      begin
         CON.STMT := CON.DR.prepare_select
                     (tables    => "nhl_teams",
                     columns    => "*");

         if CON.STMT.execute then
            CON.STMT.iterate (process => list_hockey_teams'Access);
         end if;
         TIO.Put_Line ("Finished iteration" & x'Img);
      end;
   end loop;
   TIO.Put_Line ("press any key");
   TIO.Get_Immediate (Ch);

   CON.DR.disconnect;

end Memcheck;
