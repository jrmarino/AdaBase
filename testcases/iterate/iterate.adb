with AdaBase.Results.Sets;
with Connect;
with CommonText;
with Ada.Text_IO;

procedure Iterate is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package AR  renames AdaBase.Results;
   package CT  renames CommonText;

   fruit    : aliased AR.textual;
   color    : aliased AR.textual;
   calories : aliased AR.nbyte2;
   dashes   : String (1 .. 50) := (others => '=');

   procedure list_fruit;
   procedure list_hockey_teams (row : AR.Sets.DataRow_Access);

   procedure list_fruit
   is
      pair : String := CT.USS (fruit) & " (" & CT.USS (color) & ")";
      plen : Natural := pair'Length;
      zone : String (1 .. 20) := (others => ' ');
   begin
      zone (1 .. plen) := pair;
      TIO.Put_Line (zone & " contains" & calories'Img & " calories");
   end list_fruit;

   procedure list_hockey_teams (row : AR.Sets.DataRow_Access) is
   begin
      TIO.Put_Line (row.column ("city").as_string & " " &
                    row.column ("mascot").as_string & " (" &
                    row.column ("abbreviation").as_string & ")");
   end list_hockey_teams;

begin

   begin
      CON.connect_database;
   exception
      when others =>
         TIO.Put_Line ("database connect failed.");
         return;
   end;

   begin
      CON.STMT := CON.DR.query_select
                  (tables    => "fruits",
                  columns    => "fruit, calories, color",
                  conditions => "calories >= 50",
                  order      => "calories");

      CON.STMT.bind ("fruit",    fruit'Unchecked_Access);
      CON.STMT.bind ("calories", calories'Unchecked_Access);
      CON.STMT.bind ("color",    color'Unchecked_Access);

      TIO.Put_Line ("Demonstrate STMT.iterate (query + bound variables)");
      TIO.Put_Line (dashes);
      TIO.Put_Line ("List of fruit the contain at least 50 calories");
      TIO.Put_Line (dashes);
      CON.STMT.iterate (process => list_fruit'Access);
   end;

   begin
      CON.STMT := CON.DR.prepare_select
                  (tables    => "nhl_teams",
                  columns    => "*",
                  conditions => "city LIKE :pattern",
                  order      => "city");

      TIO.Put_Line ("");
      TIO.Put_Line ("Demonstrate STMT.iterate (prepare + data access)");
      TIO.Put_Line (dashes);
      TIO.Put_Line ("List of NHL teams in locations starting with 'C'");
      TIO.Put_Line (dashes);

      CON.STMT.assign ("pattern", "C%");
      if CON.STMT.execute then
         CON.STMT.iterate (process => list_hockey_teams'Access);
      end if;
   end;

   CON.DR.disconnect;

end Iterate;
