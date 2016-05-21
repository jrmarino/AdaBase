with AdaBase.Results.Sets;
with Connect;
with CommonText;
with Ada.Text_IO;

procedure Iterate is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package AR  renames AdaBase.Results;
   package CT  renames CommonText;

   fruit    : aliased AR.Textual;
   color    : aliased AR.Textual;
   calories : aliased AR.NByte2;
   dashes   : String (1 .. 50) := (others => '=');

   procedure list_fruit;
   procedure list_hockey_teams (row : AR.Sets.Datarow);

   procedure list_fruit
   is
      pair : String := CT.USS (fruit) & " (" & CT.USS (color) & ")";
      plen : Natural := pair'Length;
      zone : String (1 .. 20) := (others => ' ');
   begin
      zone (1 .. plen) := pair;
      TIO.Put_Line (zone & " contains" & calories'Img & " calories");
   end list_fruit;

   procedure list_hockey_teams (row : AR.Sets.Datarow) is
   begin
      TIO.Put_Line (row.column ("city").as_string & " " &
                    row.column ("mascot").as_string & " (" &
                    row.column ("abbreviation").as_string & ")");
   end list_hockey_teams;

begin

   CON.connect_database;

   declare
      stmt : CON.Stmt_Type := CON.DR.query_select
                  (tables    => "fruits",
                  columns    => "fruit, calories, color",
                  conditions => "calories >= 50",
                  order      => "calories");
   begin
      stmt.bind ("fruit",    fruit'Unchecked_Access);
      stmt.bind ("calories", calories'Unchecked_Access);
      stmt.bind ("color",    color'Unchecked_Access);

      TIO.Put_Line ("Demonstrate STMT.iterate (query + bound variables)");
      TIO.Put_Line (dashes);
      TIO.Put_Line ("List of fruit the contain at least 50 calories");
      TIO.Put_Line (dashes);
      stmt.iterate (process => list_fruit'Access);
   end;

   declare
      stmt : CON.Stmt_Type := CON.DR.prepare_select
                  (tables    => "nhl_teams",
                  columns    => "*",
                  conditions => "city LIKE :pattern",
                  order      => "city");
   begin
      TIO.Put_Line ("");
      TIO.Put_Line ("Demonstrate STMT.iterate (prepare + data access)");
      TIO.Put_Line (dashes);
      TIO.Put_Line ("List of NHL teams in locations starting with 'C'");
      TIO.Put_Line (dashes);

      stmt.assign ("pattern", "C%");
      if stmt.execute then
         stmt.iterate (process => list_hockey_teams'Access);
      end if;
   end;

   CON.DR.disconnect;

end Iterate;
