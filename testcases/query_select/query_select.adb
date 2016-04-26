with AdaBase;
with Connect;
with CommonText;
with Ada.Text_IO;
with AdaBase.Results.Sets;

procedure Query_Select is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ARS renames AdaBase.Results.Sets;
   package AR  renames AdaBase.Results;
   package CT  renames CommonText;

begin

   declare
   begin
      CON.connect_database;
   exception
      when others =>
         TIO.Put_Line ("database connect failed.");
         return;
   end;

   CON.DR.set_trait_column_case (AdaBase.upper_case);

   CON.STMT := CON.DR.query_select
               (tables    => "nhl_schedule as S " &
                             "JOIN nhl_teams T1 ON S.home_team = T1.team_id " &
                             "JOIN nhl_teams T2 ON S.away_team = T2.team_id",
               columns    => "S.event_code, " &
                             "T1.city as home_city, " &
                             "T1.mascot as home_mascot, " &
                             "T1.abbreviation as home_short, " &
                             "S.home_score, " &
                             "T2.city as away_city, " &
                             "T2.mascot as away_mascot, " &
                             "T2.abbreviation as away_short, " &
                             "S.away_score",
               conditions => "S.yyyswww < 1085011",
               order      => "S.yyyswww ASC",
               limit      => 10,
               offset     => 20);

   if not CON.STMT.successful then
      TIO.Put_Line ("  Driver message: " & CON.STMT.last_driver_message);
      TIO.Put_Line ("     Driver code: " & CON.STMT.last_driver_code'Img);
      TIO.Put_Line ("       SQL State: " & CON.STMT.last_sql_state);
   else
      for c in Natural range 1 .. CON.STMT.column_count loop
         TIO.Put_Line ("Column" & c'Img & " heading: " &
                       CON.STMT.column_name (c));
      end loop;
      TIO.Put_Line ("");
   end if;

   --  Demonstrate bind/fetch_bound 
   declare
      event_code : aliased AR.nbyte2;
      home_town, home_mascot  : aliased AR.textual;
      away_town, away_mascot  : aliased AR.textual;
      home_score, away_score  : aliased AR.nbyte1;
   begin
      CON.STMT.bind (1, event_code'Unchecked_Access);
      CON.STMT.bind ("HOME_CITY", home_town'Unchecked_Access);
      CON.STMT.bind ("AWAY_CITY", away_town'Unchecked_Access);
      CON.STMT.bind (3, home_mascot'Unchecked_Access);
      CON.STMT.bind ("AWAY_MASCOT", away_mascot'Unchecked_Access);
      CON.STMT.bind ("HOME_SCORE", home_score'Unchecked_Access);
      CON.STMT.bind ("AWAY_SCORE", away_score'Unchecked_Access);

      loop
         exit when not CON.STMT.fetch_bound;

         TIO.Put ("In event" & event_code'Img & ", the " &
                  CT.USS (away_town) & " " & CT.USS (away_mascot) &
                  " visited the " &
                  CT.USS (home_town) & " " & CT.USS (home_mascot) & " and ");
         if Integer (away_score) > Integer (home_score) then
            TIO.Put ("won");
         elsif Integer (away_score) < Integer (home_score) then
            TIO.Put ("lost");
         else
            TIO.Put ("tied");
         end if;
         TIO.Put_Line (away_score'Img & " to" & home_score'Img);
      end loop;
      TIO.Put_Line ("");
   end;

   CON.STMT := CON.DR.query_select
               (tables    => "fruits",
               columns    => "fruit, calories, color",
               conditions => "calories > 50",
               order      => "calories",
               limit      => 10);

   --  demonstrate fetch_all
   declare
      rowset : ARS.DataRowSet := CON.STMT.fetch_all;
   begin
      for row in Natural range 1 .. rowset'Length loop
         TIO.Put_Line (rowset (row).column (1).as_string & ":" &
                       rowset (row).column ("calories").as_nbyte2'Img &
                       " calories, " & rowset (row).column (3).as_string);
      end loop;
   end;

   CON.DR.disconnect;

end Query_Select;
