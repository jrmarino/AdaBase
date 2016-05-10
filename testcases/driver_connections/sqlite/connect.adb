--  Used for all testcases for SQLite driver
--  Modify connection parameters as necessary

package body Connect is

   procedure connect_database is
   begin
      DR.basic_connect (database => "file:///home/marino/adabase.sqlite");
   end connect_database;

end Connect;
