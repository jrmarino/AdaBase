--  Used for all testcases for MySQL driver
--  Modify connection parameters as necessary

package body Connect is

   procedure connect_database is
   begin
      DR.basic_connect (database => "adabase_examples",
                        username => "root",
                        password => "",
                        hostname => "localhost",
                        port     => 3306);
   end connect_database;

end Connect;
