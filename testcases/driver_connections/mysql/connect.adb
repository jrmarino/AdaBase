--  Used for all testcases for MySQL driver
--  Modify connection parameters as necessary

with AdaBase.Driver.Base.MySQL;

package body Connect is

   procedure connect_database is
   begin
      DR.basic_connect (database => "test",
                        username => "anon",
                        password => "",
                        hostname => "localhost",
                        port     => 3306);
   end connect_database;

end Connect;
