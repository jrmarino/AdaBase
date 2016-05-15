--  Used for all testcases for PostgreSQL driver
--  Modify connection parameters as necessary

package body Connect is

   procedure connect_database is
   begin
      DR.basic_connect (database => "adabase_examples",
                        username => "adabaser",
                        password => "test",
                        socket   => "/tmp");
   end connect_database;

end Connect;
