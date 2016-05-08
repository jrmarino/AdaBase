--  Used for all testcases for MySQL driver

with AdaBase.Driver.Base.MySQL;
with AdaBase.Statement.Base.MySQL;

package Connect is

   --  All specific drivers renamed to "Database_Driver"
   subtype Database_Driver is AdaBase.Driver.Base.MySQL.MySQL_Driver;
   subtype Stmt_Type is AdaBase.Statement.Base.MySQL.MySQL_statement;

   DR : Database_Driver;

   procedure connect_database;

end Connect;
