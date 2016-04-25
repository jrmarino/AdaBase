--  Used for all testcases for MySQL driver

with AdaBase.Driver.Base.MySQL;
with AdaBase.Statement.Base.MySQL;

package Connect is

   --  All specific drivers renamed to "Database_Driver"
   --  All specific statement Accesses renamed to "Stmt_Access"
   subtype Database_Driver is AdaBase.Driver.Base.MySQL.MySQL_Driver;
   subtype Stmt_Access is AdaBase.Statement.Base.MySQL.MySQL_statement_access;

   DR : Database_Driver;
   STMT : Stmt_Access;

   procedure connect_database;

end Connect;
