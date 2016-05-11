--  Used for all testcases for SQLite driver

with AdaBase.Driver.Base.SQLite;
with AdaBase.Statement.Base.SQLite;

package Connect is

   --  All specific drivers renamed to "Database_Driver"
   subtype Database_Driver is AdaBase.Driver.Base.SQLite.SQLite_Driver;
   subtype Stmt_Type is AdaBase.Statement.Base.SQLite.SQLite_statement;
   subtype Stmt_Type_access is
      AdaBase.Statement.Base.SQLite.SQLite_statement_access;

   DR : Database_Driver;

   procedure connect_database;

end Connect;
