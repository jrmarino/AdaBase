--  Used for all testcases for MySQL driver

with AdaBase.Driver.Base.PostgreSQL;
with AdaBase.Statement.Base.PostgreSQL;

package Connect is

   --  All specific drivers renamed to "Database_Driver"
   subtype Database_Driver is AdaBase.Driver.Base.PostgreSQL.PostgreSQL_Driver;
   subtype Stmt_Type is AdaBase.Statement.Base.PostgreSQL.PostgreSQL_statement;
   subtype Stmt_Type_access is
      AdaBase.Statement.Base.PostgreSQL.PostgreSQL_statement_access;

   DR : Database_Driver;

   procedure connect_database;

end Connect;
