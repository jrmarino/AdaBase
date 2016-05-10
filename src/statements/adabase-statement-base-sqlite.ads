--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Connection.Base.SQLite;
with AdaBase.Bindings.SQLite;
with Ada.Containers.Vectors;

package AdaBase.Statement.Base.SQLite is

private

   function num_set_items (nv : String) return Natural;

--     procedure log_problem
--       (statement  : MySQL_statement;
--        category   : LogCategory;
--        message    : String;
--        pull_codes : Boolean := False;
--        break      : Boolean := False);

end AdaBase.Statement.Base.SQLite;
