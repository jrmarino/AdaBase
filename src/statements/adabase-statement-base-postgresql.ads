--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

--  with AdaBase.Connection.Base.PostgreSQL;
--  with AdaBase.Bindings.PostgreSQL;

package AdaBase.Statement.Base.PostgreSQL is

private

   function reformat_markers (parameterized_sql : String) return String;

end AdaBase.Statement.Base.PostgreSQL;
