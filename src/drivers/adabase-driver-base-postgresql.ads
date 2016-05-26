--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Interfaces.Driver;
with AdaBase.Statement.Base.PostgreSQL;
with AdaBase.Connection.Base.PostgreSQL;
with Ada.Containers.Vectors;

package AdaBase.Driver.Base.PostgreSQL is

   package AID renames AdaBase.Interfaces.Driver;
   package SMT renames AdaBase.Statement.Base.PostgreSQL;
   package CON renames AdaBase.Connection.Base.PostgreSQL;

   type PostgreSQL_Driver is new Base_Driver and AID.iDriver with private;

   overriding
   function execute (driver : PostgreSQL_Driver; sql : String)
                     return Affected_Rows;


   function query          (driver     : PostgreSQL_Driver;
                            sql        : String)
                            return SMT.PostgreSQL_statement;

   function prepare        (driver     : PostgreSQL_Driver;
                            sql        : String)
                            return SMT.PostgreSQL_statement;

   function query_select   (driver     : PostgreSQL_Driver;
                            distinct   : Boolean := False;
                            tables     : String;
                            columns    : String;
                            conditions : String := blankstring;
                            groupby    : String := blankstring;
                            having     : String := blankstring;
                            order      : String := blankstring;
                            null_sort  : Null_Priority := native;
                            limit      : Trax_ID := 0;
                            offset     : Trax_ID := 0)
                            return SMT.PostgreSQL_statement;

   function prepare_select (driver     : PostgreSQL_Driver;
                            distinct   : Boolean := False;
                            tables     : String;
                            columns    : String;
                            conditions : String := blankstring;
                            groupby    : String := blankstring;
                            having     : String := blankstring;
                            order      : String := blankstring;
                            null_sort  : Null_Priority := native;
                            limit      : Trax_ID := 0;
                            offset     : Trax_ID := 0)
                            return SMT.PostgreSQL_statement;

   function call_stored_procedure (driver           : PostgreSQL_Driver;
                                   stored_procedure : String;
                                   proc_arguments   : String)
                                   return SMT.PostgreSQL_statement;

   function trait_query_buffers_used       (driver : PostgreSQL_Driver)
                                            return Boolean;

   procedure set_trait_query_buffers_used  (driver : PostgreSQL_Driver;
                                            trait  : Boolean);

private

   global_statement_counter : Trax_ID := 0;

   type PostgreSQL_Driver is new Base_Driver and AID.iDriver with
      record
         local_connection : aliased CON.PostgreSQL_Connection;
         async_cmd_mode   : Boolean := False;
      end record;

   overriding
   procedure private_connect (driver   : out PostgreSQL_Driver;
                              database : String;
                              username : String;
                              password : String;
                              hostname : String     := blankstring;
                              socket   : String     := blankstring;
                              port     : Posix_Port := portless);

   function sql_assemble (distinct   : Boolean := False;
                          tables     : String;
                          columns    : String;
                          conditions : String := blankstring;
                          groupby    : String := blankstring;
                          having     : String := blankstring;
                          order      : String := blankstring;
                          null_sort  : Null_Priority := native;
                          limit      : Trax_ID := 0;
                          offset     : Trax_ID := 0) return String;

   function private_statement (driver   : PostgreSQL_Driver;
                               sql      : String;
                               nextsets : String := "";
                               prepared : Boolean)
                               return SMT.PostgreSQL_statement;

   overriding
   procedure initialize (Object : in out PostgreSQL_Driver);

end AdaBase.Driver.Base.PostgreSQL;
