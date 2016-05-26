--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Interfaces.Driver;
with AdaBase.Connection.Base.MySQL;
with AdaBase.Statement.Base.MySQL;

package AdaBase.Driver.Base.MySQL is

   package AID renames AdaBase.Interfaces.Driver;
   package ACM renames AdaBase.Connection.Base.MySQL;
   package ASM renames AdaBase.Statement.Base.MySQL;

   type MySQL_Driver is new Base_Driver and AID.iDriver with private;

   overriding
   function execute (driver : MySQL_Driver; sql : String)
                     return Affected_Rows;

   function trait_protocol_compressed (driver : MySQL_Driver) return Boolean;

   function trait_query_buffers_used  (driver : MySQL_Driver) return Boolean;

   procedure set_trait_protocol_compressed (driver : MySQL_Driver;
                                            trait  : Boolean);

   procedure set_trait_query_buffers_used  (driver : MySQL_Driver;
                                            trait  : Boolean);

   function query          (driver     : MySQL_Driver;
                            sql        : String)
                            return ASM.MySQL_statement;

   function prepare        (driver     : MySQL_Driver;
                            sql        : String)
                            return ASM.MySQL_statement;

   function query_select   (driver     : MySQL_Driver;
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
                            return ASM.MySQL_statement;

   function prepare_select (driver     : MySQL_Driver;
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
                            return ASM.MySQL_statement;

   function call_stored_procedure (driver           : MySQL_Driver;
                                   stored_procedure : String;
                                   proc_arguments   : String)
                                   return ASM.MySQL_statement;

private

   type MySQL_Driver is new Base_Driver and AID.iDriver with
      record
         local_connection : aliased ACM.MySQL_Connection;
      end record;

   overriding
   procedure private_connect (driver   : out MySQL_Driver;
                              database : String;
                              username : String;
                              password : String;
                              hostname : String     := blankstring;
                              socket   : String     := blankstring;
                              port     : Posix_Port := portless);

   function private_query   (driver : MySQL_Driver; sql : String)
                             return ASM.MySQL_statement;

   function private_prepare (driver : MySQL_Driver; sql : String)
                             return ASM.MySQL_statement;

   function sql_assemble (driver     : MySQL_Driver;
                          distinct   : Boolean := False;
                          tables     : String;
                          columns    : String;
                          conditions : String := blankstring;
                          groupby    : String := blankstring;
                          having     : String := blankstring;
                          order      : String := blankstring;
                          null_sort  : Null_Priority := native;
                          limit      : Trax_ID := 0;
                          offset     : Trax_ID := 0) return String;

   overriding
   procedure initialize (Object : in out MySQL_Driver);

end AdaBase.Driver.Base.MySQL;
