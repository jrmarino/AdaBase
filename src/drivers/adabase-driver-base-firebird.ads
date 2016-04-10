--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Interfaces.Driver;
--  with AdaBase.Connection.Base.Firebird;
with AdaBase.Statement;
with AdaBase.DataTypes;

package AdaBase.Driver.Base.Firebird is

   package AID renames AdaBase.Interfaces.Driver;
   --  package ACF renames AdaBase.Connection.Base.MySQL;
   package AS  renames AdaBase.Statement;
   package AD  renames AdaBase.DataTypes;

   type Firebird_Driver is new Base_Driver and AID.iDriver with private;

   overriding
   procedure query_drop_table        (driver      : Firebird_Driver;
                                      tables      : String;
                                      when_exists : Boolean := False;
                                      cascade     : Boolean := False);
   overriding
   procedure query_clear_table       (driver : Firebird_Driver;
                                      table  : String);

   overriding
   function last_insert_id (driver : Firebird_Driver) return AD.TraxID;

   overriding
   function last_sql_state (driver : Firebird_Driver) return AD.TSqlState;

   overriding
   function last_error_info (driver : Firebird_Driver) return AD.Error_Info;

   overriding
   function query (driver : Firebird_Driver; sql : String)
                   return  AS.Base'Class;

   overriding
   function execute (driver : Firebird_Driver; sql : String)
                     return AD.AffectedRows;

private

   type Firebird_Driver is new Base_Driver and AID.iDriver with
      record
         null;
      end record;

end AdaBase.Driver.Base.Firebird;
