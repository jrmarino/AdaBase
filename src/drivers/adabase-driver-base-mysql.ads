--
--  Copyright (c) 2015 John Marino <draco@marino.st>
--
--  Permission to use, copy, modify, and distribute this software for any
--  purpose with or without fee is hereby granted, provided that the above
--  copyright notice and this permission notice appear in all copies.
--
--  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
--  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
--  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
--  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
--  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
--  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
--  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--

with AdaBase.Interfaces.Driver;
with AdaBase.Connection.Base.MySQL;
with AdaBase.Statement;
with AdaBase.DataTypes;

package AdaBase.Driver.Base.MySQL is

   package AID renames AdaBase.Interfaces.Driver;
   package ACM renames AdaBase.Connection.Base.MySQL;
   package AS  renames AdaBase.Statement;
   package AD  renames AdaBase.DataTypes;

   type MySQL_Driver is new Base_Driver and AID.iDriver with private;

   overriding
   procedure disconnect (driver : out MySQL_Driver);

   overriding
   procedure commit (driver : MySQL_Driver);

   overriding
   procedure rollback (driver : MySQL_Driver);

   overriding
   function query  (driver : MySQL_Driver; sql : String)
                    return AS.Base'Class;

   overriding
   function last_insert_id (driver : MySQL_Driver) return AD.TraxID;

   overriding
   function last_sql_state (driver : MySQL_Driver) return AD.TSqlState;

   overriding
   function last_error_info (driver : MySQL_Driver) return AD.Error_Info;

   overriding
   function execute (driver : MySQL_Driver; sql : String)
                     return AD.AffectedRows;

   function trait_protocol_compressed (driver : MySQL_Driver) return Boolean;
   function trait_multiquery_enabled  (driver : MySQL_Driver) return Boolean;
   function trait_query_buffers_used  (driver : MySQL_Driver) return Boolean;

   procedure set_trait_protocol_compressed (driver : MySQL_Driver;
                                            trait  : Boolean);
   procedure set_trait_multiquery_enabled  (driver : MySQL_Driver;
                                            trait  : Boolean);
   procedure set_query_buffers_used        (driver : MySQL_Driver;
                                            trait  : Boolean);

   overriding
   procedure basic_connect (driver   : out MySQL_Driver;
                            database : String;
                            username : String;
                            password : String;
                            socket   : String);

   overriding
   procedure basic_connect (driver   : out MySQL_Driver;
                            database : String;
                            username : String;
                            password : String;
                            hostname : String;
                            port     : AD.PosixPort);
private

   backend : aliased ACM.MySQL_Connection;

   type MySQL_Driver is new Base_Driver and AID.iDriver with
      record

         --  connection : ACB.Base_Connection_Access := cow'Access;
         local_connection : ACM.MySQL_Connection_Access := null;
      database   : AD.textual := AD.blank;
      end record;

   procedure initialize (Object : in out MySQL_Driver);

   procedure private_connect (driver   : out MySQL_Driver;
                              database : String;
                              username : String;
                              password : String;
                              hostname : String := AD.blankstring;
                              socket   : String := AD.blankstring;
                              port     : AD.PosixPort := AD.portless);

end AdaBase.Driver.Base.MySQL;
