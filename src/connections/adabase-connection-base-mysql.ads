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

with AdaBase.Interfaces.Connection;
with AdaBase.Bindings.MySQL;
with Ada.Exceptions;

package AdaBase.Connection.Base.MySQL is

   package AIC renames AdaBase.Interfaces.Connection;
   package ABM renames AdaBase.Bindings.MySQL;
   package EX renames Ada.Exceptions;

   type MySQL_Connection is new Base_Connection and AIC.iConnection with private;
   type MySQL_Connection_Access is access all MySQL_Connection;

   overriding
   procedure setAutoCommit (conn : out MySQL_Connection; auto : Boolean);

   overriding
   procedure setCompressed (conn : out MySQL_Connection; compressed : Boolean);

   overriding
   function compressed (conn : MySQL_Connection) return Boolean;

   overriding
   procedure setUseBuffer (conn : out MySQL_Connection; buffered : Boolean);

   overriding
   function useBuffer (conn : MySQL_Connection) return Boolean;

   overriding
   procedure setMultiQuery (conn : out MySQL_Connection; multiple : Boolean);

   overriding
   function multiquery (conn : MySQL_Connection) return Boolean;

   overriding
   procedure setTransactionIsolation (conn      : out MySQL_Connection;
                                      isolation :     TransIsolation);

   overriding
   function description   (conn : MySQL_Connection) return String;

   overriding
   function SqlState      (conn : MySQL_Connection) return TSqlState;

   overriding
   function driverMessage (conn : MySQL_Connection) return String;

   overriding
   function driverCode    (conn : MySQL_Connection) return DriverCodes;

   overriding
   function lastInsertID  (conn : MySQL_Connection) return TraxID;

   overriding
   procedure commit       (conn : MySQL_Connection);

   overriding
   procedure rollback     (conn : MySQL_Connection);

   overriding
   procedure disconnect   (conn : out MySQL_Connection);

   overriding
   function  execute      (conn : MySQL_Connection;
                           sql  : String) return AffectedRows;

   overriding
   procedure connect (conn     : out MySQL_Connection;
                      database : String;
                      username : String;
                      password : String;
                      hostname : String := blankstring;
                      socket   : String := blankstring;
                      port     : PosixPort := portless);


   procedure use_result   (conn : MySQL_Connection;
                           result_handle : out ABM.MYSQL_RES_Access);

   procedure free_result  (conn : MySQL_Connection;
                           result_handle : ABM.MYSQL_RES_Access);

   procedure store_result (conn : MySQL_Connection;
                           result_handle : out ABM.MYSQL_RES_Access);


   -----------------------------------------------------------------------
   --  PREPARED STATEMENT FUNCTIONS                                     --
   -----------------------------------------------------------------------

   function prep_LastInsertID  (conn : MySQL_Connection;
                                stmt : ABM.MYSQL_STMT_Access) return TraxID;

   function prep_SqlState      (conn : MySQL_Connection;
                                stmt : ABM.MYSQL_STMT_Access) return TSqlState;

   function prep_DriverCode    (conn : MySQL_Connection;
                                stmt : ABM.MYSQL_STMT_Access)
                                return DriverCodes;

   function prep_DriverMessage (conn : MySQL_Connection;
                                stmt : ABM.MYSQL_STMT_Access) return String;

   procedure prep_free_result (conn : MySQL_Connection;
                               stmt : ABM.MYSQL_STMT_Access);

   procedure prep_store_result (conn : MySQL_Connection;
                                stmt : ABM.MYSQL_STMT_Access);

   NOT_WHILE_CONNECTED : exception;
   AUTOCOMMIT_FAIL     : exception;
   COMMIT_FAIL         : exception;
   ROLLBACK_FAIL       : exception;
   QUERY_FAIL          : exception;
   CONNECT_FAIL        : exception;
   TRAXISOL_FAIL       : exception;
   CHARSET_FAIL        : exception;
   INITIALIZE_FAIL     : exception;
   STMT_NOT_VALID      : exception;
   RESULT_FAIL         : exception;


private
   type MySQL_Connection is new Base_Connection and AIC.iConnection
     with record
      prop_compressed  : Boolean := True;
      prop_buffered    : Boolean := True;
      prop_multiquery  : Boolean := False;
      info_description : String (1 .. 24) := "MySQL 5.5+ native driver";

      handle           : ABM.MYSQL_Access;
      character_set    : conntext := blank;
   end record;

   function convert_version (mysql_version : Natural)
                             return conntext;

   function S2P (S : conntext) return ABM.ICS.chars_ptr;
   function S2P (S : String)   return ABM.ICS.chars_ptr;

   procedure set_character_set (conn : MySQL_Connection);

end AdaBase.Connection.Base.MySQL;
