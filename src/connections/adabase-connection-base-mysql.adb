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

package body AdaBase.Connection.Base.MySQL is


   ---------------------
   --  setCompressed  --
   ---------------------
   overriding
   procedure setCompressed (conn : out MySQL_Connection; compressed : Boolean)
   is
   begin
      if conn.prop_active then
         raise NOT_WHILE_CONNECTED;
      end if;
      conn.prop_compressed := compressed;
   end setCompressed;


   ------------------
   --  compressed  --
   ------------------
   overriding
   function compressed (conn : MySQL_Connection) return Boolean
   is
   begin
      return conn.prop_compressed;
   end compressed;


   ---------------------
   --  setMultiQuery  --
   ---------------------
   overriding
   procedure setMultiQuery (conn : out MySQL_Connection; multiple : Boolean)
   is
   begin
      if conn.prop_active then
         raise NOT_WHILE_CONNECTED;
      end if;
      conn.prop_multiquery := multiple;
   end setMultiQuery;


   ------------------
   --  multiquery  --
   ------------------
   overriding
   function multiquery (conn : MySQL_Connection) return Boolean
   is
   begin
      return conn.prop_multiquery;
   end multiquery;


   --------------------
   --  setUseBuffer  --
   --------------------
   overriding
   procedure setUseBuffer (conn : out MySQL_Connection; buffered : Boolean)
   is
   begin
      conn.prop_buffered := buffered;
   end setUseBuffer;


   -----------------
   --  useBuffer  --
   -----------------
   overriding
   function useBuffer (conn : MySQL_Connection) return Boolean
   is
   begin
      return conn.prop_buffered;
   end useBuffer;


   -------------------
   --  description  --
   -------------------
   overriding
   function description (conn : MySQL_Connection) return String
   is
   begin
      return conn.info_description;
   end description;


   ---------------------
   --  setAutoCommit  --
   ---------------------
   overriding
   procedure setAutoCommit (conn : out MySQL_Connection; auto : Boolean)
   is
      use type ABM.my_bool;
      auto_mode : ABM.my_bool := 0;
      result    : ABM.my_bool;
   begin
      if auto then
         auto_mode := 1;
      end if;

      if conn.prop_active then
         result := ABM.mysql_autocommit (handle => conn.handle,
                                         mode   => auto_mode);
         if result /= 0 then
            raise AUTOCOMMIT_FAIL with "autocommit result = " & result'Img;
         end if;
      end if;

      conn.prop_auto_commit := auto;
   end setAutoCommit;


   ----------------
   --  SqlState  --
   ----------------
   overriding
   function SqlState (conn : MySQL_Connection) return TSqlState
   is
      result : ABM.ICS.chars_ptr;
   begin
      result := ABM.mysql_sqlstate (handle => conn.handle);
      declare
         convstr : constant String := ABM.ICS.Value (Item => result);
      begin
         return TSqlState (convstr (1 .. 5));
      end;
   end SqlState;


   ---------------------
   --  driverMessage  --
   ---------------------
   overriding
   function driverMessage (conn : MySQL_Connection)
                           return String
   is
      result : ABM.ICS.chars_ptr;
   begin
      result := ABM.mysql_error (handle => conn.handle);
      return ABM.ICS.Value (Item => result);
   end driverMessage;


   ------------------
   --  driverCode  --
   ------------------
   overriding
   function driverCode (conn : MySQL_Connection) return DriverCodes
   is
      result : ABM.my_uint;
   begin
      result := ABM.mysql_errno (handle => conn.handle);
      return DriverCodes (result);
   end driverCode;


   --------------------
   --  lastInsertID  --
   --------------------
   overriding
   function lastInsertID (conn : MySQL_Connection) return TraxID
   is
      result : ABM.my_ulonglong;
   begin
      result := ABM.mysql_insert_id (handle => conn.handle);
      return TraxID (result);
   end lastInsertID;


   --------------
   --  commit  --
   --------------
   overriding
   procedure commit (conn : MySQL_Connection)
   is
      use type ABM.my_bool;
      result : ABM.my_bool;
   begin
      result := ABM.mysql_commit (handle => conn.handle);

      if result /= 0 then
         raise COMMIT_FAIL;
      end if;
   end commit;


   ----------------
   --  rollback  --
   ----------------
   overriding
   procedure rollback (conn : MySQL_Connection)
   is
      use type ABM.my_bool;
      result : ABM.my_bool;
   begin
      result := ABM.mysql_rollback (handle => conn.handle);

      if result /= 0 then
         raise ROLLBACK_FAIL;
      end if;
   end rollback;


   ----------------
   --  connect  --
   ----------------
   overriding
   procedure connect (conn     : out MySQL_Connection;
                      database : String;
                      username : String;
                      password : String;
                      hostname : String := blankstring;
                      socket   : String := blankstring;
                      port     : PosixPort := portless)
   is
      use type ABM.MYSQL_Access;
      options : Natural := 0;
      opt_compress : constant Natural := 2 ** (ABM.client_flag_order'Pos
                                               (ABM.CLIENT_COMPRESS) - 1);
      opt_multi : constant Natural := 2 ** (ABM.client_flag_order'Pos
                                            (ABM.CLIENT_MULTI_RESULTS) - 1);
   begin
      if conn.prop_active then
         raise NOT_WHILE_CONNECTED;
      end if;

      if conn.prop_compressed then
         options := options + opt_compress;
      end if;

      if conn.prop_multiquery then
         options := options + opt_multi;
      end if;

      conn.handle := ABM.mysql_init (null);
      if conn.handle = null then
         raise INITIALIZE_FAIL;
      end if;

      declare
         hoststr   : ABM.ICS.chars_ptr := ABM.ICS.Null_Ptr;
         userstr   : ABM.ICS.chars_ptr := S2P (username);
         passwdstr : ABM.ICS.chars_ptr := S2P (password);
         dbstr     : ABM.ICS.chars_ptr := S2P (database);
         socketstr : ABM.ICS.chars_ptr := ABM.ICS.Null_Ptr;
         portval   : ABM.my_uint       := 0;
         test      : ABM.MYSQL_Access  := null;

      begin
         if socket = blankstring then
            hoststr   := S2P (hostname);
            portval   := ABM.my_uint (port);
         else
            socketstr := S2P (socket);
         end if;
         test := ABM.mysql_real_connect
              (handle      => conn.handle,
               host        => hoststr,
               user        => userstr,
               passwd      => passwdstr,
               db          => dbstr,
               port        => portval,
               unix_socket => socketstr,
               client_flag => ABM.my_ulong (options));
         ABM.ICS.Free (userstr);
         ABM.ICS.Free (passwdstr);
         ABM.ICS.Free (dbstr);
         if socket = blankstring then
            ABM.ICS.Free (hoststr);
         else
            ABM.ICS.Free (socketstr);
         end if;
         if test = null then
            raise CONNECT_FAIL;
         end if;
      end;

      conn.prop_active := True;

      declare
         --  populate client version information
         result : ABM.my_ulong;
      begin
         result := ABM.mysql_get_client_version;
         conn.info_client_version := convert_version (Positive (result));
      end;

      declare
         --  populate client information
         result : ABM.ICS.chars_ptr;
      begin
         result := ABM.mysql_get_client_info;
         conn.info_client := SUS (ABM.ICS.Value (Item => result));
      end;

      declare
         --  populate server version information
         result : ABM.my_ulong;
      begin
         result := ABM.mysql_get_server_version (handle => conn.handle);
         conn.info_server_version := convert_version (Positive (result));
      end;

      declare
         --  populate server information
         result : ABM.ICS.chars_ptr;
      begin
         result := ABM.mysql_get_server_info (handle => conn.handle);
         conn.info_server := SUS (ABM.ICS.Value (Item => result));
      end;

      conn.set_character_set;
      conn.setTransactionIsolation (conn.prop_trax_isolation);
      conn.setAutoCommit (conn.prop_auto_commit);

   exception
      when NOT_WHILE_CONNECTED =>
         raise NOT_WHILE_CONNECTED with
           "Reconnection attempted during an active connection";
      when INITIALIZE_FAIL =>
         raise INITIALIZE_FAIL with
           "Failed to allocate enough memory for MySQL connection object";
      when CONNECT_FAIL =>
         conn.disconnect;
         raise CONNECT_FAIL with
           "Failed to connect to " & database;
      when rest : others =>
         conn.disconnect;
         EX.Reraise_Occurrence (rest);

   end connect;


   ------------------
   --  disconnect  --
   ------------------
   overriding
   procedure disconnect (conn : out MySQL_Connection)
   is
      use type ABM.MYSQL_Access;
   begin
      if conn.handle /= null then
         ABM.mysql_close (handle => conn.handle);
         conn.handle := null;
      end if;
      conn.prop_active := False;
   end disconnect;


   ---------------
   --  execute  --
   ---------------
   overriding
   procedure execute (conn : MySQL_Connection; sql : String)
   is
      use type ABM.my_int;
      result : ABM.my_int;
      query  : ABM.ICS.chars_ptr := ABM.ICS.New_String (Str => sql);
      len    : constant ABM.my_ulong := ABM.my_ulong (ABM.ICS.Strlen (query));
   begin
      result := ABM.mysql_real_query (handle   => conn.handle,
                                      stmt_str => query,
                                      length   => len);
      ABM.ICS.Free (Item => query);
      if result /= 0 then
         raise QUERY_FAIL;
      end if;
   end execute;


   -------------------------------
   --  setTransactionIsolation  --
   -------------------------------
   overriding
   procedure setTransactionIsolation (conn      : out MySQL_Connection;
                                      isolation :     TransIsolation)
   is
      use type conntext;
      use type TransIsolation;
      sql : constant String := "SET SESSION TRANSACTION ISOLATION LEVEL " &
                               IsoKeywords (isolation);
   begin
      if conn.prop_active then
         conn.execute (sql);
      end if;

      conn.prop_trax_isolation := isolation;
   exception
      when QUERY_FAIL =>
         raise TRAXISOL_FAIL with sql;
   end setTransactionIsolation;


   -------------------
   --  free_result  --
   -------------------
   procedure free_result (conn : MySQL_Connection;
                          result_handle : ABM.MYSQL_RES_Access)
   is
   begin
      ABM.mysql_free_result (handle => result_handle);
   end free_result;


   -------------------
   --  use_result  --
   -------------------
   procedure use_result (conn : MySQL_Connection;
                         result_handle : out ABM.MYSQL_RES_Access)
   is
      use type ABM.MYSQL_RES_Access;
   begin
      result_handle := ABM.mysql_use_result (handle => conn.handle);
      if result_handle = null then
         raise RESULT_FAIL with "Direct statement null use result";
      end if;
   end use_result;


   --------------------
   --  store_result  --
   --------------------
   procedure store_result (conn : MySQL_Connection;
                           result_handle : out ABM.MYSQL_RES_Access)
   is
      use type ABM.MYSQL_RES_Access;
   begin
      result_handle := ABM.mysql_store_result (handle => conn.handle);
      if result_handle = null then
         raise RESULT_FAIL with "Direct statement null store result";
      end if;
   end store_result;


   -------------------
   --  field_count  --
   -------------------
   function field_count (conn : MySQL_Connection) return Natural
   is
      result : ABM.my_uint;
   begin
      result := ABM.mysql_field_count (handle => conn.handle);
      return Natural (result);
   end field_count;


   ----------------------------------
   --  rows_affected_by_execution  --
   ----------------------------------
   overriding
   function rows_affected_by_execution (conn : MySQL_Connection)
                                        return AffectedRows
   is
      result : ABM.my_ulonglong;
   begin
      result := ABM.mysql_affected_rows (handle => conn.handle);
      return AffectedRows (result);
   end rows_affected_by_execution;


   ------------------------
   --  fields_in_result  --
   ------------------------
   function fields_in_result (conn : MySQL_Connection;
                              result_handle : ABM.MYSQL_RES_Access)
                              return Natural
   is
      result : ABM.my_uint;
   begin
      result := ABM.mysql_num_fields (handle => result_handle);
      return Natural (result);
   end fields_in_result;


   ----------------------
   --  rows_in_result  --
   ----------------------
   function rows_in_result (conn : MySQL_Connection;
                            result_handle : ABM.MYSQL_RES_Access)
                            return AffectedRows
   is
      result : ABM.my_ulonglong;
   begin
      result := ABM.mysql_num_rows (handle => result_handle);
      return AffectedRows (result);
   end rows_in_result;


   -------------------
   --  fetch_field  --
   -------------------
   function fetch_field (conn : MySQL_Connection;
                         result_handle : ABM.MYSQL_RES_Access)
                         return ABM.MYSQL_FIELD_Access
   is
   begin
      return ABM.mysql_fetch_field (handle => result_handle);
   end fetch_field;


   ------------------------
   --  field_name_field  --
   ------------------------
   function field_name_field (conn : MySQL_Connection;
                              field : ABM.MYSQL_FIELD_Access) return String
   is
      result : constant String := ABM.ICS.Value (Item => field.name);
   begin
      return result;
   end field_name_field;


   ------------------------
   --  field_name_table  --
   ------------------------
   function field_name_table (conn : MySQL_Connection;
                              field : ABM.MYSQL_FIELD_Access) return String
   is
      result : constant String := ABM.ICS.Value (Item => field.table);
   begin
      return result;
   end field_name_table;


   ---------------------------
   --  field_name_database  --
   ---------------------------
   function field_name_database (conn : MySQL_Connection;
                                 field : ABM.MYSQL_FIELD_Access) return String
   is
      result : constant String := ABM.ICS.Value (Item => field.db);
   begin
      return result;
   end field_name_database;


   -----------------------
   --  field_data_type  --
   -----------------------
   procedure field_data_type (conn : MySQL_Connection;
                              field : ABM.MYSQL_FIELD_Access;
                              std_type : out field_types;
                              size     : out Natural)
   is
      type flagtype is mod 2 ** 16;
      flag_unsigned : constant flagtype := 2 ** 5;
      mytype   : constant ABM.enum_field_types := field.field_type;
      flags    : constant flagtype := flagtype (field.flags);
      unsigned : constant Boolean := (flags and flag_unsigned) > 0;
      decimals : constant Natural := Natural (field.decimals);
      fieldlen : constant Natural := Natural (field.length);
      maxlen   : constant Natural := Natural (field.max_length);
   begin
      case mytype is
         when ABM.MYSQL_TYPE_FLOAT      => std_type := ft_real9;
         when ABM.MYSQL_TYPE_DOUBLE     => std_type := ft_real18;
         when ABM.MYSQL_TYPE_DECIMAL    |
              ABM.MYSQL_TYPE_NEWDECIMAL =>
            if decimals <= 9 then
               std_type := ft_real9;
            else
               std_type := ft_real18;
            end if;
         when ABM.MYSQL_TYPE_TINY =>
            if unsigned then
               if fieldlen = 1 then
                  std_type := ft_nbyte0;
               else
                  std_type := ft_nbyte1;
               end if;
            else
               std_type := ft_byte1;
            end if;
         when ABM.MYSQL_TYPE_SHORT =>
            if unsigned then
               std_type := ft_nbyte2;
            else
               std_type := ft_byte2;
            end if;
         when ABM.MYSQL_TYPE_INT24 =>
            if unsigned then
               std_type := ft_nbyte3;
            else
               std_type := ft_byte3;
            end if;
         when ABM.MYSQL_TYPE_LONG =>
            if unsigned then
               std_type := ft_nbyte4;
            else
               std_type := ft_byte4;
            end if;
         when ABM.MYSQL_TYPE_LONGLONG =>
            if unsigned then
               std_type := ft_nbyte8;
            else
               std_type := ft_byte8;
            end if;
         when ABM.MYSQL_TYPE_TIMESTAMP   |
              ABM.MYSQL_TYPE_DATE        |
              ABM.MYSQL_TYPE_TIME        |
              ABM.MYSQL_TYPE_DATETIME    |
              ABM.MYSQL_TYPE_YEAR        |
              ABM.MYSQL_TYPE_NEWDATE     => std_type := ft_timestamp;
         when ABM.MYSQL_TYPE_ENUM        => std_type := ft_enumtype;
         when ABM.MYSQL_TYPE_SET         => std_type := ft_settype;
         when ABM.MYSQL_TYPE_BIT =>
            case maxlen is
               when 0 .. 1               => std_type := ft_nbyte0;
               when others               => std_type := ft_chain;
            end case;
         when ABM.MYSQL_TYPE_TINY_BLOB   |
              ABM.MYSQL_TYPE_BLOB        |
              ABM.MYSQL_TYPE_MEDIUM_BLOB |
              ABM.MYSQL_TYPE_LONG_BLOB   |
              ABM.MYSQL_TYPE_VARCHAR     |
              ABM.MYSQL_TYPE_VAR_STRING  |
              ABM.MYSQL_TYPE_STRING      =>
            declare
               use type ABM.MY_CHARSET_INFO;
               chsetnr  : constant Natural := Natural (field.charsetnr);
               bin_set  : constant Natural := 63;
               binary   : constant Boolean := (chsetnr = bin_set);
               csinfo   : aliased ABM.MY_CHARSET_INFO;
            begin
               if binary then
                  std_type := ft_chain;
               else
                  ABM.mysql_get_character_set_info (handle => conn.handle,
                                                    cs => csinfo'Access);
                  case csinfo.mbmaxlen is
                     when 1  => std_type := ft_textual;
                     when 2  => std_type := ft_widetext;
                     when 4  => std_type := ft_supertext;
                     when others =>
                        raise BINDING_FAIL with
                          "Unexpected character set maximum set";
                  end case;
               end if;
            end;
         when ABM.MYSQL_TYPE_GEOMETRY =>
            raise BINDING_FAIL with
              "Geometry type not currently supported";
         when ABM.MYSQL_TYPE_NULL     => std_type := ft_textual;

      end case;
      case mytype is
         when ABM.MYSQL_TYPE_BIT =>
            case maxlen is
               when 0 .. 1               => size := 0;
               when others               => size := ((maxlen - 1) / 8) + 1;
            end case;
         when ABM.MYSQL_TYPE_TINY_BLOB   |
              ABM.MYSQL_TYPE_BLOB        |
              ABM.MYSQL_TYPE_MEDIUM_BLOB |
              ABM.MYSQL_TYPE_LONG_BLOB   |
              ABM.MYSQL_TYPE_VARCHAR     |
              ABM.MYSQL_TYPE_VAR_STRING  |
              ABM.MYSQL_TYPE_STRING      => size := maxlen;
         when others                     => size := 0;
      end case;
   end field_data_type;


   -------------------------
   --  field_allows_null  --
   -------------------------
   function field_allows_null (conn : MySQL_Connection;
                               field : ABM.MYSQL_FIELD_Access)
                               return Boolean
   is
      type flagtype is mod 2 ** 16;
      flag_null : constant flagtype := 2 ** 1;
      flags     : constant flagtype := flagtype (field.flags);
      permitted : constant Boolean := (flags and flag_null) > 0;
   begin
      return permitted;
   end field_allows_null;




   -------------------------
   --  prep_LastInsertID  --
   -------------------------
   function prep_LastInsertID (conn : MySQL_Connection;
                               stmt : ABM.MYSQL_STMT_Access) return TraxID
   is
      use type ABM.MYSQL_STMT_Access;
      result : ABM.my_ulonglong;
   begin
      if stmt = null then
         raise STMT_NOT_VALID with "PREP: Last Insert ID";
      end if;
      result := ABM.mysql_stmt_insert_id (handle => stmt);
      return TraxID (result);
   end prep_LastInsertID;


   ---------------------
   --  prep_SqlState  --
   ---------------------
   function prep_SqlState     (conn : MySQL_Connection;
                               stmt : ABM.MYSQL_STMT_Access) return TSqlState
   is
      result : ABM.ICS.chars_ptr;
   begin
      result := ABM.mysql_stmt_sqlstate (handle => stmt);
      declare
         convstr : constant String := ABM.ICS.Value (Item => result);
      begin
         return TSqlState (convstr (1 .. 5));
      end;
   end prep_SqlState;


   -----------------------
   --  prep_DriverCode  --
   -----------------------
   function prep_DriverCode (conn : MySQL_Connection;
                             stmt : ABM.MYSQL_STMT_Access) return DriverCodes
   is
      result : ABM.my_uint;
   begin
      result := ABM.mysql_stmt_errno (handle => stmt);
      return DriverCodes (result);
   end prep_DriverCode;


   --------------------------
   --  prep_DriverMessage  --
   --------------------------
   function prep_DriverMessage (conn : MySQL_Connection;
                                stmt : ABM.MYSQL_STMT_Access) return String
   is
      result : ABM.ICS.chars_ptr;
   begin
      result := ABM.mysql_stmt_error (handle => stmt);
      return ABM.ICS.Value (Item => result);
   end prep_DriverMessage;


   ------------------------
   --  prep_free_result  --
   ------------------------
   procedure prep_free_result (conn : MySQL_Connection;
                               stmt : ABM.MYSQL_STMT_Access)
   is
      use type ABM.my_bool;
      result : ABM.my_bool;
   begin
      result := ABM.mysql_stmt_free_result (handle => stmt);
      if result /= 0 then
         raise RESULT_FAIL with "Prepared statement free result";
      end if;
   end prep_free_result;


   -------------------------
   --  prep_store_result  --
   -------------------------
   procedure prep_store_result (conn : MySQL_Connection;
                                stmt : ABM.MYSQL_STMT_Access)
   is
      use type ABM.my_int;
      result : ABM.my_int;
   begin
      result := ABM.mysql_stmt_store_result (handle => stmt);
      if result /= 0 then
         raise RESULT_FAIL with "Prepared statement store result";
      end if;
   end prep_store_result;





   ------------------------------------------------------------------------
   --  From this point on, the routines are private                       -                              --
   ------------------------------------------------------------------------


   ----------------
   --  finalize  --
   ----------------
   overriding
   procedure finalize (conn : in out MySQL_Connection) is
   begin
      conn.disconnect;
   end finalize;


   -----------------------
   --  convert_version  --
   -----------------------
   function convert_version (mysql_version : Natural)
     return conntext
   is
      raw : constant String := mysql_version'Img;
   begin
      if raw'Length > 6 then
         return SUS (raw (2 .. 3) & '.' & raw (4 .. 5) & '.' & raw (6 .. 7));
      else
         return SUS (raw (2) & '.' & raw (3 .. 4) & '.' & raw (5 .. 6));
      end if;
   end convert_version;


   -----------
   --  S2P  --
   -----------
   function S2P (S : conntext) return ABM.ICS.chars_ptr
   is
   begin
      return ABM.ICS.New_String (Str => SU.To_String (Source => S));
   end S2P;


   -----------
   --  S2P  --
   -----------
   function S2P (S : String) return ABM.ICS.chars_ptr
   is
   begin
      return ABM.ICS.New_String (Str => S);
   end S2P;


   -------------------------
   --  set_character_set  --
   -------------------------
   procedure set_character_set (conn : MySQL_Connection)
   is
      use type conntext;
      sql : constant String := "SET CHARACTER SET " & USS (conn.character_set);
   begin
      if conn.prop_active then
         if conn.character_set /= SU.Null_Unbounded_String then
            execute (conn => conn, sql => sql);
         end if;
      end if;
   exception
      when QUERY_FAIL =>
         raise CHARSET_FAIL with sql;
   end set_character_set;


end AdaBase.Connection.Base.MySQL;
