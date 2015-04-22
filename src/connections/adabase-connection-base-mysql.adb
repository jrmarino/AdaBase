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
   function description (conn : MySQL_Connection) return AD.textual
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

      if conn.prop_connected then
         result := ABM.mysql_autocommit (handle => conn.handle,
                                         auto_mode => auto_mode);
         if result /= 0 then
            raise AUTOCOMMIT_FAIL;
         end if;
      end if;

      conn.prop_auto_commit := auto;
   end setAutoCommit;


   ----------------
   --  SqlState  --
   ----------------
   overriding
   function SqlState (conn : MySQL_Connection) return AD.TSqlState
   is
      result : ABM.ICS.chars_ptr;
   begin
      result := ABM.mysql_sqlstate (handle => conn.handle);
      declare
         convstr : constant String := ABM.ICS.Value (Item => result);
      begin
         return AD.TSqlState (convstr (1 .. 5));
      end;
   end SqlState;


   ---------------------
   --  driverMessage  --
   ---------------------
   overriding
   function driverMessage (conn : MySQL_Connection)
                           return AD.textual
   is
      result : ABM.ICS.chars_ptr;
   begin
      result := ABM.mysql_error (handle => conn.handle);
      return SUS (ABM.ICS.Value (Item => result));
   end driverMessage;


   ------------------
   --  driverCode  --
   ------------------
   overriding
   function driverCode (conn : MySQL_Connection) return AD.DriverCodes
   is
      result : ABM.my_uint;
   begin
      result := ABM.mysql_errno (handle => conn.handle);
      return AD.DriverCodes (result);
   end driverCode;


   --------------------
   --  lastInsertID  --
   --------------------
   overriding
   function lastInsertID (conn : MySQL_Connection) return AD.TraxID
   is
      result : ABM.my_ulonglong;
   begin
      result := ABM.mysql_insert_id (handle => conn.handle);
      return AD.TraxID (result);
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
   procedure connect (conn : out MySQL_Connection)
   is
      use type AD.textual;
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

      conn.handle := ABM.mysql_init (handle => conn.handle);

      if conn.mrc_socket = AD.blank then
         conn.handle := ABM.mysql_real_connect
           (handle      => conn.handle,
            host        => S2P (conn.mrc_host),
            user        => S2P (conn.mrc_user),
            passwd      => S2P (conn.mrc_password),
            db          => S2P (conn.mrc_db),
            port        => conn.mrc_port,
            unix_socket => ABM.ICS.Null_Ptr,
            client_flag => ABM.my_ulong (options));
      else
          conn.handle := ABM.mysql_real_connect
           (handle      => conn.handle,
            host        => ABM.ICS.Null_Ptr,
            user        => S2P (conn.mrc_user),
            passwd      => S2P (conn.mrc_password),
            db          => S2P (conn.mrc_db),
            port        => 0,
            unix_socket => S2P (conn.mrc_socket),
            client_flag => ABM.my_ulong (options));
      end if;

      conn.set_character_set;
      conn.setTransactionIsolation (conn.prop_trax_isolation);
      conn.setAutoCommit (conn.prop_auto_commit);

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

   exception

      when CONNECT_FAIL    => raise CONNECT_FAIL;
      when TRAXISOL_FAIL   => raise TRAXISOL_FAIL;
      when AUTOCOMMIT_FAIL => raise AUTOCOMMIT_FAIL;
      when CHARSET_FAIL    => raise CHARSET_FAIL;

   end connect;


   ------------------
   --  disconnect  --
   ------------------
   overriding
   procedure disconnect (conn : out MySQL_Connection)
   is
   begin
      ABM.mysql_close (handle => conn.handle);
      conn.prop_connected := False;
   end disconnect;


   ---------------
   --  execute  --
   ---------------
   overriding
   function  execute (conn : MySQL_Connection; sql : AD.textual)
                      return AD.AffectedRows
   is
      use type ABM.my_int;
      result : ABM.my_int;
      query  : constant ABM.ICS.chars_ptr :=
                        ABM.ICS.New_String (Str => AD.SU.To_String (sql));
      len    : constant ABM.my_ulong := ABM.my_ulong (AD.SU.Length (sql));
   begin
      result := ABM.mysql_real_query (handle => conn.handle,
                                      query  => query,
                                      length => len);
      if result /= 0 then
         raise QUERY_FAIL;
      end if;
      return AD.AffectedRows (result);
   end execute;


   -------------------------------
   --  setTransactionIsolation  --
   -------------------------------
   overriding
   procedure setTransactionIsolation (conn      : out MySQL_Connection;
                                      isolation :     AD.TransIsolation)
   is
      use type AD.textual;
      use type AD.TransIsolation;
      sql : AD.textual := AD.SU.To_Unbounded_String
            ("SET SESSION TRANSACTION ISOLATION LEVEL ") &
            AD.SU.To_Unbounded_String (AD.IsoKeywords (isolation));
      affected_rows : AD.AffectedRows;
   begin
      if conn.prop_connected then
         if isolation = conn.prop_trax_isolation then
            return;
         end if;
         affected_rows := execute (conn => conn, sql => sql);
      end if;

      conn.prop_trax_isolation := isolation;
   exception
      when QUERY_FAIL =>
         raise TRAXISOL_FAIL;
   end setTransactionIsolation;


   ---------------------------
   --  initializeStatement  --
   ---------------------------
   procedure initializeStatement (conn : MySQL_Connection;
                                  stmt : out AS.MySQL.MySQL_statement)
   is
   begin
      stmt.transfer_connection
        (connection  => conn.handle,
         error_mode  => conn.prop_error_mode,
         case_mode   => conn.prop_case_mode,
         string_mode => conn.prop_string_mode,
         max_blob    => conn.prop_max_blob,
         buffered    => conn.prop_buffered);
   end initializeStatement;


   ------------------------------------------------------------------------
   --  From this point on, the routines are private                       -                              --
   ------------------------------------------------------------------------

   -----------------------
   --  convert_version  --
   -----------------------
   function convert_version (mysql_version : Natural)
     return AD.textual
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
   function S2P (S : AD.textual) return ABM.ICS.chars_ptr
   is
   begin
      return ABM.ICS.New_String (Str => AD.SU.To_String (Source => S));
   end S2P;


   -------------------------
   --  set_character_set  --
   -------------------------
   procedure set_character_set (conn : MySQL_Connection)
   is
      use type AD.textual;
      sql : constant AD.textual :=
        SUS ("SET CHARACTER SET ") & conn.character_set;
      affected_rows : AD.AffectedRows;
   begin
      if conn.prop_connected then
         if conn.character_set /= AD.blank then
            affected_rows := execute (conn => conn, sql => sql);
         end if;
      end if;
   exception
      when QUERY_FAIL =>
         raise CHARSET_FAIL;
   end set_character_set;


end AdaBase.Connection.Base.MySQL;
