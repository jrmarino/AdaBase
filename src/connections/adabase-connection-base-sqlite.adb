--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with CommonText;

package body AdaBase.Connection.Base.SQLite is

   package CT renames CommonText;

   ---------------------
   --  setCompressed  --
   ---------------------
   overriding
   procedure setCompressed (conn : out SQLite_Connection; compressed : Boolean)
   is
   begin
      raise UNSUPPORTED_BY_SQLITE;
   end setCompressed;


   ------------------
   --  compressed  --
   ------------------
   overriding
   function compressed (conn : SQLite_Connection) return Boolean is
   begin
      return False;
   end compressed;


   ---------------------
   --  setMultiQuery  --
   ---------------------
   overriding
   procedure setMultiQuery (conn : out SQLite_Connection; multiple : Boolean)
   is
      pragma Unreferenced (conn);
   begin
      if not multiple then
         raise UNSUPPORTED_BY_SQLITE
           with "Multiple SQL statements cannot be disabled";
      end if;
   end setMultiQuery;


   ------------------
   --  multiquery  --
   ------------------
   overriding
   function multiquery (conn : SQLite_Connection) return Boolean is
   begin
      return True;
   end multiquery;


   --------------------
   --  setUseBuffer  --
   --------------------
   overriding
   procedure setUseBuffer (conn : out SQLite_Connection; buffered : Boolean) is
   begin
      raise UNSUPPORTED_BY_SQLITE;
   end setUseBuffer;


   -----------------
   --  useBuffer  --
   -----------------
   overriding
   function useBuffer (conn : SQLite_Connection) return Boolean is
   begin
      return False;
   end useBuffer;


   -------------------
   --  description  --
   -------------------
   overriding
   function description (conn : SQLite_Connection) return String is
   begin
      return conn.info_description;
   end description;


   ---------------------
   --  setAutoCommit  --
   ---------------------
   overriding
   procedure setAutoCommit (conn : out SQLite_Connection; auto : Boolean)
   is
      --  SQLite doesn't have a controllable setting for autocommit
      --  One indirectly controls it by issuing BEGIN command which inhibits it
      --  Autocommit is re-enabled when COMMIT or ROLLBACK is issued.
      --  Thus, we will have to simulate this by keeping track of "within"
      --  transaction and issuing BEGIN invisibly when autocommit is off
      --  (which is the AdaBase default)
   begin
      if not conn.prop_auto_commit and then auto and then conn.in_transaction
      then
         conn.commit;
      end if;
      conn.prop_auto_commit := auto;
   end setAutoCommit;


   ----------------
   --  SqlState  --
   ----------------
   overriding
   function SqlState (conn : SQLite_Connection) return TSqlState
   is
      --  Unsupported on SQLite, return constant HY000 (general error)
   begin
      return "HY000";
   end SqlState;


   ---------------
   --  PUTF82S  --
   ---------------
   function PUTF82S (cstr : BND.ICS.chars_ptr) return String
   is
      rawstr : String := BND.ICS.Value (cstr);
   begin
      return CT.UTF8S (CT.UTF8 (rawstr));
   end PUTF82S;


   ---------------------
   --  driverMessage  --
   ---------------------
   overriding
   function driverMessage (conn : SQLite_Connection) return String
   is
      result : BND.ICS.chars_ptr := BND.sqlite3_errmsg (db => conn.handle);
   begin
      return PUTF82S (result);
   end driverMessage;


   ------------------
   --  driverCode  --
   ------------------
   overriding
   function driverCode (conn : SQLite_Connection) return DriverCodes
   is
      result : BND.IC.int := BND.sqlite3_errcode (db => conn.handle);
   begin
      return DriverCodes (result);
   end driverCode;


   --------------------
   --  lastInsertID  --
   --------------------
   overriding
   function lastInsertID (conn : SQLite_Connection) return TraxID
   is
      result : BND.sql64;
   begin
      result := BND.sqlite3_last_insert_rowid (db => conn.handle);
      return TraxID (result);
   end lastInsertID;


   ----------------------------------
   --  rows_affected_by_execution  --
   ----------------------------------
   overriding
   function rows_affected_by_execution (conn : SQLite_Connection)
                                        return AffectedRows
   is
      result : BND.IC.int := BND.sqlite3_changes (db => conn.handle);
   begin
      return AffectedRows (result);
   end rows_affected_by_execution;


   ------------------
   --  disconnect  --
   ------------------
   overriding
   procedure disconnect (conn : out SQLite_Connection)
   is
      use type BND.sqlite3_Access;
      use type BND.IC.int;
      result : BND.IC.int;
   begin
      if conn.handle /= null then
         result := BND.sqlite3_close (db => conn.handle);
         if result = BND.SQLITE_OK then
            conn.handle := null;
         else
            raise DISCONNECT_FAILED with "return code =" & result'Img;
         end if;
      end if;
      conn.prop_active := False;
   end disconnect;


   -------------------------
   --  begin_transaction  --
   -------------------------
   procedure begin_transaction (conn : out SQLite_Connection) is
   begin
      execute (conn, "BEGIN TRANSACTION");
      conn.in_transaction := True;
   end begin_transaction;


   ----------------
   --  rollback  --
   ----------------
   overriding
   procedure rollback (conn : out SQLite_Connection) is
   begin
      execute (conn, "ROLLBACK");
      conn.in_transaction := False;
   end rollback;


   --------------
   --  commit  --
   --------------
   overriding
   procedure commit (conn : out SQLite_Connection) is
   begin
      execute (conn, "COMMIT");
      conn.in_transaction := False;
   end commit;


   ------------------
   --  Initialize  --
   ------------------
   overriding
   procedure Initialize (conn : in out SQLite_Connection) is
   begin
      conn.prop_trax_isolation := serializable;
   end Initialize;


   ----------------
   --  finalize  --
   ----------------
   overriding
   procedure finalize (conn : in out SQLite_Connection) is
   begin
      conn.disconnect;
   end finalize;


   ---------------
   --  execute  --
   ---------------
   overriding
   procedure execute (conn : out SQLite_Connection; sql : String)
   is
      --  Logic table: autocommit (Boolean) vs in_transaction (Boolean)
      --  -------------------------------------------------------------------
      --  autocommit = True  + in_transaction = True  = impossible
      --  autocommit = True  + in_transaction = False = BEGIN + exec + COMMIT
      --  autocommit = False + in_transaction = True  = exec
      --  autocommit = False + in_transaction = False = BEGIN + exec
   begin
      if conn.autoCommit then
         if conn.in_transaction then
            raise AUTOCOMMIT_FAIL
              with "Impossible: In transaction with autocommit set on";
         else
            conn.begin_transaction;
            conn.private_execute (sql);
            conn.commit;
         end if;
      else
         if conn.in_transaction then
            conn.private_execute (sql);
         else
            conn.begin_transaction;
            conn.private_execute (sql);
         end if;
      end if;
   end execute;


   -----------------------
   --  private_execute  --
   -----------------------
   procedure private_execute (conn : SQLite_Connection; sql : String)
   is
      use type BND.IC.int;
      result : BND.IC.int;
      query  : BND.ICS.chars_ptr := BND.ICS.New_String (Str => sql);
   begin
      result := BND.sqlite3_exec (db       => conn.handle,
                                  sql      => query,
                                  callback => BND.SYS.Null_Address,
                                  firstarg => BND.SYS.Null_Address,
                                  errmsg   => BND.SYS.Null_Address);
      BND.ICS.Free (query);
      if result /= BND.SQLITE_OK then
         raise QUERY_FAIL;
      end if;
   end private_execute;


   ---------------
   --  connect  --
   ---------------
   overriding
   procedure connect      (conn     : out SQLite_Connection;
                           database : String;
                           username : String := blankstring;
                           password : String := blankstring;
                           hostname : String := blankstring;
                           socket   : String := blankstring;
                           port     : PosixPort := portless)
   is
      pragma Unreferenced (username, password, hostname, socket, port);
      use type BND.IC.int;

      dbname   : CT.UTF8 := CT.SUTF8 (database);
      filename : BND.ICS.chars_ptr := BND.ICS.New_String (dbname);
      result   : BND.IC.int;
   begin
      if conn.prop_active then
         raise NOT_WHILE_CONNECTED;
      end if;
      result := BND.sqlite3_open (File_Name => filename,
                                  Handle    => conn.handle'Access);
      if result /= BND.SQLITE_OK then
         raise CONNECT_FAILED;
      end if;

      conn.prop_active := True;
      conn.info_server := CT.SUS ("Not applicable");
      conn.info_server_version := conn.info_server;

      declare
         result : BND.ICS.chars_ptr := BND.sqlite3_sourceid;
      begin
         conn.info_client := CT.SUS (BND.ICS.Value (Item => result));
      end;

      declare
         result : BND.ICS.chars_ptr := BND.sqlite3_libversion;
      begin
         conn.info_client := CT.SUS (BND.ICS.Value (Item => result));
      end;

      conn.setTransactionIsolation (conn.prop_trax_isolation);

   exception
      when NOT_WHILE_CONNECTED =>
         raise NOT_WHILE_CONNECTED with
           "Reconnection attempted during an active connection";
      when CONNECT_FAILED =>
         conn.disconnect;
         raise CONNECT_FAILED with "Failed to connect to " & database;
      when rest : others =>
         conn.disconnect;
         EX.Reraise_Occurrence (rest);
   end connect;


   -------------------------------
   --  setTransactionIsolation  --
   -------------------------------
   overriding
   procedure setTransactionIsolation (conn : out SQLite_Connection;
                                      isolation : TransIsolation)
   is
      sql : constant String := "PRAGMA read_uncommitted = ";
      TL  : constant String := "Transaction Level ";
   begin
      if not conn.prop_active then
         raise TRAXISOL_FAIL with "database not connected";
      end if;

      case isolation is
         when read_committed | repeatable_read =>
            raise UNSUPPORTED_BY_SQLITE with TL & isolation'Img;
         when serializable =>
            begin
               conn.execute (sql & "False");
            exception
               when others =>
                  raise TRAXISOL_FAIL with TL & isolation'Img;
            end;
         when read_uncommitted =>
            begin
               conn.execute (sql & "True");
            exception
               when others =>
                  raise TRAXISOL_FAIL with TL & isolation'Img;
            end;
      end case;
   end setTransactionIsolation;

end AdaBase.Connection.Base.SQLite;
