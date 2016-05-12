--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with CommonText;
with Ada.Characters.Handling;
with Ada.Unchecked_Conversion;

package body AdaBase.Connection.Base.SQLite is

   package CT  renames CommonText;
   package ACH renames Ada.Characters.Handling;

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
      conn.private_execute ("BEGIN TRANSACTION");
      conn.in_transaction := True;
   end begin_transaction;


   ----------------
   --  rollback  --
   ----------------
   overriding
   procedure rollback (conn : out SQLite_Connection) is
   begin
      conn.private_execute ("ROLLBACK");
      conn.in_transaction := False;
   end rollback;


   --------------
   --  commit  --
   --------------
   overriding
   procedure commit (conn : out SQLite_Connection) is
   begin
      conn.private_execute ("COMMIT");
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
      sql8   : CT.UTF8 := CT.SUTF8 (sql);
      query  : BND.ICS.chars_ptr := BND.ICS.New_String (Str => sql8);
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


   -------------------------
   --  prepare_statement  --
   -------------------------
   function prepare_statement (conn : SQLite_Connection;
                               stmt : aliased out BND.sqlite3_stmt_Access;
                               sql  : String) return Boolean
   is
      use type BND.IC.int;
      sql8   : CT.UTF8 := CT.SUTF8 (sql);
      query  : BND.ICS.chars_ptr := BND.ICS.New_String (Str => sql8);
      nbyte  : BND.IC.int := BND.IC.int (sql8'Length + 1);
      result : BND.IC.int;
      dummy  : aliased BND.ICS.chars_ptr;
   begin
      result := BND.sqlite3_prepare_v2 (db     => conn.handle,
                                        zSql   => query,
                                        nByte  => nbyte,
                                        ppStmt => stmt'Access,
                                        pzTail => dummy'Access);
      BND.ICS.Free (query);
      return (result = BND.SQLITE_OK);
   end prepare_statement;


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
      BND.ICS.Free (filename);
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
         conn.info_client_version := CT.SUS (BND.ICS.Value (Item => result));
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


   --------------------------
   --  prep_markers_found  --
   --------------------------
   function prep_markers_found (conn : SQLite_Connection;
                                stmt : BND.sqlite3_stmt_Access) return Natural
   is
      result : BND.IC.int := BND.sqlite3_bind_parameter_count (stmt);
   begin
      return Natural (result);
   end prep_markers_found;


   ------------------------
   --  fields_in_result  --
   ------------------------
   function fields_in_result (conn : SQLite_Connection;
                              stmt : BND.sqlite3_stmt_Access) return Natural
   is
      result : BND.IC.int := BND.sqlite3_column_count (stmt);
   begin
      return Natural (result);
   end fields_in_result;


   -------------------
   --  field_table  --
   -------------------
   function field_table (conn  : SQLite_Connection;
                              stmt  : BND.sqlite3_stmt_Access;
                              index : Natural) return String
   is
      col : BND.IC.int := BND.IC.int (index);
      res : BND.ICS.chars_ptr := BND.sqlite3_column_table_name (stmt, col);
      str : constant String := CT.UTF8S (BND.ICS.Value (res));
   begin
      return str;
   end field_table;


   ------------------
   --  field_name  --
   ------------------
   function field_name (conn  : SQLite_Connection;
                        stmt  : BND.sqlite3_stmt_Access;
                        index : Natural) return String
   is
      col : BND.IC.int := BND.IC.int (index);
      res : BND.ICS.chars_ptr := BND.sqlite3_column_origin_name (stmt, col);
      str : constant String := CT.UTF8S (BND.ICS.Value (res));
   begin
      return str;
   end field_name;


   ----------------------
   --  field_database  --
   ----------------------
   function field_database (conn  : SQLite_Connection;
                            stmt  : BND.sqlite3_stmt_Access;
                            index : Natural) return String
   is
      col : BND.IC.int := BND.IC.int (index);
      res : BND.ICS.chars_ptr := BND.sqlite3_column_database_name (stmt, col);
      str : constant String := CT.UTF8S (BND.ICS.Value (res));
   begin
      return str;
   end field_database;


   ---------------------------
   --  get_field_meta_data  --
   ---------------------------
   procedure get_field_meta_data (conn  : SQLite_Connection;
                                  stmt  : BND.sqlite3_stmt_Access;
                                  database  : String;
                                  table     : String;
                                  column    : String;
                                  data_type : out BND.enum_field_types;
                                  nullable  : out Boolean)
   is
      c_database : BND.ICS.chars_ptr := BND.ICS.New_String (database);
      c_table    : BND.ICS.chars_ptr := BND.ICS.New_String (table);
      c_column   : BND.ICS.chars_ptr := BND.ICS.New_String (column);
      c_dtype    : aliased BND.ICS.chars_ptr;
      c_collset  : aliased BND.ICS.chars_ptr;
      c_notnull  : aliased BND.IC.int;
      c_primekey : aliased BND.IC.int;
      c_autoinc  : aliased BND.IC.int;
      result     : BND.IC.int;

      use type BND.IC.int;
   begin

      result := BND.sqlite3_table_column_metadata
        (Handle   => conn.handle,
         dbname   => c_database,
         table    => c_table,
         column   => c_column,
         datatype => c_dtype'Access,
         collseq  => c_collset'Access,
         notnull  => c_notnull'Access,
         primekey => c_primekey'Access,
         autoinc  => c_autoinc'Access);

      if result /= BND.SQLITE_OK then
         raise METADATA_FAIL;
      end if;

      nullable := (c_notnull = 0);

      --  We have to mimic "affinity" handling. SQLite is dynamically typed
      --  and AdaBase doesn't have this "numeric" concept.  Below is basically
      --  the same logic as sqlite3AffinityType() in sqlite3.c with some
      --  tweaks and additional types.

      declare
         raw : String := ACH.To_Upper (BND.ICS.Value (c_dtype));
         pt1 : String := CT.part_1 (raw, "(");
         typelen : constant Natural := pt1'Length;
         dtype : String (1 .. typelen) := pt1;
      begin
         if CT.contains (dtype, "INT") or else
           dtype = "YEAR" or else
           dtype = "BOOLEAN"
         then
            data_type := BND.SQLITE_INTEGER;
         elsif
           CT.contains (dtype, "BLOB") or else
           CT.contains (dtype, "BINARY") or else
           CT.contains (dtype, "BIT") or else
           dtype = "IMAGE"
         then
            data_type := BND.SQLITE_BLOB;
         elsif
           CT.contains (dtype, "REAL") or else
           CT.contains (dtype, "FLOA") or else
           CT.contains (dtype, "DOUB") or else
           CT.contains (dtype, "MONEY") or else
           dtype = "NUMBER" or else
           dtype = "DECIMAL" or else
           dtype = "NUMERIC"
         then
            data_type := BND.SQLITE_FLOAT;
         elsif dtype = "NULL" then
            data_type := BND.SQLITE_NULL;
         else
            data_type := BND.SQLITE_TEXT;
         end if;
      end;
      BND.ICS.Free (c_database);
      BND.ICS.Free (c_table);
      BND.ICS.Free (c_column);

   end get_field_meta_data;


   -----------------------
   --  reset_prep_stmt  --
   -----------------------
   procedure reset_prep_stmt (conn  : SQLite_Connection;
                              stmt  : BND.sqlite3_stmt_Access)
   is
      use type BND.IC.int;
      result : BND.IC.int := BND.sqlite3_reset (stmt);
   begin
      if result /= BND.SQLITE_OK then
         raise STMT_RESET_FAIL with "SQLite3 Reset error code" & result'Img;
      end if;
   end reset_prep_stmt;


   ------------------------
   --  retrieve_integer  --
   ------------------------
   function retrieve_integer (conn  : SQLite_Connection;
                              stmt  : BND.sqlite3_stmt_Access;
                              index : Natural) return AR.byte8
   is
      result : BND.sql64;
      col_index : constant BND.IC.int := BND.IC.int (index);
   begin
      result := BND.sqlite3_column_int64 (stmt, col_index);
      return AR.byte8 (result);
   end retrieve_integer;


   -----------------------
   --  retrieve_double  --
   -----------------------
   function retrieve_double (conn  : SQLite_Connection;
                             stmt  : BND.sqlite3_stmt_Access;
                             index : Natural) return AR.real18
   is
      result : BND.IC.double;
      col_index : constant BND.IC.int := BND.IC.int (index);
   begin
      result := BND.sqlite3_column_double (stmt, col_index);
      return AR.real18 (result);
   end retrieve_double;


   ---------------------
   --  field_is_null  --
   ---------------------
   function field_is_null (conn  : SQLite_Connection;
                           stmt  : BND.sqlite3_stmt_Access;
                           index : Natural) return Boolean
   is
      --  use type BND.enum_field_types;
      result    : BND.IC.int;
      col_index : constant BND.IC.int := BND.IC.int (index);
      null_type : constant Natural :=
                  BND.enum_field_types'Pos (BND.SQLITE_NULL);
   begin
      result := BND.sqlite3_column_type (stmt, col_index);
      return (Natural (result) = null_type);
   end field_is_null;


   ---------------------
   --  retrieve_text  --
   ---------------------
   function retrieve_text (conn  : SQLite_Connection;
                           stmt  : BND.sqlite3_stmt_Access;
                           index : Natural) return AR.textual
   is
      result : BND.ICS.chars_ptr;
      col_index : constant BND.IC.int := BND.IC.int (index);
   begin
      result := BND.sqlite3_column_text (stmt, col_index);
      declare
         asString : String := BND.ICS.Value (result);
      begin
         return CT.SUS (CT.SUTF8 (asString));
      end;
   end retrieve_text;


   ---------------------
   --  retrieve_blob  --
   ---------------------
   function retrieve_blob (conn  : SQLite_Connection;
                           stmt  : BND.sqlite3_stmt_Access;
                           index : Natural;
                           maxsz : Natural) return String
   is
      col_index : constant BND.IC.int := BND.IC.int (index);
      data_size : BND.IC.int := BND.sqlite3_column_bytes (stmt, col_index);
      str_size  : Natural := Natural (data_size);
   begin
      if maxsz < str_size then
         str_size := maxsz;
      end if;
      declare
         result : BND.ICS.chars_ptr;
         buflen : constant BND.IC.size_t := BND.IC.size_t (str_size);
         subtype data_buffer is BND.IC.char_array (1 .. buflen);
         type db_access is access all data_buffer;

         dba : db_access;

         function convert is new Ada.Unchecked_Conversion
           (Source => BND.ICS.chars_ptr, Target => db_access);

         function db_convert (dba : db_access) return String;
         function db_convert (dba : db_access) return String is
         begin
            declare
               result : String (1 .. str_size);
            begin
               for x in result'Range loop
                  result (x) := Character (dba.all (BND.IC.size_t (x)));
               end loop;
               return result;
            end;
         end db_convert;
      begin
         result := BND.sqlite3_column_blob (stmt, col_index);
         dba := convert (result);
         return db_convert (dba);
      end;
   end retrieve_blob;


   -----------------------
   --  prep_fetch_next  --
   -----------------------
   function prep_fetch_next (conn : SQLite_Connection;
                             stmt : BND.sqlite3_stmt_Access) return Boolean
   is
      use type BND.IC.int;
      step_result : BND.IC.int := BND.sqlite3_step (stmt);
   begin
      if step_result = BND.SQLITE_DONE then
         return False;
      end if;
      if step_result = BND.SQLITE_ROW then
         return True;
      end if;
      raise STMT_FETCH_FAIL with "Step() error " & step_result'Img;
   end prep_fetch_next;


   ---------------------
   --  prep_finalize  --
   ---------------------
   function prep_finalize (conn : SQLite_Connection;
                           stmt : BND.sqlite3_stmt_Access) return Boolean
   is
      use type BND.IC.int;
      result : BND.IC.int := BND.sqlite3_finalize (stmt);
   begin
      return (result = BND.SQLITE_OK);
   end prep_finalize;


   ----------------------
   --  marker_is_null  --
   ----------------------
   function marker_is_null (conn  : SQLite_Connection;
                            stmt  : BND.sqlite3_stmt_Access;
                            index : Natural) return Boolean
   is
      use type BND.IC.int;
      result    : BND.IC.int;
      col_index : constant BND.IC.int := BND.IC.int (index);
   begin
      result := BND.sqlite3_bind_null (stmt, col_index);
      return (result = BND.SQLITE_OK);
   end marker_is_null;


   -------------------------
   --  marker_is_integer  --
   -------------------------
   function marker_is_integer (conn  : SQLite_Connection;
                               stmt  : BND.sqlite3_stmt_Access;
                               index : Natural;
                               value : AR.byte8) return Boolean
   is
      use type BND.IC.int;
      result    : BND.IC.int;
      col_index : constant BND.IC.int := BND.IC.int (index);
      SL_value  : BND.sql64 := BND.sql64 (value);
   begin
      result := BND.sqlite3_bind_int64 (stmt, col_index, SL_value);
      return (result = BND.SQLITE_OK);
   end marker_is_integer;


   ------------------------
   --  marker_is_double  --
   ------------------------
   function marker_is_double (conn  : SQLite_Connection;
                              stmt  : BND.sqlite3_stmt_Access;
                              index : Natural;
                              value : AR.real18) return Boolean
   is
      use type BND.IC.int;
      result    : BND.IC.int;
      col_index : constant BND.IC.int := BND.IC.int (index);
      SL_value  : BND.IC.double := BND.IC.double (value);
   begin
      result := BND.sqlite3_bind_double (stmt, col_index, SL_value);
      return (result = BND.SQLITE_OK);
   end marker_is_double;


   ----------------------
   --  marker_is_text  --
   ----------------------
   function marker_is_text (conn  : SQLite_Connection;
                            stmt  : BND.sqlite3_stmt_Access;
                            index : Natural;
                            value : String;
                            cstr  : out BND.ICS.chars_ptr) return Boolean
   is
      use type BND.IC.int;
      result    : BND.IC.int;
      col_index : constant BND.IC.int := BND.IC.int (index);
      SL_value  : BND.ICS.chars_ptr := BND.ICS.New_String (value);
      SL_length : BND.IC.int := BND.IC.int (BND.ICS.Strlen (SL_value));
   begin
      result := BND.sqlite3_bind_text (Handle     => stmt,
                                       Index      => col_index,
                                       Text       => SL_value,
                                       nBytes     => SL_length,
                                       destructor => BND.SQLITE_STATIC);
      cstr := SL_value;
      return (result = BND.SQLITE_OK);
   end marker_is_text;


   ----------------------
   --  marker_is_blob  --
   ----------------------
   function marker_is_blob (conn  : SQLite_Connection;
                            stmt  : BND.sqlite3_stmt_Access;
                            index : Natural;
                            value : String) return Boolean
   is
      use type BND.IC.int;
      result    : BND.IC.int;
      col_index : constant BND.IC.int := BND.IC.int (index);
      len       : constant BND.IC.size_t := BND.IC.size_t (value'Length);
      SL_length : BND.IC.int := BND.IC.int (len);
      SL_value  : BND.ICS.char_array_access :=
                  new BND.IC.char_array (1 .. len);
   begin
      SL_value.all := BND.IC.To_C (value, False);

      result := BND.sqlite3_bind_blob (Handle     => stmt,
                                       Index      => col_index,
                                       binary     => SL_value,
                                       nBytes     => SL_length,
                                       destructor => BND.SQLITE_TRANSIENT);

--      free_binary (SL_value);
      return (result = BND.SQLITE_OK);
   end marker_is_blob;

end AdaBase.Connection.Base.SQLite;
