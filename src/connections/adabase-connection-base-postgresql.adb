--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Characters.Handling;

package body AdaBase.Connection.Base.PostgreSQL is

   package ACH renames Ada.Characters.Handling;

   ---------------------
   --  setCompressed  --
   ---------------------
   overriding
   procedure setCompressed (conn : out PostgreSQL_Connection; compressed : Boolean)
   is
   begin
      raise UNSUPPORTED_BY_PGSQL;
   end setCompressed;


   ------------------
   --  compressed  --
   ------------------
   overriding
   function compressed (conn : PostgreSQL_Connection) return Boolean is
   begin
      return False;
   end compressed;


   --------------------
   --  setUseBuffer  --
   --------------------
   overriding
   procedure setUseBuffer (conn : out PostgreSQL_Connection;
                           buffered : Boolean) is
   begin
      raise UNSUPPORTED_BY_PGSQL;
   end setUseBuffer;


   -----------------
   --  useBuffer  --
   -----------------
   overriding
   function useBuffer (conn : PostgreSQL_Connection) return Boolean is
   begin
      return False;
   end useBuffer;


   --------------------------------
   --  driverMessage (interface) --
   --------------------------------
   overriding
   function driverMessage (conn : PostgreSQL_Connection) return String
   is
      result : BND.ICS.chars_ptr := BND.PQerrorMessage (conn.handle);
   begin
      return BND.ICS.Value (result);
   end driverMessage;


   -----------------------
   --  driverMessage #2 --
   -----------------------
   function driverMessage (conn : PostgreSQL_Connection;
                           res : BND.PGresult_Access) return String
   is
      result : BND.ICS.chars_ptr := BND.PQresultErrorMessage (res);
   begin
      return BND.ICS.Value (result);
   end driverMessage;


   ------------------------------
   --  driverCode (interface)  --
   ------------------------------
   overriding
   function driverCode (conn : PostgreSQL_Connection) return Driver_Codes is
   begin
      if conn.cmd_sql_state = stateless or else
        conn.cmd_sql_state = "00000"
      then
         return 0;
      end if;
      if conn.cmd_sql_state (1 .. 2) = "01" then
         return 1;
      end if;
      return 2;
   end driverCode;


   ---------------------
   --  driverCode #2  --
   ---------------------
   function driverCode (conn : PostgreSQL_Connection;
                        res  : BND.PGresult_Access) return Driver_Codes
   is
      SS : constant SQL_State := conn.SqlState (res);
   begin
      if SS = stateless or else SS = "00000" then
         return 0;
      end if;
      if SS (1 .. 2) = "01" then
         return 1;
      end if;
      return 2;
   end driverCode;


   ----------------------------
   --  SqlState (interface)  --
   ----------------------------
   overriding
   function SqlState (conn : PostgreSQL_Connection) return SQL_State is
   begin
      return conn.cmd_sql_state;
   end SqlState;


   -------------------
   --  SqlState #2  --
   -------------------
   function SqlState (conn : PostgreSQL_Connection; res : BND.PGresult_Access)
                      return SQL_State
   is
      use type BND.ICS.chars_ptr;
      fieldcode : constant BND.IC.int := BND.PG_DIAG_SQLSTATE;
      detail    : BND.ICS.chars_ptr;
   begin
      detail := BND.PQresultErrorField (res, fieldcode);
      if detail = BND.ICS.Null_Ptr then
         return stateless;
      end if;
      declare
         SS : String := BND.ICS.Value (detail);
      begin
         return SQL_State (SS);
      end;
   end SqlState;


   -------------------
   --  description  --
   -------------------
   overriding
   function description (conn : PostgreSQL_Connection) return String
   is
   begin
      return conn.info_description;
   end description;


   -------------------------
   --  helper_get_row_id  --
   -------------------------
   function returned_id (conn : PostgreSQL_Connection;
                         res  : BND.PGresult_Access) return Trax_ID
   is
   begin
      if conn.field_is_null (res, 0, 0) then
         return 0;
      end if;

      declare
         field : constant String := conn.field_string (res, 0, 0);
      begin
         return Trax_ID (Integer'Value (field));
      exception
         when others => return 0;
      end;
   end returned_id;


   -----------------------
   --  private_execute  --
   -----------------------
   procedure private_execute (conn : out PostgreSQL_Connection; sql : String)
   is
      use type BND.ExecStatusType;
      pgres   : BND.PGresult_Access;
      query   : BND.ICS.chars_ptr := BND.ICS.New_String (Str => sql);
      success : Boolean;
      msg     : CT.Text;
      ins_cmd : Boolean := False;
   begin
      if sql'Length > 12 and then
        ACH.To_Upper (sql (sql'First .. sql'First + 6)) = "INSERT "
      then
         ins_cmd := True;
      end if;

      pgres := BND.PQexec (conn => conn.handle, command => query);

      BND.ICS.Free (query);
      case conn.examine_result (pgres) is
         when executed =>
            success := True;
            conn.cmd_insert_return := False;
         when returned_data =>
            success := True;
            conn.cmd_insert_return := ins_cmd;
         when failed =>
            success := False;
            msg := CT.SUS (conn.driverMessage (pgres));
      end case;
      conn.cmd_sql_state := conn.SqlState (pgres);

      if success then
         conn.cmd_rows_impact := conn.rows_impacted (pgres);
      else
         conn.cmd_rows_impact := 0;
      end if;

      if conn.cmd_insert_return then
         conn.insert_return_val := conn.returned_id (pgres);
      else
         conn.insert_return_val := 0;
      end if;

      BND.PQclear (pgres);

      if not success then
         raise QUERY_FAIL with CT.USS (msg);
      end if;

   end private_execute;


   ----------------------
   --  private_select  --
   ----------------------
   function private_select (conn : PostgreSQL_Connection; sql : String)
                            return BND.PGresult_Access
   is
      use type BND.ExecStatusType;
      pgres   : BND.PGresult_Access;
      query   : BND.ICS.chars_ptr := BND.ICS.New_String (Str => sql);
      selcmd  : Boolean := True;
      success : Boolean;
      msg     : CT.Text;
   begin
      pgres := BND.PQexec (conn => conn.handle, command => query);

      BND.ICS.Free (query);

      case conn.examine_result (pgres) is
         when executed =>
            success := False;
            selcmd := False;
         when returned_data =>
            success := True;
         when failed =>
            success := False;
            msg := CT.SUS (conn.driverMessage (pgres));
      end case;

      if not success then
         if selcmd then
            raise QUERY_FAIL with CT.USS (msg);
         else
            raise QUERY_FAIL with "Not a SELECT query: " & sql;
         end if;
      end if;

      return pgres;
   end private_select;


   ----------------------------------------------
   --  rows_affected_by_execution (interface)  --
   ----------------------------------------------
   overriding
   function rows_affected_by_execution (conn : PostgreSQL_Connection)
                                        return Affected_Rows is
   begin
      return conn.cmd_rows_impact;
   end rows_affected_by_execution;


   ----------------------
   --  rows_in_result  --
   ----------------------
   function rows_in_result (conn : PostgreSQL_Connection;
                            res  : BND.PGresult_Access)
                            return Affected_Rows
   is
      use type BND.IC.int;
      result : BND.IC.int := BND.PQntuples (res);
   begin
      if result < 0 then
         --  overflowed (e.g. > 2 ** 31 on 32-bit system)
         return Affected_Rows'Last;
      end if;
      return Affected_Rows (result);
   end rows_in_result;


   ---------------------
   --  rows_impacted  --
   ---------------------
   function rows_impacted (conn : PostgreSQL_Connection;
                           res  : BND.PGresult_Access)
                           return Affected_Rows
   is
      result  : BND.ICS.chars_ptr := BND.PQcmdTuples (res);
      resstr  : constant String := BND.ICS.Value (result);
   begin
      if CT.IsBlank (resstr) then
         return 0;
      end if;
      begin
         return Affected_Rows (Integer'Value (resstr));
      exception
         when others => return 0;
      end;
   end rows_impacted;


   -------------------------
   --  begin_transaction  --
   -------------------------
   procedure begin_transaction (conn : out PostgreSQL_Connection) is
   begin
      conn.private_execute ("BEGIN");
      conn.dummy := True;
   exception
      when E : QUERY_FAIL =>
         raise TRAX_BEGIN_FAIL with EX.Exception_Message (E);
   end begin_transaction;


   --------------
   --  commit  --
   --------------
   overriding
   procedure commit (conn : out PostgreSQL_Connection)
   is
      procedure deallocate_prep_statement (Position : stmt_vector.Cursor);
      procedure deallocate_prep_statement (Position : stmt_vector.Cursor)
      is
         identifier : constant Trax_ID := stmt_vector.Element (Position);
         stmt_name  : constant String := "AdaBase_" & CT.trim (identifier'Img);
      begin
         if conn.destroy_statement (stmt_name) then
            null;
         end if;
      end deallocate_prep_statement;
   begin
      begin
         conn.private_execute ("COMMIT");
         conn.stmts_to_destroy.Iterate (deallocate_prep_statement'Access);
         conn.stmts_to_destroy.Clear;
      exception
         when E : QUERY_FAIL =>
            raise COMMIT_FAIL with EX.Exception_Message (E);
      end;
      if not conn.autoCommit then
         conn.begin_transaction;
      end if;
   end commit;


   ----------------
   --  rollback  --
   ----------------
   overriding
   procedure rollback (conn : out PostgreSQL_Connection)
   is
      procedure deallocate_prep_statement (Position : stmt_vector.Cursor);
      procedure deallocate_prep_statement (Position : stmt_vector.Cursor)
      is
         identifier : constant Trax_ID := stmt_vector.Element (Position);
         stmt_name  : constant String := "AdaBase_" & CT.trim (identifier'Img);
      begin
         if conn.destroy_statement (stmt_name) then
            null;
         end if;
      end deallocate_prep_statement;
   begin
      begin
         conn.private_execute ("ROLLBACK");
         conn.stmts_to_destroy.Iterate (deallocate_prep_statement'Access);
         conn.stmts_to_destroy.Clear;
      exception
         when E : QUERY_FAIL =>
            raise ROLLBACK_FAIL with EX.Exception_Message (E);
      end;
      if not conn.autoCommit then
         conn.begin_transaction;
      end if;
   end rollback;


   ---------------------
   --  setAutoCommit  --
   ---------------------
   overriding
   procedure setAutoCommit (conn : out PostgreSQL_Connection; auto : Boolean)
   is
      --  PGSQL server has no setting to disable autocommit.  Only issuing
      --  a BEGIN transaction command will inhibit autocommit (and commit/
      --  rollback enables it again).  Thus autocommit has to be handled at
      --  the adabase level.   A "BEGIN" command is issued immediately after
      --  connection, COMMIT and ROLLBACK to ensure we're always in a
      --  transaction when autocommit is off.
      previous_state : Boolean := conn.prop_auto_commit;
   begin
      conn.prop_auto_commit := auto;

      if conn.prop_active then
         if auto /= previous_state then
            if conn.within_transaction then
               if auto then
                  conn.commit;
               end if;
            else
               if not auto then
                  conn.begin_transaction;
               end if;
            end if;
         end if;
      end if;
   end setAutoCommit;


   ------------------
   --  disconnect  --
   ------------------
   overriding
   procedure disconnect (conn : out PostgreSQL_Connection)
   is
      use type BND.PGconn_Access;
   begin
      if conn.handle /= null then
         BND.PQfinish (conn => conn.handle);
         conn.handle := null;
      end if;
      conn.tables.Clear;
      conn.data_types.Clear;
      conn.prop_active := False;
   end disconnect;


   --------------------
   --  fields_count  --
   --------------------
   function fields_count (conn : PostgreSQL_Connection;
                          res  : BND.PGresult_Access) return Natural
   is
      result : BND.IC.int := BND.PQnfields (res);
   begin
      return Natural (result);
   end fields_count;


   ---------------------
   --  field_is_null  --
   ---------------------
   function field_is_null  (conn : PostgreSQL_Connection;
                            res  : BND.PGresult_Access;
                            row_number    : Natural;
                            column_number : Natural) return Boolean
   is
      use type BND.IC.int;
      rownum : constant BND.IC.int := BND.IC.int (row_number);
      colnum : constant BND.IC.int := BND.IC.int (column_number);
      result : constant BND.IC.int := BND.PQgetisnull (res, rownum, colnum);
   begin
      return (result = 1);
   end field_is_null;


   --------------------
   --  field_length  --
   --------------------
   function field_length (conn : PostgreSQL_Connection;
                          res  : BND.PGresult_Access;
                          row_number    : Natural;
                          column_number : Natural) return Natural
   is
      rownum : constant BND.IC.int := BND.IC.int (row_number);
      colnum : constant BND.IC.int := BND.IC.int (column_number);
      result : constant BND.IC.int := BND.PQgetlength (res, rownum, colnum);
   begin
      return Natural (result);
   end field_length;


   ------------------------
   --  discard_pgresult  --
   ------------------------
   procedure discard_pgresult (conn : PostgreSQL_Connection;
                               res  : out BND.PGresult_Access)
   is
      use type BND.PGresult_Access;
   begin
      if res /= null then
         BND.PQclear (res);
      end if;
      res := null;
   end discard_pgresult;


   ----------------------------
   --  field_data_is_binary  --
   ----------------------------
   function field_data_is_binary (conn : PostgreSQL_Connection;
                                  res  : BND.PGresult_Access;
                                  column_number : Natural) return Boolean
   is
      use type BND.IC.int;
      colnum : constant BND.IC.int := BND.IC.int (column_number);
      result : constant BND.IC.int := BND.PQfformat (res, colnum);
   begin
      return (result = 1);
   end field_data_is_binary;


   ----------------
   --  finalize  --
   ----------------
   overriding
   procedure finalize (conn : in out PostgreSQL_Connection) is
   begin
      conn.disconnect;
   end finalize;


   ---------------------
   --  setMultiQuery  --
   ---------------------
   overriding
   procedure setMultiQuery (conn     : out PostgreSQL_Connection;
                            multiple : Boolean)
   is
      --  Applicable only to driver.execute and implemented manually there
      --  (in order to use parameter execute rather than pgexec function
   begin
      conn.prop_multiquery := multiple;
   end setMultiQuery;


   ------------------
   --  multiquery  --
   ------------------
   overriding
   function multiquery (conn : PostgreSQL_Connection) return Boolean is
   begin
      return conn.prop_multiquery;
   end multiquery;


   -------------------------------
   --  setTransactionIsolation  --
   -------------------------------
   overriding
   procedure setTransactionIsolation (conn : out PostgreSQL_Connection;
                                      isolation : Trax_Isolation)
   is
      use type Trax_Isolation;
      sql : constant String :=
        "SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL " &
        ISO_Keywords (isolation);
   begin
      if conn.prop_active then
         conn.private_execute (sql);
      end if;

      conn.prop_trax_isolation := isolation;
   exception
      when QUERY_FAIL =>
         raise TRAXISOL_FAIL with sql;
   end setTransactionIsolation;


   ------------------------------------
   --  connection_attempt_succeeded  --
   ------------------------------------
   function connection_attempt_succeeded (conn : PostgreSQL_Connection)
                                          return Boolean
   is
      use type BND.ConnStatusType;
      status : constant BND.ConnStatusType := BND.PQstatus (conn.handle);
   begin
      return (status = BND.CONNECTION_OK);
   end connection_attempt_succeeded;


   -----------------------
   --  convert_version  --
   -----------------------
   function convert_version (pgsql_version : Natural) return CT.Text
   is
      six : String (1 .. 6) := (others => '0');
      raw : constant String := CT.int2str (pgsql_version);
      len : constant Natural := raw'Length;
   begin
      six (7 - len .. 6) := raw;
      if six (1) = '0' then
         return CT.SUS (six (2) & '.' & six (3 .. 4) & '.' & six (5 .. 6));
      else
         return CT.SUS
           (six (1 .. 2) & '.' & six (3 .. 4) & '.' & six (5 .. 6));
      end if;
   end convert_version;


   --------------------------
   --  get_server_version  --
   --------------------------
   function get_server_version (conn : PostgreSQL_Connection) return Natural
   is
      use type BND.IC.int;
      version : BND.IC.int := BND.PQserverVersion (conn.handle);
   begin
      return Natural (version);
   end get_server_version;


   ---------------------------
   --  get_library_version  --
   ---------------------------
   function get_library_version return Natural
   is
      use type BND.IC.int;
      version : BND.IC.int := BND.PQlibVersion;
   begin
      return Natural (version);
   end get_library_version;


   -----------------------
   --  get_server_info  --
   -----------------------
   function get_server_info (conn : PostgreSQL_Connection) return CT.Text
   is
      use type BND.IC.int;
      protocol : BND.IC.int := BND.PQprotocolVersion (conn.handle);
   begin
      return CT.SUS ("Protocol " & CT.int2str (Integer (protocol)) & ".0");
   end get_server_info;


   -----------------------
   --  is_ipv4_or_ipv6  --
   -----------------------
   function is_ipv4_or_ipv6 (teststr : String) return Boolean
   is
      function is_byte (segment : String) return Boolean;
      function is_byte (segment : String) return Boolean is
      begin
         if segment'Length > 3 then
            return False;
         end if;
         for x in segment'Range loop
            case segment (x) is
               when '0' .. '9' => null;
               when others => return False;
            end case;
         end loop;
         return (Integer'Value (segment) < 256);
      end is_byte;

      num_dots : constant Natural := CT.count_char (teststr, '.');
      dot      : constant String  := ".";
   begin
      if num_dots = 3 then
         declare
            P1A : String := CT.part_1 (teststr, dot);
            P1B : String := CT.part_2 (teststr, dot);
         begin
            if is_byte (P1A) then
               declare
                  P2A : String := CT.part_1 (P1B, dot);
                  P2B : String := CT.part_2 (P1B, dot);
               begin
                  if is_byte (P2A) then
                     declare
                        P3A : String := CT.part_1 (P2B, dot);
                        P3B : String := CT.part_2 (P2B, dot);
                     begin
                        if is_byte (P3A) and then is_byte (P3B) then
                           return True;
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end if;
      for x in teststr'Range loop
         case teststr (x) is
            when ':' | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' => null;
            when others => return False;
         end case;
      end loop;
      return True;
   end is_ipv4_or_ipv6;


   --------------------------
   --  within_transaction  --
   --------------------------
   function within_transaction (conn : PostgreSQL_Connection) return Boolean
   is
      use type BND.PGTransactionStatusType;
      status : BND.PGTransactionStatusType;
   begin
      status := BND.PQtransactionStatus (conn.handle);
      return (status /= BND.PQTRANS_IDLE);
   end within_transaction;


   ---------------
   --  connect  --
   ---------------
   overriding
   procedure connect (conn     : out PostgreSQL_Connection;
                      database : String;
                      username : String     := blankstring;
                      password : String     := blankstring;
                      hostname : String     := blankstring;
                      socket   : String     := blankstring;
                      port     : Posix_Port := portless)
   is
      constr : CT.Text := CT.SUS ("dbname=" & database);
   begin
      if conn.prop_active then
         raise NOT_WHILE_CONNECTED;
      end if;

      if not CT.IsBlank (username) then
         CT.SU.Append (constr, " user=" & username);
      end if;
      if not CT.IsBlank (password) then
         CT.SU.Append (constr, " password=" & password);
      end if;
      if not CT.IsBlank (hostname) then
         if is_ipv4_or_ipv6 (hostname) then
            CT.SU.Append (constr, " hostaddr=" & hostname);
         else
            CT.SU.Append (constr, " host=" & hostname);
         end if;
      else
         if not CT.IsBlank (socket) then
            CT.SU.Append (constr, " host=" & socket);
         end if;
      end if;
      if port /= portless then
         CT.SU.Append (constr, " port=" & CT.int2str (port));
      end if;

      declare
         use type BND.PGconn_Access;
         conninfo : BND.ICS.chars_ptr := BND.ICS.New_String (CT.USS (constr));
      begin
         conn.tables.Clear;
         conn.handle := BND.PQconnectdb (conninfo);
         BND.ICS.Free (conninfo);

         if not conn.connection_attempt_succeeded then
            raise CONNECT_FAILED;
         end if;
      end;

      conn.prop_active := True;
      conn.info_server_version := convert_version (conn.get_server_version);
      conn.info_server         := conn.get_server_info;

      conn.establish_uniform_encoding;
      conn.setTransactionIsolation (conn.prop_trax_isolation);
      if not conn.prop_auto_commit then
         conn.begin_transaction;
      end if;

      --  dump all tables and data types
      conn.cache_table_names;
      conn.cache_data_types;

   exception
      when NOT_WHILE_CONNECTED =>
         raise NOT_WHILE_CONNECTED with
           "Reconnection attempted during an active connection";
      when CONNECT_FAILED =>
         declare
            msg : String := "connection failure: " & conn.driverMessage;
         begin
            conn.disconnect;
            raise CONNECT_FAILED with msg;
         end;
      when rest : others =>
         conn.disconnect;
         EX.Reraise_Occurrence (rest);
   end connect;


   ------------------
   --  Initialize  --
   ------------------
   overriding
   procedure Initialize (conn : in out PostgreSQL_Connection) is
   begin
      conn.info_client_version := convert_version (get_library_version);
      conn.info_client := conn.info_client_version;
   end Initialize;


   ------------------
   --  field_name  --
   ------------------
   function field_name (conn : PostgreSQL_Connection;
                        res  : BND.PGresult_Access;
                        column_number : Natural) return String
   is
      colnum : constant BND.IC.int := BND.IC.int (column_number);
      result : BND.ICS.chars_ptr := BND.PQfname (res, colnum);
   begin
      return BND.ICS.Value (result);
   end field_name;


   --------------------
   --  field_string  --
   --------------------
   function field_string  (conn : PostgreSQL_Connection;
                           res  : BND.PGresult_Access;
                           row_number    : Natural;
                           column_number : Natural) return String
   is
      rownum : constant BND.IC.int := BND.IC.int (row_number);
      colnum : constant BND.IC.int := BND.IC.int (column_number);
      result : BND.ICS.chars_ptr := BND.PQgetvalue (res, rownum, colnum);
   begin
      return BND.ICS.Value (result);
   end field_string;


   --------------------
   --  lastInsertID  --
   --------------------
   overriding
   function lastInsertID (conn : PostgreSQL_Connection) return Trax_ID
   is
      --  PostgreSQL has a non-standard extension to INSERT INTO called
      --  RETURNING that is the most reliably method to get the last insert
      --  ID on the primary key.  We use it (determined in private_execute)
      --  if RETURNING was part of the INSERT query, otherwise we fall back
      --  to the less reliable lastval() method.
   begin
      if conn.cmd_insert_return then
         return conn.insert_return_val;
      else
         return conn.select_last_val;
      end if;
   end lastInsertID;


   -----------------------
   --  select_last_val  --
   -----------------------
   function select_last_val (conn : PostgreSQL_Connection) return Trax_ID
   is
      pgres   : BND.PGresult_Access;
      product : Trax_ID := 0;
   begin
      --  private_select can raise exception, but don't catch it
      --  For lastval(), exceptions should not be thrown so don't mask it
      pgres := conn.private_select ("SELECT lastval()");

      if conn.field_is_null (pgres, 0, 0) then
         BND.PQclear (pgres);
         return 0;
      end if;

      declare
         field : constant String := conn.field_string (pgres, 0, 0);
      begin
         product := Trax_ID (Integer'Value (field));
      exception
         when others => null;
      end;
      BND.PQclear (pgres);
      return product;
   end select_last_val;


   ---------------
   --  execute  --
   ---------------
   overriding
   procedure execute (conn : out PostgreSQL_Connection; sql : String) is
   begin
      conn.private_execute (sql => sql);
   end execute;


   -------------------------
   --  cache_table_names  --
   -------------------------
   procedure cache_table_names (conn : out PostgreSQL_Connection)
   is
      pgres : BND.PGresult_Access;
      nrows : Affected_Rows;
      sql   : constant String :=
                "SELECT oid, relname FROM pg_class " &
                "WHERE relkind = 'r' and relname !~ '^(pg|sql)_' " &
                "ORDER BY oid";
   begin
      pgres := conn.private_select (sql);
      nrows := conn.rows_in_result (pgres);
      for x in Natural range 0 .. Natural (nrows) - 1 loop
         declare
            s_oid   : constant String := conn.field_string (pgres, x, 0);
            s_table : constant String := conn.field_string (pgres, x, 1);
            payload : table_cell := (column_1 => CT.SUS (s_table));
         begin
            conn.tables.Insert (Key      => Integer'Value (s_oid),
                                New_Item => payload);
         end;
      end loop;
      BND.PQclear (pgres);
   end cache_table_names;


   -------------------
   --  field_table  --
   -------------------
   function field_table (conn : PostgreSQL_Connection;
                         res  : BND.PGresult_Access;
                         column_number : Natural) return String
   is
      use type BND.Oid;
      colnum : constant BND.IC.int := BND.IC.int (column_number);
      pg_oid : BND.Oid := BND.PQftable (res, colnum);
      pg_key : Integer := Integer (pg_oid);
   begin
      if pg_oid = BND.InvalidOid then
         return "INVALID COLUMN";
      end if;
      pg_key := Positive (pg_oid);
      if conn.tables.Contains (Key => pg_key) then
         return CT.USS (conn.tables.Element (pg_key).column_1);
      else
         return "INVALID OID" & pg_key'Img;
      end if;
   end field_table;


   ------------------
   --  field_type  --
   ------------------
   function field_type (conn : PostgreSQL_Connection;
                        res  : BND.PGresult_Access;
                        column_number : Natural) return field_types
   is
      colnum : constant BND.IC.int := BND.IC.int (column_number);
      pg_oid : BND.Oid := BND.PQftype (res, colnum);
      pg_key : Positive := Positive (pg_oid);
   begin
      if conn.data_types.Contains (Key => pg_key) then
         return conn.data_types.Element (pg_key).data_type;
      else
         --  Not in container, fall back to text tupe
         return ft_textual;
      end if;
   end field_type;


   -------------------------
   --  prepare_statement  --
   -------------------------
   function prepare_statement (conn : PostgreSQL_Connection;
                               stmt : aliased out BND.PGresult_Access;
                               name : String;
                               sql  : String) return Boolean
   is
      use type BND.ExecStatusType;
      c_stmt_name : BND.ICS.chars_ptr := BND.ICS.New_String (name);
      c_query     : BND.ICS.chars_ptr := BND.ICS.New_String (sql);
   begin

      stmt := BND.PQprepare (conn       => conn.handle,
                             stmtName   => c_stmt_name,
                             query      => c_query,
                             nParams    => 0,
                             paramTypes => null);
      BND.ICS.Free (c_stmt_name);
      BND.ICS.Free (c_query);
      return (BND.PQresultStatus (stmt) = BND.PGRES_COMMAND_OK);
   end prepare_statement;


   ------------------------
   --  prepare_metadata  --
   ------------------------
   function prepare_metadata  (conn : PostgreSQL_Connection;
                               meta : aliased out BND.PGresult_Access;
                               name : String) return Boolean
   is
      use type BND.ExecStatusType;
      c_stmt_name : BND.ICS.chars_ptr := BND.ICS.New_String (name);
   begin
      meta := BND.PQdescribePrepared (conn     => conn.handle,
                                      stmtName => c_stmt_name);
      BND.ICS.Free (c_stmt_name);
      return (BND.PQresultStatus (meta) = BND.PGRES_COMMAND_OK);
   end prepare_metadata;


   ----------------------
   --  examine_result  --
   ----------------------
   function examine_result (conn : PostgreSQL_Connection;
                            res  : BND.PGresult_Access) return postexec_status
   is
   begin
      case BND.PQresultStatus (res) is
         when BND.PGRES_COMMAND_OK =>
            return executed;
         when BND.PGRES_TUPLES_OK =>
            return returned_data;
         when others =>
            return failed;
      end case;
   end examine_result;


   ------------------------
   --  direst_stmt_exec  --
   ------------------------
   function direct_stmt_exec  (conn : out PostgreSQL_Connection;
                               stmt : aliased out BND.PGresult_Access;
                               sql  : String) return Boolean
   is
      use type BND.ExecStatusType;
      query   : BND.ICS.chars_ptr := BND.ICS.New_String (Str => sql);
      success : Boolean;
      msg     : CT.Text;
      ins_cmd : Boolean := False;
   begin
      if sql'Length > 12 and then
        ACH.To_Upper (sql (sql'First .. sql'First + 6)) = "INSERT "
      then
         ins_cmd := True;
      end if;

      stmt := BND.PQexec (conn => conn.handle, command => query);

      BND.ICS.Free (query);
      case conn.examine_result (stmt) is
         when executed =>
            success := True;
            conn.cmd_insert_return := False;
         when returned_data =>
            success := True;
            conn.cmd_insert_return := ins_cmd;
         when failed =>
            success := False;
            msg := CT.SUS (conn.driverMessage (stmt));
      end case;
      conn.insert_return_val := 0;
      conn.cmd_sql_state := conn.SqlState (stmt);

      if success then
         conn.cmd_rows_impact := conn.rows_impacted (stmt);
      else
         conn.cmd_rows_impact := 0;
      end if;

      if conn.cmd_insert_return then
         if not conn.field_is_null (stmt, 0, 0) then
            declare
               field : constant String := conn.field_string (stmt, 0, 0);
            begin
               conn.insert_return_val := Trax_ID (Integer'Value (field));
            exception
               when others => null;
            end;
         end if;
      end if;
      return success;
   end direct_stmt_exec;


   --------------------
   --  piped_tables  --
   --------------------
   function piped_tables (conn : PostgreSQL_Connection) return String
   is
      result : CT.Text := CT.blank;
      procedure add (position : table_map.Cursor);
      procedure add (position : table_map.Cursor) is
      begin
         if not CT.IsBlank (result) then
            CT.SU.Append (result, '|');
         end if;
         CT.SU.Append (result, table_map.Element (position).column_1);
      end add;
   begin
      conn.tables.Iterate (Process => add'Access);
      return CT.USS (result);
   end piped_tables;


   -------------------------
   --  refined_byte_type  --
   -------------------------
   function refined_byte_type (byteX : field_types; constraint : String)
                               return field_types
   is
      --  This routine is not used!
      --  by policy, byteX is ft_byte2, ft_byte3, ft_byte4 or ft_byte8

      subtype max_range is Positive range 1 .. 4;
      zero_required : constant String := "(VALUE >= 0)";
      max_size      : max_range;
   begin
      if CT.IsBlank (constraint) then
         return byteX;
      end if;
      if not CT.contains (S => constraint, fragment => zero_required) then
         return byteX;
      end if;

      case byteX is
         when ft_byte8 => max_size := 4;  -- NByte4
         when ft_byte4 => max_size := 3;  -- NByte3
         when ft_byte3 => max_size := 2;  -- NByte2
         when others   => max_size := 1;  -- NByte1;
      end case;

      for x in max_range loop
         declare
            bits   : constant Positive := x * 8;
            limit1 : constant Positive := 2 ** bits;
            limit2 : constant Positive := limit1 - 1;
            check1 : constant String := "(VALUE <" & limit1'Img & ")";
            check2 : constant String := "(VALUE <=" & limit2'Img & ")";
         begin
            if x <= max_size then
               if CT.contains (S => constraint, fragment => check1) or else
                 CT.contains (S => constraint, fragment => check2)
               then
                  case x is
                     when 1 => return ft_nbyte1;
                     when 2 => return ft_nbyte2;
                     when 3 => return ft_nbyte3;
                     when 4 => return ft_nbyte4;
                  end case;
               end if;
            end if;
         end;
      end loop;
      return byteX;
   end refined_byte_type;


   -------------------------
   --  convert_data_type  --
   -------------------------
   function convert_data_type (pg_type : String; category : Character;
                               typelen : Integer)
                               return field_types
   is
      --  Code Category (typcategory)
      --  A     Array types
      --  B     Boolean types
      --  C     Composite types
      --  D     Date/time types
      --  E     Enum types
      --  G     Geometric types
      --  I     Network address types
      --  N     Numeric types
      --  P     Pseudo-types
      --  S     String types
      --  T     Timespan types
      --  U     User-defined types
      --  V     Bit-string types
      --  X     unknown type

      desc : constant String := pg_type & " (" & category & ")";
   begin
      --  One User-defined type, bytea, is a chain.  Check for this one first
      --  and treat the reast as strings

      if pg_type = "bytea" then
         return ft_chain;
      end if;

      case category is
         when 'A' => return ft_textual;  --  No support for arrays yet
         when 'B' => return ft_nbyte0;
         when 'C' => return ft_textual;  --  No support for composites yet
         when 'D' => return ft_timestamp;
         when 'E' => return ft_enumtype;
         when 'G' => return ft_textual;  --  IMPLEMENT GEOMETRY LATER
         when 'I' => return ft_textual;
         when 'N' => null;               --  Let numerics fall through
         when 'S' => return ft_textual;
         when 'T' => return ft_textual;  --  Huge, 4/12/16 bytes
         when 'U' => return ft_textual;
         when 'V' => return ft_bits;     --  String of 1/0 for now

         when 'X' => raise METADATA_FAIL
                     with "Unknown type encountered: " & desc;
         when 'P' => raise METADATA_FAIL
                     with "Pseudo-type encountered: " & desc;
         when others => null;
      end case;

      --  Pick out standard float/double types from the remaining (numerics)

      if pg_type = "real" then
         return ft_real9;
      elsif pg_type = "float4" then
         return ft_real9;
      elsif pg_type = "float8" then
         return ft_real18;
      elsif pg_type = "money" then
         return ft_real18;
      elsif pg_type = "decimal" then
         return ft_real18;
      elsif pg_type = "numeric" then
         return ft_real18;
      elsif pg_type = "double precision" then
         return ft_real18;
      elsif typelen = -1 then
         return ft_real18;
      end if;

      if typelen = 1 then
         return ft_byte1;
      elsif typelen = 2 then
         return ft_byte2;
      elsif typelen = 3 then
         return ft_byte3;
      elsif typelen = 4 then
         return ft_byte4;
      elsif typelen = 8 then
         return ft_byte8;
      else
         raise METADATA_FAIL
           with "Unknown numeric type encountered: " & desc;
      end if;

   end convert_data_type;


   ------------------------
   --  cache_data_types  --
   ------------------------
   procedure cache_data_types  (conn : out PostgreSQL_Connection)
   is
      pgres  : BND.PGresult_Access;
      nrows  : Affected_Rows;
      tables : constant String := conn.piped_tables;
      sql    : constant String :=
               "SELECT DISTINCT a.atttypid,t.typname,t.typlen,t.typcategory " &
               "FROM pg_class c, pg_attribute a, pg_type t " &
               "WHERE c.relname ~ '^(" & tables & ")$' " &
               "AND a.attnum > 0 AND a.attrelid = c.oid " &
               "AND a.atttypid = t.oid " &
               "ORDER BY a.atttypid";
   begin
      pgres := conn.private_select (sql);
      nrows := conn.rows_in_result (pgres);
      for x in Natural range 0 .. Natural (nrows) - 1 loop
         declare
            s_oid   : constant String := conn.field_string (pgres, x, 0);
            s_name  : constant String := conn.field_string (pgres, x, 1);
            s_tlen  : constant String := conn.field_string (pgres, x, 2);
            s_cat   : constant String := conn.field_string (pgres, x, 3);
            s_cons  : constant String := "";
            typcat  : constant Character := s_cat (s_cat'First);
            typelen : constant Integer := Integer'Value (s_tlen);
            payload : data_type_rec := (data_type => convert_data_type
                                        (s_name, typcat, typelen));
         begin
            conn.data_types.Insert (Key      => Integer'Value (s_oid),
                                    New_Item => payload);
         end;
      end loop;
      BND.PQclear (pgres);
   end cache_data_types;


   --------------------
   --  field_binary  --
   --------------------
   function field_binary  (conn : PostgreSQL_Connection;
                           res  : BND.PGresult_Access;
                           row_number    : Natural;
                           column_number : Natural;
                           max_length    : Natural) return String
   is
      rownum : constant BND.IC.int := BND.IC.int (row_number);
      colnum : constant BND.IC.int := BND.IC.int (column_number);
      result : BND.ICS.chars_ptr := BND.PQgetvalue (res, rownum, colnum);
      len    : Natural := conn.field_length (res, row_number, column_number);
   begin
      declare
         bufmax : constant BND.IC.size_t := BND.IC.size_t (max_length);
         subtype data_buffer is BND.IC.char_array (1 .. bufmax);
         type db_access is access all data_buffer;
         buffer : aliased data_buffer;

         function db_convert (dba : db_access; size : Natural) return String;
         function db_convert (dba : db_access; size : Natural) return String
         is
            max : Natural := size;
         begin
            if max > max_length then
               max := max_length;
            end if;
            declare
               result : String (1 .. max);
            begin
               for x in result'Range loop
                  result (x) := Character (dba.all (BND.IC.size_t (x)));
               end loop;
               return result;
            end;
         end db_convert;
      begin
         return db_convert (buffer'Access, len);
      end;
   end field_binary;


   --------------------
   --  field_chain  --
   --------------------
   function field_chain  (conn : PostgreSQL_Connection;
                          res  : BND.PGresult_Access;
                          row_number    : Natural;
                          column_number : Natural;
                          max_length    : Natural) return String
   is
      --  raw expected in format "/x[hex-byte][hex-byte]...[hex-byte]"
      raw      : String := conn.field_string (res, row_number, column_number);
      maxlen   : Natural := raw'Length / 2;
      staged   : String (1 .. maxlen) := (others => '_');
      arrow    : Natural := raw'First;
      terminus : Natural := raw'Last;
      marker   : Natural := 0;
   begin
      if CT.len (raw) < 4 then
         return "";
      end if;

      arrow := arrow + 2;   --  skip past "/x"

      loop
         marker := marker + 1;
         if arrow + 1 > terminus then
            --  format error!  Odd length should never happen
            --  replace with zero and eject
               staged (marker) := Character'Val (0);
            exit;
         end if;
         declare
            hex : constant hexbyte := raw (arrow .. arrow + 1);
         begin
            staged (marker) := convert_hexbyte_to_char (hex);
            arrow := arrow + 2;
         end;
         exit when arrow > terminus;
         exit when marker = max_length;
      end loop;
      return staged (1 .. marker);
   end field_chain;


   ---------------------
   --  markers_found  --
   ---------------------
   function markers_found (conn : PostgreSQL_Connection;
                           res  : BND.PGresult_Access) return Natural
   is
      result : constant BND.IC.int := BND.PQnparams (res);
   begin
      return (Natural (result));
   end markers_found;


   -------------------------
   --  destroy_statement  --
   -------------------------
   function destroy_statement (conn : out PostgreSQL_Connection;
                               name : String) return Boolean
   is
      sql : constant String := "DEALLOCATE " & name;
   begin
      if conn.prop_active then
         conn.private_execute (sql);
      end if;

      return True;
   exception
      when QUERY_FAIL =>
         return False;
   end destroy_statement;


   --------------------------------
   --  execute_prepared_stmt #1  --
   --------------------------------
   function execute_prepared_stmt (conn : PostgreSQL_Connection;
                                   name : String;
                                   data : parameter_block)
                                   return BND.PGresult_Access
   is
      subtype param_range is Positive range 1 .. data'Length;

      nParams      : constant BND.IC.int := BND.IC.int (data'Length);
      resultFormat : constant BND.IC.int := 0;  --  specify text results
      stmtName     : BND.ICS.chars_ptr := BND.ICS.New_String (name);
      paramValues  : BND.Param_Val_Array (param_range);
      paramLengths : BND.Param_Int_Array (param_range);
      paramFormats : BND.Param_Int_Array (param_range);
      need_free    : array (param_range) of Boolean;
      pgres        : BND.PGresult_Access;
      datalen      : Natural;
   begin
      for x in paramLengths'Range loop
         datalen := CT.len (data (x).payload);
         paramLengths (x) := BND.IC.int (datalen);

         if data (x).binary then
            paramFormats (x) := BND.IC.int (1);
            if data (x).is_null then
               need_free (x) := False;
               paramValues (x).buffer := null;
            else
               need_free (x) := True;
               declare
                  Str : constant String := CT.USS (data (x).payload);
                  bsz : BND.IC.size_t := BND.IC.size_t (datalen);
               begin
                  paramValues (x).buffer := new BND.IC.char_array (1 .. bsz);
                  paramValues (x).buffer.all := BND.IC.To_C (Str, False);
               end;
            end if;
         else
            paramFormats (x) := BND.IC.int (0);
            if data (x).is_null then
               need_free (x) := False;
               paramValues (x).buffer := null;
            else
               declare
                  use type BND.IC.size_t;
                  Str : constant String := CT.USS (data (x).payload);
                  bsz : BND.IC.size_t := BND.IC.size_t (datalen) + 1;
               begin
                  paramValues (x).buffer := new BND.IC.char_array (1 .. bsz);
                  paramValues (x).buffer.all := BND.IC.To_C (Str, True);
               end;
            end if;
         end if;
      end loop;

      pgres := BND.PQexecPrepared
        (conn         => conn.handle,
         stmtName     => stmtName,
         nParams      => nParams,
         paramValues  => paramValues (1)'Unchecked_Access,
         paramLengths => paramLengths (1)'Unchecked_Access,
         paramFormats => paramFormats (1)'Unchecked_Access,
         resultFormat => resultFormat);

      BND.ICS.Free (stmtName);

      for x in need_free'Range loop
         if need_free (x) then
            free_binary (paramValues (x).buffer);
         end if;
      end loop;

      --  Let the caller check the state of pgres, just return it as is
      return pgres;
   end execute_prepared_stmt;


   --------------------------------
   --  execute_prepared_stmt #2  --
   --------------------------------
   function execute_prepared_stmt (conn : PostgreSQL_Connection;
                                   name : String) return BND.PGresult_Access
   is
      resultFormat : constant BND.IC.int := 0;  --  specify text results
      stmtName     : BND.ICS.chars_ptr := BND.ICS.New_String (name);
      pgres        : BND.PGresult_Access;
   begin
      pgres := BND.PQexecPrepared
        (conn         => conn.handle,
         stmtName     => stmtName,
         nParams      => 0,
         paramValues  => null,
         paramLengths => null,
         paramFormats => null,
         resultFormat => resultFormat);

      BND.ICS.Free (stmtName);
      --  Let the caller check the state of pgres, just return it as is
      return pgres;
   end execute_prepared_stmt;


   ---------------------
   --  destroy_later  --
   ---------------------
   procedure destroy_later (conn : out PostgreSQL_Connection;
                            identifier : Trax_ID) is
   begin
      conn.stmts_to_destroy.Append (New_Item => identifier);
   end destroy_later;


   -----------------------
   --  holds_refcursor  --
   ------------------------
   function holds_refcursor (conn : PostgreSQL_Connection;
                             res  : BND.PGresult_Access;
                             column_number : Natural) return Boolean
   is
      use type BND.Oid;
      colnum : constant BND.IC.int := BND.IC.int (column_number);
      pg_oid : BND.Oid := BND.PQftype (res, colnum);
   begin
      return (pg_oid = BND.PG_TYPE_refcursor);
   end holds_refcursor;


   -----------------------------
   --  convert_octet_to_char  --
   -----------------------------
   function convert_octet_to_char (before : octet) return Character
   is
      function digit (raw : Character) return Natural;

      --  This convert function does no error checking, it expects to receive
      --  valid octal numbers.  It will no throw an error if illegal
      --  characters are found, but rather it will return something value.

      function digit (raw : Character) return Natural is
      begin
         case raw is
            when '0' .. '7' => return Character'Pos (raw) - 48;
            when others     => return 0;
         end case;
      end digit;
   begin
      return Character'Val (digit (before (3)) +
                            digit (before (2)) * 8 +
                            digit (before (1)) * 64);
   end convert_octet_to_char;


   -------------------------------
   --  convert_hexbyte_to_char  --
   -------------------------------
   function convert_hexbyte_to_char (before : hexbyte) return Character
   is
      function digit (raw : Character) return Natural;

      --  This convert function does no error checking, it expects to receive
      --  valid octal numbers.  It will no throw an error if illegal
      --  characters are found, but rather it will return something value.

      function digit (raw : Character) return Natural is
      begin
         case raw is
            when '0' .. '9' => return Character'Pos (raw) -
                                      Character'Pos ('0');
            when 'A' .. 'F' => return Character'Pos (raw) + 10 -
                                      Character'Pos ('A');
            when 'a' .. 'f' => return Character'Pos (raw) + 10 -
                                      Character'Pos ('a');
            when others     => return 0;
         end case;
      end digit;
   begin
      return Character'Val (digit (before (2)) +
                            digit (before (1)) * 16);
   end convert_hexbyte_to_char;


   ----------------------------------
   --  establish_uniform_encoding  --
   ----------------------------------
   procedure establish_uniform_encoding (conn : out PostgreSQL_Connection)
   is
      sql : constant String := "SET CLIENT_ENCODING TO '" &
                               CT.USS (conn.character_set) & "'";
   begin
      if conn.prop_active then
         if not CT.IsBlank (conn.character_set) then
            execute (conn => conn, sql => sql);
         end if;
      end if;
   exception
      when QUERY_FAIL =>
         raise CHARSET_FAIL with sql;
   end establish_uniform_encoding;


   -------------------------
   --  set_character_set  --
   -------------------------
   overriding
   procedure set_character_set (conn : out PostgreSQL_Connection;
                                charset : String)
   is
   begin
      if conn.prop_active then
         raise NOT_WHILE_CONNECTED
           with "You may only alter the character set prior to connection";
      end if;
      conn.character_set := CT.SUS (charset);
   end set_character_set;


end AdaBase.Connection.Base.PostgreSQL;
