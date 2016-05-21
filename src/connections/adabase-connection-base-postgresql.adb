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
      case BND.PQresultStatus (pgres) is
         when BND.PGRES_COMMAND_OK =>
            success := True;
            conn.cmd_insert_return := False;
         when BND.PGRES_TUPLES_OK =>
            success := True;
            conn.cmd_insert_return := ins_cmd;
         when others =>
            success := False;
            msg := CT.SUS (conn.driverMessage (pgres));
      end case;
      conn.insert_return_val := 0;
      conn.cmd_sql_state := conn.SqlState (pgres);

      if success then
         conn.cmd_rows_impact := conn.rows_impacted (pgres);
      else
         conn.cmd_rows_impact := 0;
      end if;

      if conn.cmd_insert_return then
         if not conn.field_is_null (pgres, 0, 0) then
            declare
               field : constant String := conn.field_string (pgres, 0, 0);
            begin
               conn.insert_return_val := Trax_ID (Integer'Value (field));
            exception
               when others => null;
            end;
         end if;
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
      case BND.PQresultStatus (pgres) is
         when BND.PGRES_TUPLES_OK =>
            success := True;
         when BND.PGRES_COMMAND_OK =>
            success := False;
            selcmd := False;
         when others =>
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
   procedure commit (conn : out PostgreSQL_Connection) is
   begin
      begin
         conn.private_execute ("COMMIT");
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
   procedure rollback (conn : out PostgreSQL_Connection) is
   begin
      begin
         conn.private_execute ("ROLLBACK");
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
                               res  : BND.PGresult_Access) is
   begin
      BND.PQclear (res);
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

      --  not yet implemented  conn.set_character_set;
      conn.setTransactionIsolation (conn.prop_trax_isolation);
      if not conn.prop_auto_commit then
         conn.begin_transaction;
      end if;

      --  dump all tables and data types
      conn.cache_table_names;

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
     conn.private_execute (sql);
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
      pg_key : Positive := Positive (pg_oid);
   begin
      if pg_oid = BND.InvalidOid then
         return "INVALID COLUMN";
      end if;
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
      if pg_key > 0 then
         null;
      end if;
      return ft_nbyte0;
   end field_type;


end AdaBase.Connection.Base.PostgreSQL;