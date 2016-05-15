--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body AdaBase.Connection.Base.PostgreSQL is

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


   ---------------------
   --  driverMessage  --
   ---------------------
   overriding
   function driverMessage (conn : PostgreSQL_Connection) return String
   is
      result : BND.ICS.chars_ptr := BND.PQerrorMessage (conn.handle);
   begin
      return BND.ICS.Value (result);
   end driverMessage;


   ------------------
   --  driverCode  --
   ------------------
   overriding
   function driverCode (conn : PostgreSQL_Connection) return DriverCodes is
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


   ----------------------------
   --  SqlState (interface)  --
   ----------------------------
   overriding
   function SqlState (conn : PostgreSQL_Connection) return TSqlState is
   begin
      return conn.cmd_sql_state;
   end SqlState;


   -------------------
   --  SqlState #2  --
   -------------------
   function SqlState (conn : PostgreSQL_Connection; res : BND.PGresult_Access)
                      return TSqlState
   is
      fieldcode : constant BND.IC.int := BND.PG_DIAG_SQLSTATE;
      detail    : BND.ICS.chars_ptr;
   begin
      detail := BND.PQresultErrorField (res, fieldcode);
      return BND.ICS.Value (detail);
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
   begin
      pgres := BND.PQexec (conn => conn.handle, command => query);

      BND.ICS.Free (query);
      success := (BND.PQresultStatus (pgres) = BND.PGRES_COMMAND_OK);
      conn.cmd_sql_state := conn.SqlState (pgres);

      if success then
         conn.cmd_rows_impact := conn.rows_affected_by_execution (pgres);
      else
         conn.cmd_rows_impact := 0;
      end if;

      BND.PQclear (pgres);

      if not success then
         raise QUERY_FAIL;
      end if;

   end private_execute;


   ----------------------------------------------
   --  rows_affected_by_execution (interface)  --
   ----------------------------------------------
   overriding
   function rows_affected_by_execution (conn : PostgreSQL_Connection)
                                        return AffectedRows is
   begin
      return conn.cmd_rows_impact;
   end rows_affected_by_execution;


   -------------------------------------
   --  rows_affected_by_execution #2  --
   -------------------------------------
   function rows_affected_by_execution (conn : PostgreSQL_Connection;
                                        res  : BND.PGresult_Access)
                                        return AffectedRows
   is
      use type BND.IC.int;
      result : BND.IC.int := BND.PQntuples (res);
   begin
      if result < 0 then
         --  overflowed (> 2 ** 31)
         return AffectedRows'Last;
      end if;
      return AffectedRows (result);
   end rows_affected_by_execution;


   -------------------------
   --  begin_transaction  --
   -------------------------
   procedure begin_transaction (conn : out PostgreSQL_Connection) is
   begin
      conn.private_execute ("BEGIN");
      conn.dummy := True;
   exception
      when QUERY_FAIL => raise TRAX_BEGIN_FAIL;
   end begin_transaction;


   --------------
   --  commit  --
   --------------
   overriding
   procedure commit (conn : out PostgreSQL_Connection) is
   begin
      conn.private_execute ("COMMIT");
      conn.dummy := True;
   exception
      when QUERY_FAIL => raise COMMIT_FAIL;
   end commit;


   ----------------
   --  rollback  --
   ----------------
   overriding
   procedure rollback (conn : out PostgreSQL_Connection) is
   begin
      conn.private_execute ("ROLLBACK");
      conn.dummy := True;
   exception
      when QUERY_FAIL => raise ROLLBACK_FAIL;
   end rollback;


   ---------------------
   --  setAutoCommit  --
   ---------------------
   overriding
   procedure setAutoCommit (conn : out PostgreSQL_Connection; auto : Boolean)
   is
   begin
      if auto then
         conn.private_execute ("SET AUTOCOMMIT ON");
      else
         conn.private_execute ("SET AUTOCOMMIT OFF");
      end if;
      conn.dummy := True;
   exception
         when QUERY_FAIL => raise AUTOCOMMIT_FAIL;
   end setAutoCommit;


   ------------------
   --  disconnect  --
   ------------------
   overriding
   procedure disconnect (conn : out PostgreSQL_Connection) is
   begin
      BND.PQfinish (conn => conn.handle);
      conn.dummy := True;
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


end AdaBase.Connection.Base.PostgreSQL;
