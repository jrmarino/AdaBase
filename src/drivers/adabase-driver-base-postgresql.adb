--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body AdaBase.Driver.Base.PostgreSQL is

   ------------------------
   --  last_driver_code  --
   ------------------------
   overriding
   function last_sql_state (driver : PostgreSQL_Driver) return TSqlState
   is
      --  Polled by driver.execute before result is cleared
   begin
      return driver.connection.SqlState;
   end last_sql_state;


   ------------------------
   --  last_driver_code  --
   ------------------------
   overriding
   function last_driver_code (driver : PostgreSQL_Driver) return DriverCodes
   is
   begin
      return driver.connection.driverCode;
   end last_driver_code;


   ---------------------------
   --  last_driver_message  --
   ---------------------------
   overriding
   function last_driver_message (driver : PostgreSQL_Driver) return String is
   begin
      return driver.connection.driverMessage;
   end last_driver_message;


   ----------------------
   --  last_insert_id  --
   ----------------------
   overriding
   function last_insert_id (driver : PostgreSQL_Driver) return TraxID
   is
   begin
      return driver.connection.lastInsertID;
   end last_insert_id;


   ------------------
   --  disconnect  --
   ------------------
   overriding
   procedure disconnect (driver : out PostgreSQL_Driver) is
   begin
      --  TO BE IMPLEMENTED
      null;
   end disconnect;


   --------------
   --  commit  --
   --------------
   overriding
   procedure commit (driver : PostgreSQL_Driver) is
   begin
      --  TO BE IMPLEMENTED
      null;
   end commit;


   ----------------
   --  rollback  --
   ----------------
   overriding
   procedure rollback (driver : PostgreSQL_Driver) is
   begin
      --  TO BE IMPLEMENTED
      null;
   end rollback;


   ------------------------
   --  query_drop_table  --
   ------------------------
   overriding
   procedure query_drop_table  (driver      : PostgreSQL_Driver;
                                tables      : String;
                                when_exists : Boolean := False;
                                cascade     : Boolean := False)
   is
   begin
      --  TO BE IMPLEMENTED
      null;
   end query_drop_table;


   -------------------------
   --  query_clear_table  --
   -------------------------
   overriding
   procedure query_clear_table (driver : PostgreSQL_Driver;
                                table  : String)
   is
   begin
      --  TO BE IMPLEMENTED
      null;
   end query_clear_table;


   ------------------------------------
   --  set_trait_multiquery_enabled  --
   ------------------------------------
   overriding
   procedure set_trait_multiquery_enabled  (driver : PostgreSQL_Driver;
                                            trait  : Boolean) is
   begin
      --  TO BE IMPLEMENTED
      null;
   end set_trait_multiquery_enabled;


--     -------------
--     --  query  --
--     -------------
--     function query (driver : PostgreSQL_Driver; sql : String)
--                     return SMT.PostgreSQL_statement is
--     begin
--        --  TO BE IMPLEMENTED
--        raise CON.UNSUPPORTED_BY_PGSQL;
--     end query;
--
--     function prepare (driver : PostgreSQL_Driver; sql : String)
--                       return SMT.PostgreSQL_statement is
--     begin
--        --  TO BE IMPLEMENTED
--        raise CON.UNSUPPORTED_BY_PGSQL;
--     end prepare;
--
--
--     --------------------
--     --  query_select  --
--     --------------------
--     function query_select   (driver     : PostgreSQL_Driver;
--                              distinct   : Boolean := False;
--                              tables     : String;
--                              columns    : String;
--                              conditions : String := blankstring;
--                              groupby    : String := blankstring;
--                              having     : String := blankstring;
--                              order      : String := blankstring;
--                              null_sort  : NullPriority := native;
--                              limit      : TraxID := 0;
--                              offset     : TraxID := 0)
--                              return SMT.PostgreSQL_statement is
--     begin
--        --  TO BE IMPLEMENTED
--        raise CON.UNSUPPORTED_BY_PGSQL;
--     end query_select;
--
--
--     ----------------------
--     --  prepare_select  --
--     ----------------------
--     function prepare_select (driver     : PostgreSQL_Driver;
--                              distinct   : Boolean := False;
--                              tables     : String;
--                              columns    : String;
--                              conditions : String := blankstring;
--                              groupby    : String := blankstring;
--                              having     : String := blankstring;
--                              order      : String := blankstring;
--                              null_sort  : NullPriority := native;
--                              limit      : TraxID := 0;
--                              offset     : TraxID := 0)
--                              return SMT.PostgreSQL_statement is
--     begin
--        --  TO BE IMPLEMENTED
--        raise CON.UNSUPPORTED_BY_PGSQL;
--     end prepare_select;


   ------------------
   --  initialize  --
   ------------------
   overriding
   procedure initialize (Object : in out PostgreSQL_Driver) is
   begin
      Object.connection       := Object.local_connection'Unchecked_Access;
      Object.dialect          := driver_postgresql;
   end initialize;


   ------------------------
   --  basic_connect #1  --
   ------------------------
   overriding
   procedure basic_connect (driver   : out PostgreSQL_Driver;
                            database : String;
                            username : String := blankstring;
                            password : String := blankstring;
                            socket   : String := blankstring) is
   begin
      driver.private_connect (database => database,
                              username => username,
                              password => password,
                              socket   => socket);
   end basic_connect;


   ------------------------
   --  basic_connect #2  --
   ------------------------
   overriding
   procedure basic_connect (driver   : out PostgreSQL_Driver;
                            database : String;
                            username : String := blankstring;
                            password : String := blankstring;
                            hostname : String := blankstring;
                            port     : PosixPort) is
   begin
      driver.private_connect (database => database,
                              username => username,
                              password => password,
                              hostname => hostname,
                              port     => port);
   end basic_connect;


   -----------------------
   --  private_connect  --
   -----------------------
   procedure private_connect (driver   : out PostgreSQL_Driver;
                              database : String;
                              username : String;
                              password : String;
                              hostname : String := blankstring;
                              socket   : String := blankstring;
                              port     : PosixPort := portless)
   is
      err1 : constant CT.Text :=
        CT.SUS ("ACK! Reconnection attempted on active connection");
      nom  : constant CT.Text :=
        CT.SUS ("Connection to " & database & " database succeeded.");
   begin
      if driver.connection_active then
         driver.log_problem (category => execution,
                             message  => err1);
         return;
      end if;
      driver.connection.connect (database => database,
                                 username => username,
                                 password => password,
                                 socket   => socket,
                                 hostname => hostname,
                                 port     => port);

      driver.connection_active := driver.connection.all.connected;

      driver.log_nominal (category => connecting, message => nom);
   exception
      when Error : others =>
         driver.log_problem
           (category => connecting,
            break    => True,
            message  => CT.SUS (CON.EX.Exception_Message (X => Error)));
   end private_connect;


   ------------------
   --  execute #1  --
   ------------------
   overriding
   function execute (driver : PostgreSQL_Driver; sql : String)
                     return AffectedRows is
   begin
      --  TO BE IMPLEMENTED
      return 0;
   end execute;

end AdaBase.Driver.Base.PostgreSQL;
