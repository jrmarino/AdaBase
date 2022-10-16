--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body AdaBase.Driver.Base is


   ------------------------
   --  trait_autocommit  --
   ------------------------
   overriding
   function trait_autocommit (driver : Base_Driver) return Boolean is
   begin
      return driver.connection.autoCommit;
   end trait_autocommit;


   -------------------------
   --  trait_column_case  --
   -------------------------
   overriding
   function trait_column_case (driver : Base_Driver) return Case_Modes is
   begin
      return driver.connection.getCaseMode;
   end trait_column_case;


   ------------------------
   --  trait_error_mode  --
   ------------------------
   overriding
   function trait_error_mode (driver : Base_Driver) return Error_Modes is
   begin
      return logger.error_mode;
   end trait_error_mode;


   -----------------------
   --  trait_connected  --
   -----------------------
   overriding
   function trait_connected (driver : Base_Driver) return Boolean is
   begin
      return driver.connection.connected;
   end trait_connected;


   --------------------
   --  trait_driver  --
   --------------------
   overriding
   function trait_driver (driver : Base_Driver) return String is
   begin
      return driver.connection.description;
   end trait_driver;


   -------------------------
   --  trait_client_info  --
   -------------------------
   overriding
   function trait_client_info (driver : Base_Driver) return String is
   begin
      return driver.connection.clientInfo;
   end trait_client_info;


   ----------------------------
   --  trait_client_version  --
   ----------------------------
   overriding
   function trait_client_version (driver : Base_Driver) return String is
   begin
      return driver.connection.clientVersion;
   end trait_client_version;


   -------------------------
   --  trait_server_info  --
   -------------------------
   overriding
   function trait_server_info (driver : Base_Driver) return String is
   begin
      return driver.connection.serverInfo;
   end trait_server_info;


   ----------------------------
   --  trait_server_version  --
   ----------------------------
   overriding
   function trait_server_version (driver : Base_Driver) return String is
   begin
      return driver.connection.serverVersion;
   end trait_server_version;


   ---------------------------
   --  trait_max_blob_size  --
   ---------------------------
   overriding
   function trait_max_blob_size (driver : Base_Driver) return BLOB_Maximum is
   begin
      return driver.connection.maxBlobSize;
   end trait_max_blob_size;


   ---------------------------
   --  trait_character_set  --
   ---------------------------
   overriding
   function trait_character_set (driver : Base_Driver) return String is
   begin
      return driver.connection.character_set;
   end trait_character_set;


   ----------------------------
   --  set_trait_autocommit  --
   ----------------------------
   overriding
   procedure set_trait_autocommit (driver : Base_Driver; trait  : Boolean) is
   begin
      driver.connection.setAutoCommit (auto => trait);
   end set_trait_autocommit;


   -----------------------------
   --  set_trait_column_case  --
   -----------------------------
   overriding
   procedure set_trait_column_case (driver : Base_Driver; trait  : Case_Modes)
   is
   begin
      driver.connection.setCaseMode (mode => trait);
   end set_trait_column_case;


   ----------------------------
   --  set_trait_error_mode  --
   ----------------------------
   overriding
   procedure set_trait_error_mode  (driver : Base_Driver; trait  : Error_Modes)
   is
   begin
      logger.set_error_mode (mode => trait);
   end set_trait_error_mode;


   -------------------------------
   --  set_trait_max_blob_size  --
   -------------------------------
   overriding
   procedure set_trait_max_blob_size (driver : Base_Driver;
                                      trait  : BLOB_Maximum) is
   begin
      driver.connection.setMaxBlobSize (maxsize => trait);
   end set_trait_max_blob_size;


   ------------------------------------
   --  set_trait_multiquery_enabled  --
   ------------------------------------
   overriding
   procedure set_trait_multiquery_enabled (driver : Base_Driver;
                                           trait  : Boolean)
   is
   begin
      driver.connection.setMultiQuery (multiple => trait);
   end set_trait_multiquery_enabled;


   -------------------------------
   --  set_trait_character_set  --
   -------------------------------
   overriding
   procedure set_trait_character_set (driver : Base_Driver; trait : String) is
   begin
      driver.connection.set_character_set (charset => trait);
   end set_trait_character_set;


   --------------------------------
   --  trait_multiquery_enabled  --
   --------------------------------
   overriding
   function trait_multiquery_enabled (driver : Base_Driver) return Boolean is
   begin
      return driver.connection.multiquery;
   end trait_multiquery_enabled;


   -----------------------
   --  standard_logger  --
   -----------------------
   overriding
   procedure command_standard_logger (driver : Base_Driver;
                                      device : ALF.TLogger;
                                      action : ALF.TAction) is
   begin
      logger.standard_logger (logger => device, action => action);
   end command_standard_logger;


   ---------------------------
   --  set_logger_filename  --
   ---------------------------
   overriding
   procedure set_logger_filename (driver  : Base_Driver; filename : String) is
   begin
      logger.set_log_file (filename);
   end set_logger_filename;


   ----------------------------
   --  detach_custom_logger  --
   ----------------------------
   overriding
   procedure detach_custom_logger (driver : Base_Driver) is
   begin
      logger.detach_custom_logger;
   end detach_custom_logger;


   ----------------------------
   --  attach_custom_logger  --
   ----------------------------
   overriding
   procedure attach_custom_logger
     (driver        : Base_Driver;
      logger_access : ALF.AL.BaseClass_Logger_access) is
   begin
      logger.attach_custom_logger (logger_access => logger_access);
   end attach_custom_logger;


   -------------------------
   --  query_clear_table  --
   -------------------------
   overriding
   procedure query_clear_table (driver : Base_Driver; table  : String)
   is
      sql : constant String := "TRUNCATE " & table;
   begin
      pragma Warnings (Off, "*ARSILENT*");
      declare
         ARSILENT : Affected_Rows;
      begin
         ARSILENT := execute (driver => Base_Driver'Class (driver), sql => sql);
      end;
      pragma Warnings (On, "*ARSILENT*");
   end query_clear_table;


   ------------------------
   --  query_drop_table  --
   ------------------------
   overriding
   procedure query_drop_table (driver      : Base_Driver;
                               tables      : String;
                               when_exists : Boolean := False;
                               cascade     : Boolean := False)

   is
      --  MySQL accepts CASCADE but ignores it
      --  MySQL and PostgreSQL can use this versions, but Firebird
      --  needs if_exists implementation and doesn't know CASCADE, so it
      --  needs an overriding implementation.
      sql : CT.Text;
   begin
      if cascade and then driver.dialect = driver_mysql
      then
         driver.log_nominal (category => note, message =>
                        CT.SUS ("Requested CASCADE has no effect on MySQL"));
      end if;
      case when_exists is
         when True  => sql := CT.SUS ("DROP TABLE IF EXISTS " & tables);
         when False => sql := CT.SUS ("DROP TABLE " & tables);
      end case;
      if cascade then
         CT.SU.Append (Source => sql, New_Item => " CASCADE");
      end if;
      pragma Warnings (Off, "*ARSILENT*");
      declare
         ARSILENT : Affected_Rows;
      begin
         ARSILENT := execute (driver => Base_Driver'Class (driver),
                              sql    => CT.USS (sql));
      end;
      pragma Warnings (On, "*ARSILENT*");
   end query_drop_table;


   ------------------
   --  disconnect  --
   ------------------
   overriding
   procedure disconnect (driver : out Base_Driver)
   is
      msg : constant CT.Text :=
        CT.SUS ("Disconnect From " & CT.USS (driver.database) & "database");
      err : constant CT.Text :=
        CT.SUS ("ACK! Disconnect attempted on inactive connection");
   begin
      if driver.connection_active then
         driver.connection.disconnect;
         driver.connection_active := False;

         driver.log_nominal (category => disconnecting,
                             message  => msg);
      else
         --  Non-fatal attempt to disconnect db when none is connected
         driver.log_problem (category => disconnecting,
                             message  => err);
      end if;
   end disconnect;


   ----------------
   --  rollback  --
   ----------------
   overriding
   procedure rollback (driver : Base_Driver)
   is
      err1 : constant CT.Text :=
             CT.SUS ("ACK! Rollback attempted on inactive connection");
      err2 : constant CT.Text :=
             CT.SUS ("ACK! Rollback attempted when autocommit mode set on");
      err3 : constant CT.Text :=
             CT.SUS ("Rollback attempt failed");
      msg1 : constant CT.Text := CT.SUS ("ROLLBACK TRANSACTION");
   begin
      if not driver.connection_active then
         --  Non-fatal attempt to roll back when no database is connected
         driver.log_problem (category => miscellaneous,
                             message  => err1);
         return;
      end if;
      if driver.connection.autoCommit then
         --  Non-fatal attempt to roll back when autocommit is on
         driver.log_problem (category => miscellaneous,
                             message  => err2);
         return;
      end if;
      driver.connection.rollback;
      driver.log_nominal (category => transaction, message => msg1);
   exception
      when others =>
         driver.log_problem (category   => miscellaneous,
                             message    => err3,
                             pull_codes => True);
   end rollback;


   --------------
   --  commit  --
   --------------
   overriding
   procedure commit (driver : Base_Driver)
   is
      err1 : constant CT.Text :=
             CT.SUS ("ACK! Commit attempted on inactive connection");
      err2 : constant CT.Text :=
             CT.SUS ("ACK! Commit attempted when autocommit mode set on");
      err3 : constant CT.Text := CT.SUS ("Commit attempt failed");
      msg1 : constant CT.Text := CT.SUS ("END TRANSACTION (COMMIT)");
   begin
      if not driver.connection_active then
         --  Non-fatal attempt to commit when no database is connected
         driver.log_problem (category => transaction, message  => err1);
         return;
      end if;
      if driver.connection.autoCommit then
         --  Non-fatal attempt to commit when autocommit is on
         driver.log_problem (category => transaction, message  => err2);
         return;
      end if;
      driver.connection.commit;
      driver.log_nominal (category => transaction, message => msg1);
   exception
      when others =>
         driver.log_problem (category   => transaction,
                             message    => err3,
                             pull_codes => True);
   end commit;


   ------------------------
   --  last_driver_code  --
   ------------------------
   overriding
   function last_sql_state (driver : Base_Driver) return SQL_State is
   begin
      return driver.connection.SqlState;
   end last_sql_state;


   ------------------------
   --  last_driver_code  --
   ------------------------
   overriding
   function last_driver_code (driver : Base_Driver) return Driver_Codes is
   begin
      return driver.connection.driverCode;
   end last_driver_code;


   ---------------------------
   --  last_driver_message  --
   ---------------------------
   overriding
   function last_driver_message (driver : Base_Driver) return String is
   begin
      return driver.connection.driverMessage;
   end last_driver_message;


   ----------------------
   --  last_insert_id  --
   ----------------------
   overriding
   function last_insert_id (driver : Base_Driver) return Trax_ID is
   begin
      return driver.connection.lastInsertID;
   end last_insert_id;


   ------------------------------------------------------------------------
   --  PUBLIC ROUTINES NOT COVERED BY INTERFACES                         --
   ------------------------------------------------------------------------

   ------------------------
   --  basic_connect #1  --
   ------------------------
   overriding
   procedure basic_connect (driver   : out Base_Driver;
                            database : String;
                            username : String := blankstring;
                            password : String := blankstring;
                            socket   : String := blankstring)
   is
   begin
      private_connect (driver   => Base_Driver'Class (driver),
                       database => database,
                       username => username,
                       password => password,
                       socket   => socket);
   end basic_connect;


   ------------------------
   --  basic_connect #2  --
   ------------------------
   overriding
   procedure basic_connect (driver   : out Base_Driver;
                            database : String;
                            username : String := blankstring;
                            password : String := blankstring;
                            hostname : String := blankstring;
                            port     : Posix_Port)
   is
   begin
      private_connect (driver   => Base_Driver'Class (driver),
                       database => database,
                       username => username,
                       password => password,
                       hostname => hostname,
                       port     => port);
   end basic_connect;


   -----------------------------------------------------------------------
   --  PRIVATE ROUTINES NOT COVERED BY INTERFACES                        --
   ------------------------------------------------------------------------

   ------------------
   --  log_nominal --
   ------------------
   procedure log_nominal (driver    : Base_Driver;
                          category  : Log_Category;
                          message   : CT.Text)
   is
   begin
         logger.log_nominal (driver   => driver.dialect,
                             category => category,
                             message  => message);
   end log_nominal;


   ------------------
   --  log_problem --
   ------------------
   procedure log_problem
     (driver     : Base_Driver;
      category   : Log_Category;
      message    : CT.Text;
      pull_codes : Boolean := False;
      break      : Boolean := False)
   is
      error_msg  : CT.Text      := CT.blank;
      error_code : Driver_Codes := 0;
      sqlstate   : SQL_State    := stateless;
   begin
      if pull_codes then
         error_msg  := CT.SUS (driver.connection.driverMessage);
         error_code := driver.connection.driverCode;
         sqlstate   := driver.connection.SqlState;
      end if;

      logger.log_problem (driver     => driver.dialect,
                          category   => category,
                          message    => message,
                          error_msg  => error_msg,
                          error_code => error_code,
                          sqlstate   => sqlstate,
                          break      => break);
   end log_problem;


   ------------------------------
   --  assembly_common_select  --
   ------------------------------
   function assembly_common_select (distinct   : Boolean;
                                    tables     : String;
                                    columns    : String;
                                    conditions : String;
                                    groupby    : String;
                                    having     : String;
                                    order      : String) return String
   is
      function proc_distinct   (given : Boolean) return String;
      function proc_conditions (given : String) return String;
      function proc_groupby    (given : String) return String;
      function proc_having     (given : String) return String;
      function proc_order      (given : String) return String;

      function proc_distinct (given : Boolean) return String is
      begin
         if given then
            return "DISTINCT ";
         end if;
         return "ALL ";
      end proc_distinct;
      function proc_conditions (given : String) return String is
      begin
         if CT.IsBlank (given) then
            return blankstring;
         end if;
         return " WHERE " & given;
      end proc_conditions;
      function proc_groupby (given : String) return String is
      begin
         if CT.IsBlank (given) then
            return blankstring;
         end if;
         return " GROUP BY " & given;
      end proc_groupby;
      function proc_having (given : String) return String is
      begin
         if CT.IsBlank (given) then
            return blankstring;
         end if;
         return " HAVING " & given;
      end proc_having;
      function proc_order (given : String) return String is
      begin
         if CT.IsBlank (given) then
            return blankstring;
         end if;
         return " ORDER BY " & given;
      end proc_order;
   begin
      return "SELECT " & proc_distinct (distinct) & columns &
        " FROM " & tables &
        proc_conditions (conditions) &
        proc_groupby (groupby) &
        proc_having (having) &
        proc_order (order);
   end assembly_common_select;

end AdaBase.Driver.Base;
