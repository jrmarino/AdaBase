--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package body AdaBase.Driver.Base is


   ------------------------
   --  trait_autocommit  --
   ------------------------
   overriding
   function trait_autocommit (driver : Base_Driver) return Boolean
   is
   begin
      return driver.connection.all.autoCommit;
   end trait_autocommit;


   -------------------------
   --  trait_column_case  --
   -------------------------
   overriding
   function trait_column_case (driver : Base_Driver) return CaseMode
   is
   begin
      return driver.connection.all.getCaseMode;
   end trait_column_case;


   ------------------------
   --  trait_error_mode  --
   ------------------------
   overriding
   function trait_error_mode (driver : Base_Driver) return ErrorMode
   is
   begin
      return logger.error_mode;
   end trait_error_mode;


   -----------------------
   --  trait_connected  --
   -----------------------
   overriding
   function trait_connected (driver : Base_Driver) return Boolean
   is
   begin
      return driver.connection.all.connected;
   end trait_connected;


   --------------------
   --  trait_driver  --
   --------------------
   overriding
   function trait_driver (driver : Base_Driver) return String
   is
   begin
      return driver.connection.all.description;
   end trait_driver;


   -------------------------
   --  trait_client_info  --
   -------------------------
   overriding
   function trait_client_info (driver : Base_Driver)
                               return String
   is
   begin
      return driver.connection.all.clientInfo;
   end trait_client_info;


   ----------------------------
   --  trait_client_version  --
   ----------------------------
   overriding
   function trait_client_version (driver : Base_Driver)
                                  return String
   is
   begin
      return driver.connection.all.clientVersion;
   end trait_client_version;


   -------------------------
   --  trait_server_info  --
   -------------------------
   overriding
   function trait_server_info (driver : Base_Driver)
                               return String
   is
   begin
      return driver.connection.all.serverInfo;
   end trait_server_info;


   ----------------------------
   --  trait_server_version  --
   ----------------------------
   overriding
   function trait_server_version (driver : Base_Driver)
                                  return String
   is
   begin
      return driver.connection.all.serverVersion;
   end trait_server_version;


   ---------------------------
   --  trait_max_blob_size  --
   ---------------------------
   overriding
   function trait_max_blob_size (driver : Base_Driver)
                                 return BLOB_maximum
   is
   begin
      return driver.connection.all.maxBlobSize;
   end trait_max_blob_size;


   ----------------------------
   --  set_trait_autocommit  --
   ----------------------------
   overriding
   procedure set_trait_autocommit  (driver : Base_Driver;
                                    trait  : Boolean)
   is
   begin
      driver.connection.all.setAutoCommit (auto => trait);
   end set_trait_autocommit;


   -----------------------------
   --  set_trait_column_case  --
   -----------------------------
   overriding
   procedure set_trait_column_case (driver : Base_Driver;
                                    trait  : CaseMode)
   is
   begin
      driver.connection.all.setCaseMode (mode => trait);
   end set_trait_column_case;


   ----------------------------
   --  set_trait_error_mode  --
   ----------------------------
   overriding
   procedure set_trait_error_mode  (driver : Base_Driver;
                                    trait  : ErrorMode)
   is
   begin
      logger.set_error_mode (mode => trait);
   end set_trait_error_mode;


   -------------------------------
   --  set_trait_max_blob_size  --
   -------------------------------
   overriding
   procedure set_trait_max_blob_size (driver : Base_Driver;
                                      trait  : BLOB_maximum)
   is
   begin
      driver.connection.all.setMaxBlobSize (maxsize => trait);
   end set_trait_max_blob_size;


   --------------------------------
   --  trait_multiquery_enabled  --
   --------------------------------
   overriding
   function trait_multiquery_enabled (driver : Base_Driver) return Boolean
   is
   begin
      return driver.connection.all.multiquery;
   end trait_multiquery_enabled;


   -----------------------
   --  standard_logger  --
   -----------------------
   overriding
   procedure command_standard_logger (driver : Base_Driver;
                                      device : ALF.TLogger;
                                      action : ALF.TAction)
   is
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
   procedure detach_custom_logger (driver : Base_Driver)
   is
   begin
      logger.detach_custom_logger;
   end detach_custom_logger;


   ----------------------------
   --  attach_custom_logger  --
   ----------------------------
   overriding
   procedure attach_custom_logger
     (driver        : Base_Driver;
      logger_access : ALF.AL.BaseClass_Logger_access)
   is
   begin
      logger.attach_custom_logger (logger_access => logger_access);
   end attach_custom_logger;


   -------------------------
   --  query_clear_table  --
   -------------------------
   overriding
   procedure query_clear_table (driver : Base_Driver;
                                table  : String)
   is
      sql : constant String := "TRUNCATE " & table;
      AR  : AffectedRows;
   begin
      AR := execute (driver => Base_Driver'Class (driver), sql => sql);
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
      use type TDriver;
      --  MySQL accepts CASCADE but ignores it
      --  MySQL and PostgreSQL can use this versions, but Firebird
      --  needs if_exists implementation and doesn't know CASCADE, so it
      --  needs an overriding implementation.
      sql : CT.Text;
      AR  : AffectedRows;
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
      AR := execute (driver => Base_Driver'Class (driver),
                     sql    => CT.USS (sql));
   end query_drop_table;


   -----------------------------------------------------------------------
   --  PRIVATE ROUTINES NOT COVERED BY INTERFACES                        --
   ------------------------------------------------------------------------

   ------------------
   --  log_nominal --
   ------------------
   procedure log_nominal (driver    : Base_Driver;
                          category  : LogCategory;
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
      category   : LogCategory;
      message    : CT.Text;
      pull_codes : Boolean := False;
      break      : Boolean := False)
   is
      error_msg  : CT.Text      := CT.blank;
      error_code : DriverCodes  := 0;
      sqlstate   : TSqlState    := stateless;
   begin
      if pull_codes then
         error_msg  := CT.SUS (driver.connection.all.driverMessage);
         error_code := driver.connection.all.driverCode;
         sqlstate   := driver.connection.all.SqlState;
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
