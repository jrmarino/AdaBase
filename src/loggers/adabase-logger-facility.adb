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

package body AdaBase.Logger.Facility is


   ------------------
   --  error_mode  --
   ------------------
   function error_mode (facility : LogFacility) return ErrorMode
   is
   begin
      return facility.prop_error_mode;
   end error_mode;


   ----------------------
   --  set_error_mode  --
   ----------------------
   procedure set_error_mode (facility : out LogFacility; mode : ErrorMode)
   is
   begin
      facility.prop_error_mode := mode;
   end set_error_mode;


   ---------------------
   --  standard_logger  --
   ---------------------
   procedure standard_logger  (facility : out LogFacility;
                               logger   : TLogger;
                               action   : TAction)
   is
      use type ALF.File_Logger_access;
      use type ALS.Screen_Logger_access;
   begin
      case logger is
         when screen =>
            case action is
               when detach =>
                  if facility.listener_screen = null then
                     raise ALREADY_DETACHED;
                  else
                     facility.listener_screen := null;
                  end if;
               when attach =>
                  if facility.listener_screen = null then
                     facility.listener_screen := logger_screen'Access;
                  else
                     raise ALREADY_ATTACHED;
                  end if;
            end case;
         when file =>
            case action is
               when detach =>
                  if facility.listener_file = null then
                     raise ALREADY_DETACHED;
                  else
                     facility.listener_file := null;
                  end if;
               when attach =>
                  if facility.listener_file = null then
                     facility.listener_file := logger_file'Access;
                  else
                     raise ALREADY_ATTACHED;
                  end if;
            end case;
      end case;
   end standard_logger;


   ----------------------------
   --  detach_custom_logger  --
   ----------------------------
   procedure detach_custom_logger (facility : out LogFacility)
   is
      use type AL.BaseClass_Logger_access;
   begin
      if facility.listener_custom = null then
         raise ALREADY_DETACHED;
      else
         facility.listener_custom := null;
      end if;
   end detach_custom_logger;


   ----------------------------
   --  attach_custom_logger  --
   ----------------------------
   procedure attach_custom_logger (facility : out LogFacility;
                                   logger_access : AL.BaseClass_Logger_access)
   is
      use type AL.BaseClass_Logger_access;
   begin
      if facility.listener_custom = null then
         facility.listener_custom := logger_access;
      else
         raise ALREADY_ATTACHED;
      end if;
   end attach_custom_logger;


   -------------------
   --  log_nominal  --
   -------------------
   procedure log_nominal (facility  : LogFacility;
                          driver    : TDriver;
                          category  : LogCategory;
                          message   : textual)
   is
      use type AL.Screen.Screen_Logger_access;
      use type AL.File.File_Logger_access;
      use type AL.BaseClass_Logger_access;
   begin

      if facility.listener_screen /= null then
         facility.listener_screen.all.set_information
           (driver   => driver,
            category => category,
            message  => message);
         facility.listener_screen.all.reaction;
      end if;

      if facility.listener_file /= null then
         facility.listener_file.all.set_information
           (driver   => driver,
            category => category,
            message  => message);
         facility.listener_file.all.reaction;
      end if;

      if facility.listener_custom /= null then
         facility.listener_custom.all.set_information
           (driver   => driver,
            category => category,
            message  => message);
         facility.listener_custom.all.reaction;
      end if;

   end log_nominal;


   -------------------
   --  log_problem  --
   -------------------
   procedure log_problem
     (facility   : LogFacility;
      driver     : TDriver;
      category   : LogCategory;
      message    : textual;
      error_msg  : textual      := blank;
      error_code : DriverCodes  := 0;
      sqlstate   : TSqlState    := stateless;
      break      : Boolean      := False)
   is
      use type ErrorMode;
      use type AL.Screen.Screen_Logger_access;
      use type AL.File.File_Logger_access;
      use type AL.BaseClass_Logger_access;
      QND : constant String := SU.To_String (message);
   begin
      if not break and then facility.prop_error_mode = silent
      then
         return;
      end if;

      if facility.listener_screen /= null then
         facility.listener_screen.all.set_information
           (driver     => driver,
            category   => category,
            message    => message,
            error_msg  => error_msg,
            error_code => error_code,
            sqlstate   => sqlstate);
         facility.listener_screen.all.reaction;
      end if;

      if facility.listener_file /= null then
         facility.listener_file.all.set_information
           (driver     => driver,
            category   => category,
            message    => message,
            error_msg  => error_msg,
            error_code => error_code,
            sqlstate   => sqlstate);
         facility.listener_file.all.reaction;
      end if;

      if facility.listener_custom /= null then
         facility.listener_custom.all.set_information
           (driver     => driver,
            category   => category,
            message    => message,
            error_msg  => error_msg,
            error_code => error_code,
            sqlstate   => sqlstate);
         facility.listener_custom.all.reaction;
      end if;

      if break or else facility.prop_error_mode = raise_exception
      then
         raise ERRMODE_EXCEPTION with QND;
      end if;

   end log_problem;


end AdaBase.Logger.Facility;
