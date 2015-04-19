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

with AdaBase.DataTypes;
with AdaBase.Logger.Base.File;
with AdaBase.Logger.Base.Screen;

package AdaBase.Logger.Facility is

   package AD  renames AdaBase.DataTypes;
   package AL  renames AdaBase.Logger.Base;
   package ALF renames AdaBase.Logger.Base.File;
   package ALS renames AdaBase.Logger.Base.Screen;

   type LogFacility is tagged private;
   type TAction is (attach, detach);
   type TLogger is (file, screen);

   ALREADY_ATTACHED  : exception;
   ALREADY_DETACHED  : exception;
   ERRMODE_EXCEPTION : exception;

   procedure standard_logger (facility : out LogFacility;
                              logger   : TLogger;
                              action   : TAction);
   procedure set_error_mode (facility : out LogFacility; mode : AD.ErrorMode);
   function  error_mode     (facility : LogFacility) return AD.ErrorMode;

   procedure detach_custom_logger (facility : out LogFacility);
   procedure attach_custom_logger (facility : out LogFacility;
                                   logger_access : AL.BaseClass_Logger_access);

   procedure log_nominal (facility  : LogFacility;
                          driver    : AL.AD.TDriver;
                          category  : AL.AD.LogCategory;
                          message   : AL.AD.textual);

   procedure log_problem
     (facility   : LogFacility;
      driver     : AL.AD.TDriver;
      category   : AL.AD.LogCategory;
      message    : AL.AD.textual;
      error_msg  : AL.AD.textual     := AL.AD.blank;
      error_code : AL.AD.DriverCodes := 0;
      sqlstate   : AL.AD.TSqlState   := AD.stateless;
      break      : Boolean           := False);

private
   type LogFacility is tagged record
      prop_error_mode : AD.ErrorMode := AD.warning;
      listener_file   : ALF.File_Logger_access := null;
      listener_screen : ALS.Screen_Logger_access := null;
      listener_custom : AL.BaseClass_Logger_access := null;
   end record;

   logger_file     : aliased ALF.File_Logger;
   logger_screen   : aliased ALS.Screen_Logger;

end AdaBase.Logger.Facility;
