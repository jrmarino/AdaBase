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

with AdaBase.Logger.Base.File;
with AdaBase.Logger.Base.Screen;

package AdaBase.Logger.Facility is

   package AL  renames AdaBase.Logger.Base;
   package ALF renames AdaBase.Logger.Base.File;
   package ALS renames AdaBase.Logger.Base.Screen;

   type LogFacility is tagged private;
   type LogFacility_access is access all LogFacility;
   type TAction is (attach, detach);
   type TLogger is (file, screen);

   ALREADY_ATTACHED  : exception;
   ALREADY_DETACHED  : exception;
   ERRMODE_EXCEPTION : exception;

   procedure standard_logger (facility : out LogFacility;
                              logger   : TLogger;
                              action   : TAction);
   procedure set_error_mode (facility : out LogFacility; mode : ErrorMode);
   function  error_mode     (facility : LogFacility) return ErrorMode;

   procedure detach_custom_logger (facility : out LogFacility);
   procedure attach_custom_logger (facility : out LogFacility;
                                   logger_access : AL.BaseClass_Logger_access);

   procedure log_nominal (facility  : LogFacility;
                          driver    : TDriver;
                          category  : LogCategory;
                          message   : AL.logtext);

   procedure log_problem
     (facility   : LogFacility;
      driver     : TDriver;
      category   : LogCategory;
      message    : AL.logtext;
      error_msg  : AL.logtext  := AL.blank;
      error_code : DriverCodes := 0;
      sqlstate   : TSqlState   := stateless;
      break      : Boolean     := False);

private
   type LogFacility is tagged record
      prop_error_mode : ErrorMode := warning;
      listener_file   : ALF.File_Logger_access := null;
      listener_screen : ALS.Screen_Logger_access := null;
      listener_custom : AL.BaseClass_Logger_access := null;
   end record;

   logger_file     : aliased ALF.File_Logger;
   logger_screen   : aliased ALS.Screen_Logger;

end AdaBase.Logger.Facility;
