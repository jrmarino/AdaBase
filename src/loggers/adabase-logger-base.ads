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

with Ada.Calendar;
with AdaBase.DataTypes;
with AdaBase.Interfaces.Logger;

package AdaBase.Logger.Base is

   package AC  renames Ada.Calendar;
   package AD  renames AdaBase.DataTypes;
   package AIL renames AdaBase.Interfaces.Logger;

   type Base_Logger is abstract new Base_Pure and AIL.iLogger with private;
   type BaseClass_Logger_access is access all Base_Logger'Class;

   procedure set_information
               (listener   : out Base_Logger;
                category   : AD.LogCategory;
                driver     : AD.TDriver;
                message    : AD.textual;
                error_msg  : AD.textual      := AD.blank;
                error_code : AD.DriverCodes  := 0;
                sqlstate   : AD.TSqlState    := AD.stateless);

   function timestamp  (listener : Base_Logger) return AC.Time;
   function category   (listener : Base_Logger) return AD.LogCategory;
   function driver     (listener : Base_Logger) return AD.TDriver;
   function composite  (listener : Base_Logger) return AD.textual;
   function message    (listener : Base_Logger) return AD.textual;
   function error_msg  (listener : Base_Logger) return AD.textual;
   function error_code (listener : Base_Logger) return AD.DriverCodes;

private

   type Base_Logger is abstract new Base_Pure and AIL.iLogger with record
      prop_timestamp  : AC.Time;
      prop_category   : AD.LogCategory;
      prop_driver     : AD.TDriver;
      prop_composite  : AD.textual := AD.blank;
      prop_message    : AD.textual := AD.blank;
      prop_error_msg  : AD.textual := AD.blank;
      prop_error_code : AD.DriverCodes;
      prop_sqlstate   : AD.TSqlState;
   end record;

   function S (before : String) return AD.textual;

end AdaBase.Logger.Base;
