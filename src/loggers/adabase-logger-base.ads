---  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Calendar;
with Ada.Strings.Unbounded;
with AdaBase.Interfaces.Logger;

package AdaBase.Logger.Base is

   package SU  renames Ada.Strings.Unbounded;
   package AC  renames Ada.Calendar;
   package AIL renames AdaBase.Interfaces.Logger;

   type Base_Logger is abstract new Base_Pure and AIL.iLogger with private;
   type BaseClass_Logger_access is access all Base_Logger'Class;
   subtype logtext is SU.Unbounded_String;

   blank : constant logtext := SU.Null_Unbounded_String;

   procedure set_information
               (listener   : out Base_Logger;
                category   : LogCategory;
                driver     : TDriver;
                message    : logtext;
                error_msg  : logtext      := blank;
                error_code : DriverCodes  := 0;
                sqlstate   : TSqlState    := stateless);

   function timestamp  (listener : Base_Logger) return AC.Time;
   function category   (listener : Base_Logger) return LogCategory;
   function driver     (listener : Base_Logger) return TDriver;
   function composite  (listener : Base_Logger) return logtext;
   function message    (listener : Base_Logger) return logtext;
   function error_msg  (listener : Base_Logger) return logtext;
   function error_code (listener : Base_Logger) return DriverCodes;

private

   type Base_Logger is abstract new Base_Pure and AIL.iLogger with record
      prop_timestamp  : AC.Time;
      prop_category   : LogCategory;
      prop_driver     : TDriver;
      prop_composite  : logtext := blank;
      prop_message    : logtext := blank;
      prop_error_msg  : logtext := blank;
      prop_error_code : DriverCodes;
      prop_sqlstate   : TSqlState;
   end record;

   function S (before : String) return logtext;

end AdaBase.Logger.Base;
