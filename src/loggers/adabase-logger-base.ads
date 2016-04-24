---  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with CommonText;
with Ada.Calendar;
with AdaBase.Interfaces.Logger;

package AdaBase.Logger.Base is

   package CT  renames CommonText;
   package AC  renames Ada.Calendar;
   package AIL renames AdaBase.Interfaces.Logger;

   type Base_Logger is abstract new Base_Pure and AIL.iLogger with private;
   type BaseClass_Logger_access is access all Base_Logger'Class;

   procedure set_information
               (listener   : out Base_Logger;
                category   : LogCategory;
                driver     : TDriver;
                message    : CT.Text;
                error_msg  : CT.Text      := CT.blank;
                error_code : DriverCodes  := 0;
                sqlstate   : TSqlState    := stateless);

   function timestamp  (listener : Base_Logger) return AC.Time;
   function category   (listener : Base_Logger) return LogCategory;
   function driver     (listener : Base_Logger) return TDriver;
   function composite  (listener : Base_Logger) return CT.Text;
   function message    (listener : Base_Logger) return CT.Text;
   function error_msg  (listener : Base_Logger) return CT.Text;
   function error_code (listener : Base_Logger) return DriverCodes;
   function sqlstate   (listener : Base_Logger) return TSqlState;
   function is_error   (listener : Base_Logger) return Boolean;

private

   type Base_Logger is abstract new Base_Pure and AIL.iLogger with record
      prop_timestamp  : AC.Time;
      prop_category   : LogCategory;
      prop_driver     : TDriver;
      prop_composite  : CT.Text := CT.blank;
      prop_message    : CT.Text := CT.blank;
      prop_error_msg  : CT.Text := CT.blank;
      prop_error_code : DriverCodes;
      prop_sqlstate   : TSqlState;
      prop_is_error   : Boolean;
   end record;

end AdaBase.Logger.Base;
