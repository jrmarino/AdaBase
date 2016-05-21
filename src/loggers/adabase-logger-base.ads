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
                category   : Log_Category;
                driver     : Driver_Type;
                message    : CT.Text;
                error_msg  : CT.Text      := CT.blank;
                error_code : Driver_Codes := 0;
                sqlstate   : SQL_State    := stateless);

   function timestamp  (listener : Base_Logger) return AC.Time;
   function category   (listener : Base_Logger) return Log_Category;
   function driver     (listener : Base_Logger) return Driver_Type;
   function composite  (listener : Base_Logger) return CT.Text;
   function message    (listener : Base_Logger) return CT.Text;
   function error_msg  (listener : Base_Logger) return CT.Text;
   function error_code (listener : Base_Logger) return Driver_Codes;
   function sqlstate   (listener : Base_Logger) return SQL_State;
   function is_error   (listener : Base_Logger) return Boolean;

private

   type Base_Logger is abstract new Base_Pure and AIL.iLogger with record
      prop_timestamp  : AC.Time;
      prop_category   : Log_Category;
      prop_driver     : Driver_Type;
      prop_composite  : CT.Text := CT.blank;
      prop_message    : CT.Text := CT.blank;
      prop_error_msg  : CT.Text := CT.blank;
      prop_error_code : Driver_Codes;
      prop_sqlstate   : SQL_State;
      prop_is_error   : Boolean;
   end record;

end AdaBase.Logger.Base;
