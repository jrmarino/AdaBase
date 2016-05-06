---  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Calendar.Formatting;

package body AdaBase.Logger.Base is

   package ACF renames Ada.Calendar.Formatting;


   -----------------------
   --  set_information  --
   -----------------------
   procedure set_information
               (listener   : out Base_Logger;
                category   : LogCategory;
                driver     : TDriver;
                message    : CT.Text;
                error_msg  : CT.Text     := CT.blank;
                error_code : DriverCodes := 0;
                sqlstate   : TSqlState   := stateless)
   is
      prefix    : String (1 .. 17);
      drv       : String (1 .. 11);
      timestamp : constant AC.Time := AC.Clock;
      TS        : constant String  := ACF.Image (Date => timestamp);
      error     : CT.Text := CT.SUS (error_code'Img &  " : SQLSTATE[" &
                                     sqlstate & "] : ");
      err_label : CT.Text := CT.SUS (" : Driver code :");
      composite : CT.Text := CT.blank;
   begin
      listener.prop_timestamp  := timestamp;
      listener.prop_category   := category;
      listener.prop_driver     := driver;
      listener.prop_message    := message;
      listener.prop_error_msg  := error_msg;
      listener.prop_error_code := error_code;
      listener.prop_sqlstate   := sqlstate;
      listener.prop_is_error   := not CT.IsBlank (error_msg);

      case driver is
         when driver_mysql      => drv := "    mysql :";
         when driver_sqlite     => drv := "   sqlite :";
         when driver_postgresql => drv := "    pgsql :";
         when driver_firebird   => drv := " firebird :";
         when foundation        => drv := "     none :";
      end case;

      case category is
         when connecting            => prefix := "       Connect : ";
         when disconnecting         => prefix := "    Disconnect : ";
         when transaction           => prefix := "   Transaction : ";
         when execution             => prefix := "       Execute : ";
         when statement_preparation => prefix := "  Prepare Stmt : ";
         when statement_execution   => prefix := "  Execute Stmt : ";
         when miscellaneous         => prefix := " Miscellaneous : ";
         when note                  => prefix := "          Note : ";
      end case;

      composite := CT.SUS (TS & drv & prefix);

      CT.SU.Append (Source => composite, New_Item => message);
      if listener.prop_is_error then
         CT.SU.Append (Source => composite, New_Item => err_label);
         CT.SU.Append (Source => composite, New_Item => error);
         CT.SU.Append (Source => composite, New_Item => error_msg);
      end if;

      listener.prop_composite  := composite;
   end set_information;


   -----------------
   --  timestamp  --
   -----------------
   function timestamp  (listener : Base_Logger) return AC.Time
   is
   begin
      return listener.prop_timestamp;
   end timestamp;


   ----------------
   --  category  --
   ----------------
   function category   (listener : Base_Logger) return LogCategory
   is
   begin
      return listener.prop_category;
   end category;


   --------------
   --  driver  --
   --------------
   function driver (listener : Base_Logger) return TDriver
   is
   begin
      return listener.prop_driver;
   end driver;


   -----------------
   --  composite  --
   -----------------
   function composite (listener : Base_Logger) return CT.Text
   is
   begin
      return listener.prop_composite;
   end composite;


   ---------------
   --  message  --
   ---------------
   function message (listener : Base_Logger) return CT.Text
   is
   begin
      return listener.prop_message;
   end message;


   -----------------
   --  error_msg  --
   -----------------
   function error_msg  (listener : Base_Logger) return CT.Text
   is
   begin
      return listener.prop_error_msg;
   end error_msg;


   ------------------
   --  error_code  --
   ------------------
   function error_code (listener : Base_Logger) return DriverCodes
   is
   begin
      return listener.prop_error_code;
   end error_code;


   ----------------
   --  sqlstate  --
   ----------------
   function sqlstate (listener : Base_Logger) return TSqlState is
   begin
      return listener.prop_sqlstate;
   end sqlstate;


   ----------------
   --  is_error  --
   ----------------
   function is_error (listener : Base_Logger) return Boolean is
   begin
      return listener.prop_is_error;
   end is_error;


end AdaBase.Logger.Base;
