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

with Ada.Calendar.Formatting;

package body AdaBase.Logger.Base is

   package ACF renames Ada.Calendar.Formatting;

   ---------
   --  S  --
   ---------
   function S (before : String) return logtext
   is
   begin
      return SU.To_Unbounded_String (before);
   end S;


   -----------------------
   --  set_information  --
   -----------------------
   procedure set_information
               (listener   : out Base_Logger;
                category   : LogCategory;
                driver     : TDriver;
                message    : logtext;
                error_msg  : logtext     := blank;
                error_code : DriverCodes := 0;
                sqlstate   : TSqlState   := stateless)
   is
      use type logtext;
      prefix    : String (1 .. 17);
      drv       : String (1 .. 11);
      timestamp : constant AC.Time := AC.Clock;
      TS        : constant String  := ACF.Image (Date => timestamp);
      error     : logtext :=
                  S (error_code'Img &  " : SQLSTATE[" & sqlstate & "] : ");
      err_label : logtext := S (" : Driver code :");
      composite : logtext := SU.Null_Unbounded_String;
   begin
      listener.prop_timestamp  := timestamp;
      listener.prop_category   := category;
      listener.prop_driver     := driver;
      listener.prop_message    := message;
      listener.prop_error_msg  := error_msg;
      listener.prop_error_code := error_code;
      listener.prop_sqlstate   := sqlstate;

      case driver is
         when driver_mysql      => drv := "    mysql :";
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

      composite := S (TS & drv & prefix);

      SU.Append (Source => composite, New_Item => message);
      if error_msg /= SU.Null_Unbounded_String then
         SU.Append (Source => composite, New_Item => err_label);
         SU.Append (Source => composite, New_Item => error);
         SU.Append (Source => composite, New_Item => error_msg);
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
   function composite (listener : Base_Logger) return logtext
   is
   begin
      return listener.prop_composite;
   end composite;


   ---------------
   --  message  --
   ---------------
   function message (listener : Base_Logger) return logtext
   is
   begin
      return listener.prop_message;
   end message;


   -----------------
   --  error_msg  --
   -----------------
   function error_msg  (listener : Base_Logger) return logtext
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


end AdaBase.Logger.Base;
