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
   function S (before : String) return AD.textual
   is
   begin
      return AD.SU.To_Unbounded_String (before);
   end S;


   -----------------------
   --  set_information  --
   -----------------------
   procedure set_information
               (listener   : out Base_Logger;
                category   : AD.LogCategory;
                driver     : AD.TDriver;
                message    : AD.textual;
                error_msg  : AD.textual     := AD.blank;
                error_code : AD.DriverCodes := 0;
                sqlstate   : AD.TSqlState   := AD.stateless)
   is
      use type AD.textual;
      prefix    : String (1 .. 17);
      drv       : String (1 .. 11);
      timestamp : constant AC.Time := AC.Clock;
      TS        : constant String  := ACF.Image (Date => timestamp);
      error     : AD.textual :=
                  S (error_code'Img &  " : SQLSTATE[" & sqlstate & "] : ");
      err_label : AD.textual := S (" : Driver code :");
      composite : AD.textual := AD.blank;
   begin
      listener.prop_timestamp  := timestamp;
      listener.prop_category   := category;
      listener.prop_driver     := driver;
      listener.prop_message    := message;
      listener.prop_error_msg  := error_msg;
      listener.prop_error_code := error_code;
      listener.prop_sqlstate   := sqlstate;

      case driver is
         when AD.mysql      => drv := "    mysql :";
         when AD.postgresql => drv := "    pgsql :";
         when AD.firebird   => drv := " firebird :";
         when AD.foundation => drv := "     none :";
      end case;

      case category is
         when AD.connection            => prefix := "       Connect : ";
         when AD.disconnection         => prefix := "    Disconnect : ";
         when AD.transaction           => prefix := "   Transaction : ";
         when AD.execution             => prefix := "       Execute : ";
         when AD.statement_preparation => prefix := "  Prepare Stmt : ";
         when AD.statement_execution   => prefix := "  Execute Stmt : ";
         when AD.miscellaneous         => prefix := " Miscellaneous : ";
         when AD.note                  => prefix := "          Note : ";
      end case;

      composite := S (TS & drv & prefix);

      AD.SU.Append (Source => composite, New_Item => message);
      if error_msg /= AD.blank then
         AD.SU.Append (Source => composite, New_Item => err_label);
         AD.SU.Append (Source => composite, New_Item => error);
         AD.SU.Append (Source => composite, New_Item => error_msg);
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
   function category   (listener : Base_Logger) return AD.LogCategory
   is
   begin
      return listener.prop_category;
   end category;


   --------------
   --  driver  --
   --------------
   function driver (listener : Base_Logger) return AD.TDriver
   is
   begin
      return listener.prop_driver;
   end driver;


   -----------------
   --  composite  --
   -----------------
   function composite (listener : Base_Logger) return AD.textual
   is
   begin
      return listener.prop_composite;
   end composite;


   ---------------
   --  message  --
   ---------------
   function message (listener : Base_Logger) return AD.textual
   is
   begin
      return listener.prop_message;
   end message;


   -----------------
   --  error_msg  --
   -----------------
   function error_msg  (listener : Base_Logger) return AD.textual
   is
   begin
      return listener.prop_error_msg;
   end error_msg;


   ------------------
   --  error_code  --
   ------------------
   function error_code (listener : Base_Logger) return AD.DriverCodes
   is
   begin
      return listener.prop_error_code;
   end error_code;


end AdaBase.Logger.Base;
