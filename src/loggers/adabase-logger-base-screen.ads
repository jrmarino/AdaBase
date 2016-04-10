--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package AdaBase.Logger.Base.Screen is

   type Screen_Logger is new Base_Logger and AIL.iLogger with private;
   type Screen_Logger_access is access all Screen_Logger;

   overriding
   procedure reaction (listener : Screen_Logger);

private

   type Screen_Logger is new Base_Logger and AIL.iLogger
     with record
      null;
   end record;

end AdaBase.Logger.Base.Screen;
