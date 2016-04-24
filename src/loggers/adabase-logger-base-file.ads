--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package AdaBase.Logger.Base.File is

   type File_Logger is new Base_Logger and AIL.iLogger with private;
   type File_Logger_access is access all File_Logger;

   overriding
   procedure reaction (listener : File_Logger);

   procedure set_filepath (listener : out File_Logger; filename : String);

private

   type File_Logger is new Base_Logger and AIL.iLogger
     with record
      filepath : CT.Text := CT.SUS ("/tmp/adabase.log");
   end record;

end AdaBase.Logger.Base.File;
