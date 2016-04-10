--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

package AdaBase.Interfaces.Logger is

   type iLogger is interface;

   procedure reaction (listener : iLogger) is null;

end AdaBase.Interfaces.Logger;
