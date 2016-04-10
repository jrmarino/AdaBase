--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package AdaBase is

   pragma Pure;

   type ErrorMode      is (silent, warning, raise_exception);
   type CaseMode       is (lower_case, natural_case, upper_case);
   type TransIsolation is (read_uncommitted, read_committed, repeatable_read,
                           serializable);
   type LogCategory    is (connecting, disconnecting, transaction, execution,
                           statement_preparation, statement_execution,
                           miscellaneous, note);
   type TDriver        is (foundation, driver_mysql, driver_firebird,
                           driver_postgresql);
   type TIsoKeywords   is array (TransIsolation) of String (1 .. 16);
   type TraxID         is mod 2 ** 64;

   subtype BLOB_maximum  is Positive range 2 ** 12 .. 2 ** 30;
   subtype TSqlState     is String (1 .. 5);
   subtype DriverCodes   is Integer range -999 .. 1999;
   subtype PosixPort     is Natural range 0 .. 65535;
   subtype AffectedRows  is TraxID;

   IsoKeywords : constant TIsoKeywords :=
     ("READ UNCOMMITTED",
      "READ COMMITTED  ",
      "REPEATABLE READ ",
      "SERIALIZABLE    ");

   blankstring : constant String      := "";
   stateless   : constant TSqlState   := "     ";
   portless    : constant PosixPort   := 0;

   type field_types is (ft_nbyte0, ft_nbyte1, ft_nbyte2, ft_nbyte3, ft_nbyte4,
                        ft_nbyte8, ft_byte1, ft_byte2, ft_byte3, ft_byte4,
                        ft_byte8, ft_real9, ft_real18, ft_textual,
                        ft_widetext, ft_supertext, ft_timestamp,
                        ft_chain, ft_enumtype, ft_settype);
end AdaBase;
