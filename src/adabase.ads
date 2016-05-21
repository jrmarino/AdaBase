--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package AdaBase is

   pragma Pure;

   type Error_Modes      is (silent, warning, raise_exception);
   type Case_Modes       is (lower_case, natural_case, upper_case);
   type Trax_Isolation   is (read_uncommitted, read_committed, repeatable_read,
                             serializable);
   type Log_Category     is (connecting, disconnecting, transaction, execution,
                             statement_preparation, statement_execution,
                             miscellaneous, note);
   type Driver_Type      is (foundation, driver_mysql, driver_postgresql,
                             driver_sqlite, driver_firebird);
   type Null_Priority    is (native, nulls_first, nulls_last);
   type ISO_Keyword_List is array (Trax_Isolation) of String (1 .. 16);
   type Trax_ID          is mod 2 ** 64;

   subtype BLOB_Maximum  is Positive range 2 ** 12 .. 2 ** 30;
   subtype SQL_State     is String (1 .. 5);
   subtype Driver_Codes  is Integer range -999 .. 4999;
   subtype Posix_Port    is Natural range 0 .. 65535;
   subtype Affected_Rows is Trax_ID;

   Iso_Keywords : constant ISO_Keyword_List :=
     ("READ UNCOMMITTED",
      "READ COMMITTED  ",
      "REPEATABLE READ ",
      "SERIALIZABLE    ");

   blankstring : constant String      := "";
   stateless   : constant SQL_State   := "     ";
   portless    : constant Posix_Port  := 0;

   type field_types is (ft_nbyte0, ft_nbyte1, ft_nbyte2, ft_nbyte3, ft_nbyte4,
                        ft_nbyte8, ft_byte1, ft_byte2, ft_byte3, ft_byte4,
                        ft_byte8, ft_real9, ft_real18, ft_textual,
                        ft_widetext, ft_supertext, ft_timestamp,
                        ft_chain, ft_enumtype, ft_settype);

   ERRMODE_EXCEPTION : exception;

end AdaBase;
