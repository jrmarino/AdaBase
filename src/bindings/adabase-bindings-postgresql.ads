--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt
--
--  Some of these definitions originate from the Matresha Project
--  http://forge.ada-ru.org/matreshka
--  Used with permission from Vadim Godunko <vgodunko@gmail.com>

with Interfaces.C.Strings;

package AdaBase.Bindings.PostgreSQL is

   pragma Preelaborate;

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   ------------------------
   --  Type Definitions  --
   ------------------------

   type PGconn is limited private;

   type PGconn_Access is access all PGconn;
   pragma Convention (C, PGconn_Access);

   type PGresult is limited private;

   type PGresult_Access is access all PGresult;
   pragma Convention (C, PGresult_Access);

   type Oid is new IC.int;

   type Oid_Access is access all Oid;
   pragma Convention (C, Oid_Access);

   type ConnStatusType is
    (CONNECTION_OK,
     CONNECTION_BAD,
     CONNECTION_STARTED,
     CONNECTION_MADE,
     CONNECTION_AWAITING_RESPONSE,
     CONNECTION_AUTH_OK,
     CONNECTION_SETENV,
     CONNECTION_SSL_STARTUP,
     CONNECTION_NEEDED);
   pragma Convention (C, ConnStatusType);

   type ExecStatusType is
    (PGRES_EMPTY_QUERY,
     PGRES_COMMAND_OK,
     PGRES_TUPLES_OK,
     PGRES_COPY_OUT,
     PGRES_COPY_IN,
     PGRES_BAD_RESPONSE,
     PGRES_NONFATAL_ERROR,
     PGRES_FATAL_ERROR);
   pragma Convention (C, ExecStatusType);

   type chars_ptr_Access is access all ICS.chars_ptr;
   pragma Convention (C, chars_ptr_Access);

   type int_Access is access all IC.int;
   pragma Convention (C, int_Access);


   ------------------------
   --  Type Definitions  --
   ------------------------

   PG_DIAG_SEVERITY        : constant IC.int := IC.int (Character'Pos ('S'));
   PG_DIAG_SQLSTATE        : constant IC.int := IC.int (Character'Pos ('C'));
   PG_DIAG_MESSAGE_PRIMARY : constant IC.int := IC.int (Character'Pos ('M'));
   PG_DIAG_MESSAGE_DETAIL  : constant IC.int := IC.int (Character'Pos ('D'));
   PG_DIAG_SCHEMA_NAME     : constant IC.int := IC.int (Character'Pos ('s'));
   PG_DIAG_TABLE_NAME      : constant IC.int := IC.int (Character'Pos ('t'));
   PG_DIAG_COLUMN_NAME     : constant IC.int := IC.int (Character'Pos ('c'));
   PG_DIAG_DATATYPE_NAME   : constant IC.int := IC.int (Character'Pos ('d'));
   PG_DIAG_CONSTRAINT_NAME : constant IC.int := IC.int (Character'Pos ('n'));

   -----------------
   -- Subprograms --
   -----------------

   function pg_encoding_to_char (encoding_id : IC.int) return ICS.chars_ptr;
   pragma Import (C, pg_encoding_to_char);

   procedure PQclear (res : PGresult_Access);
   pragma Import (C, PQclear);

   function PQclientEncoding (conn : PGconn_Access) return IC.int;
   pragma Import (C, PQclientEncoding);

   function PQconnectdbParams (keywords     : ICS.chars_ptr_array;
                               values       : ICS.chars_ptr_array;
                               expand_dbnam : IC.int) return PGconn_Access;
   pragma Import (C, PQconnectdbParams);

   function PQerrorMessage (conn : PGconn_Access) return ICS.chars_ptr;
   pragma Import (C, PQerrorMessage);

   function PQresultErrorField (res : PGresult_Access; fieldcode : IC.int)
                                return ICS.chars_ptr;
   pragma Import (C, PQresultErrorField);

   function PQexec (conn    : PGconn_Access;
                    command : ICS.chars_ptr) return PGresult_Access;
   pragma Import (C, PQexec);

   function PQexecPrepared (conn         : PGconn_Access;
                            stmtName     : ICS.chars_ptr;
                            nParams      : IC.int;
                            paramValues  : ICS.chars_ptr_array;
                            paramLengths : int_Access;
                            paramFormats : int_Access;
                            resultFormat : IC.int) return PGresult_Access;
   pragma Import (C, PQexecPrepared);

   procedure PQfinish (conn : PGconn_Access);
   pragma Import (C, PQfinish);

   function PQftype (res : PGresult_Access; column_number : IC.int) return Oid;
   pragma Import (C, PQftype);

   function PQgetisnull (res           : PGresult_Access;
                         row_number    : IC.int;
                         column_number : IC.int) return IC.int;
   pragma Import (C, PQgetisnull);

   function PQgetlength (res           : PGresult_Access;
                         row_number    : IC.int;
                         column_number : IC.int) return IC.int;
   pragma Import (C, PQgetlength);

   function PQgetvalue  (res           : PGresult_Access;
                         row_number    : IC.int;
                         column_number : IC.int) return ICS.chars_ptr;
   pragma Import (C, PQgetvalue);

   function PQfformat   (res           : PGresult_Access;
                         column_number : IC.int) return IC.int;
   pragma Import (C, PQfformat);

   function PQisthreadsafe return IC.int;
   pragma Import (C, PQisthreadsafe);

   function PQntuples (res : PGresult_Access) return IC.int;
   pragma Import (C, PQntuples);

   function PQnfields (res : PGresult_Access) return IC.int;
   pragma Import (C, PQnfields);

   function PQnparams (res : PGresult_Access) return IC.int;
   pragma Import (C, PQnparams);

   function PQfname (res : PGresult_Access; column_number : IC.int)
                     return ICS.chars_ptr;
   pragma Import (C, PQfname);

   function PQprepare (conn       : PGconn_Access;
                       stmtName   : ICS.chars_ptr;
                       query      : ICS.chars_ptr;
                       nParams    : IC.int;
                       paramTypes : Oid_Access) return PGresult_Access;
   pragma Import (C, PQprepare);

   function PQresultStatus (res : PGresult_Access) return ExecStatusType;
   pragma Import (C, PQresultStatus);

   function PQsetClientEncoding (conn     : PGconn_Access;
                                 encoding : ICS.chars_ptr) return IC.int;
   pragma Import (C, PQsetClientEncoding);

   function PQstatus (conn : PGconn_Access) return ConnStatusType;
   pragma Import (C, PQstatus);

private

   type PGconn   is limited null record;
   type PGresult is limited null record;


end AdaBase.Bindings.PostgreSQL;
