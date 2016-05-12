--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt
--
--  Some of these definitions originate from the Matresha Project
--  http://forge.ada-ru.org/matreshka
--  Used with permission from Vadim Godunko <vgodunko@gmail.com>

with System;
with Interfaces.C.Strings;

package AdaBase.Bindings.SQLite is

   pragma Preelaborate;

   package SYS renames System;
   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   ------------------------
   --  Type Definitions  --
   ------------------------

   type sql64 is new Interfaces.Integer_64;

   type sqlite3 is limited private;

   type sqlite3_Access is access all sqlite3;
   pragma Convention (C, sqlite3_Access);

   type sqlite3_stmt is limited private;

   type sqlite3_stmt_Access is access all sqlite3_stmt;
   pragma Convention (C, sqlite3_stmt_Access);

   type sqlite3_destructor is access procedure (text : ICS.char_array_access);
   pragma Convention (C, sqlite3_destructor);

   ---------------
   -- Constants --
   ---------------

   type enum_field_types is
     (SQLITE_INTEGER,
      SQLITE_FLOAT,
      SQLITE_TEXT,
      SQLITE_BLOB,
      SQLITE_NULL);
   pragma Convention (C, enum_field_types);
   for enum_field_types use
     (SQLITE_INTEGER => 1,
      SQLITE_FLOAT   => 2,
      SQLITE_TEXT    => 3,
      SQLITE_BLOB    => 4,
      SQLITE_NULL    => 5);

   SQLITE_OK      : constant :=   0;  --  Successful result
   SQLITE_ROW     : constant := 100;  --  sqlite3_step() has another row ready
   SQLITE_DONE    : constant := 101;  --  sqlite3_step() has finished executing

   SQLITE_CONFIG_SINGLETHREAD : constant := 1;  --  nil
   SQLITE_CONFIG_MULTITHREAD  : constant := 2;  --  nil
   SQLITE_CONFIG_SERIALIZED   : constant := 3;  --  nil

   SQLITE_STATIC    : constant IC.int := IC.int (0);
   SQLITE_TRANSIENT : constant IC.int := IC.int (-1);

   ---------------------
   --  Library Calls  --
   ----------------------

   --  For now, only support SQLITE_STATIC and SQLITE_TRANSIENT at the
   --  cost of sqlite3_destructor.  Shame on them mixing pointers and integers
   --  Applies to bind_text and bind_blob
   function sqlite3_bind_text (Handle     : sqlite3_stmt_Access;
                               Index      : IC.int;
                               Text       : ICS.chars_ptr;
                               nBytes     : IC.int;
                               destructor : IC.int) return IC.int;
   pragma Import (C, sqlite3_bind_text);

   function sqlite3_bind_blob (Handle     : sqlite3_stmt_Access;
                               Index      : IC.int;
                               binary     : ICS.char_array_access;
                               nBytes     : IC.int;
                               destructor : IC.int) return IC.int;
   pragma Import (C, sqlite3_bind_blob);

   function sqlite3_bind_double (Handle : not null sqlite3_stmt_Access;
                                 Index  : IC.int;
                                 Value  : IC.double) return IC.int;
   pragma Import (C, sqlite3_bind_double);

   function sqlite3_bind_int64 (Handle : not null sqlite3_stmt_Access;
                                Index  : IC.int;
                                Value  : sql64) return IC.int;
   pragma Import (C, sqlite3_bind_int64);

   function sqlite3_bind_null (Handle : not null sqlite3_stmt_Access;
                               Index  : IC.int) return IC.int;
   pragma Import (C, sqlite3_bind_null);

   function sqlite3_bind_parameter_count
     (Handle : not null sqlite3_stmt_Access) return IC.int;
   pragma Import (C, sqlite3_bind_parameter_count);

   function sqlite3_column_count (Handle : not null sqlite3_stmt_Access)
                                  return IC.int;
   pragma Import (C, sqlite3_column_count);

   function sqlite3_column_table_name (Handle : not null sqlite3_stmt_Access;
                                       index  : IC.int) return ICS.chars_ptr;
   pragma Import (C, sqlite3_column_table_name);

   function sqlite3_column_origin_name (Handle : not null sqlite3_stmt_Access;
                                        index  : IC.int) return ICS.chars_ptr;
   pragma Import (C, sqlite3_column_origin_name);

   function sqlite3_column_database_name
     (Handle : not null sqlite3_stmt_Access;
      index  : IC.int) return ICS.chars_ptr;
   pragma Import (C, sqlite3_column_database_name);

   function sqlite3_table_column_metadata
     (Handle : not null sqlite3_Access;
      dbname : ICS.chars_ptr;
      table  : ICS.chars_ptr;
      column : ICS.chars_ptr;
      datatype : access ICS.chars_ptr;
      collseq  : access ICS.chars_ptr;
      notnull  : access IC.int;
      primekey : access IC.int;
      autoinc  : access IC.int) return IC.int;
   pragma Import (C, sqlite3_table_column_metadata);

   function sqlite3_close (db : not null sqlite3_Access) return IC.int;
   pragma Import (C, sqlite3_close);

   function sqlite3_column_type    (Handle : not null sqlite3_stmt_Access;
                                    iCol   : IC.int) return IC.int;
   pragma Import (C, sqlite3_column_type);

   function sqlite3_column_bytes   (Handle : not null sqlite3_stmt_Access;
                                    iCol   : IC.int) return IC.int;
   pragma Import (C, sqlite3_column_bytes);

   function sqlite3_column_double  (Handle : not null sqlite3_stmt_Access;
                                    iCol   : IC.int) return IC.double;
   pragma Import (C, sqlite3_column_double);

   function sqlite3_column_int64   (Handle : not null sqlite3_stmt_Access;
                                    iCol   : IC.int) return sql64;
   pragma Import (C, sqlite3_column_int64);

   function sqlite3_column_text    (Handle : not null sqlite3_stmt_Access;
                                    iCol   : IC.int) return ICS.chars_ptr;
   pragma Import (C, sqlite3_column_text);

   function sqlite3_column_blob    (Handle : not null sqlite3_stmt_Access;
                                    iCol   : IC.int) return ICS.chars_ptr;
   pragma Import (C, sqlite3_column_blob);

   function sqlite3_config (Option : IC.int) return IC.int;
   pragma Import (C, sqlite3_config);

   function sqlite3_errmsg (db : not null sqlite3_Access) return ICS.chars_ptr;
   pragma Import (C, sqlite3_errmsg);

   function sqlite3_errcode (db : not null sqlite3_Access) return IC.int;
   pragma Import (C, sqlite3_errcode);

   function sqlite3_changes (db : not null sqlite3_Access) return IC.int;
   pragma Import (C, sqlite3_changes);

   function sqlite3_last_insert_rowid (db : not null sqlite3_Access)
                                       return sql64;
   pragma Import (C, sqlite3_last_insert_rowid);

   function sqlite3_exec (db : not null sqlite3_Access;
                          sql : ICS.chars_ptr;
                          callback : System.Address;
                          firstarg : System.Address;
                          errmsg   : System.Address) return IC.int;
   pragma Import (C, sqlite3_exec);

   function sqlite3_open (File_Name : ICS.chars_ptr;
                          Handle    : not null access sqlite3_Access)
                          return IC.int;
   pragma Import (C, sqlite3_open);

   function sqlite3_prepare_v2 (db     : not null sqlite3_Access;
                                zSql   : ICS.chars_ptr;
                                nByte  : IC.int;
                                ppStmt : not null access sqlite3_stmt_Access;
                                pzTail : not null access ICS.chars_ptr)
                                return IC.int;
   pragma Import (C, sqlite3_prepare_v2);

   function sqlite3_reset (pStmt : not null sqlite3_stmt_Access) return IC.int;
   pragma Import (C, sqlite3_reset);

   function sqlite3_step (Handle : not null sqlite3_stmt_Access) return IC.int;
   pragma Import (C, sqlite3_step);

   function sqlite3_finalize (Handle : not null sqlite3_stmt_Access)
                              return IC.int;
   pragma Import (C, sqlite3_finalize);

   function sqlite3_libversion return ICS.chars_ptr;
   pragma Import (C, sqlite3_libversion);

   function sqlite3_sourceid return ICS.chars_ptr;
   pragma Import (C, sqlite3_sourceid);

private

   type sqlite3      is limited null record;
   type sqlite3_stmt is limited null record;

end AdaBase.Bindings.SQLite;
