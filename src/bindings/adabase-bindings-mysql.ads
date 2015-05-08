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
--
--  Some of these definitions originate from the Matresha Project
--  http://forge.ada-ru.org/matreshka
--  Used with permission from Vadim Godunko <vgodunko@gmail.com>
--

with System;
with Interfaces.C.Strings;

package AdaBase.Bindings.MySQL is

   pragma Preelaborate;

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   ------------------------
   --  Type Definitions  --
   ------------------------

   type enum_field_types is
    (MYSQL_TYPE_DECIMAL,
     MYSQL_TYPE_TINY,
     MYSQL_TYPE_SHORT,
     MYSQL_TYPE_LONG,
     MYSQL_TYPE_FLOAT,
     MYSQL_TYPE_DOUBLE,
     MYSQL_TYPE_NULL,
     MYSQL_TYPE_TIMESTAMP,
     MYSQL_TYPE_LONGLONG,
     MYSQL_TYPE_INT24,
     MYSQL_TYPE_DATE,
     MYSQL_TYPE_TIME,
     MYSQL_TYPE_DATETIME,
     MYSQL_TYPE_YEAR,
     MYSQL_TYPE_NEWDATE,
     MYSQL_TYPE_VARCHAR,
     MYSQL_TYPE_BIT,
     MYSQL_TYPE_NEWDECIMAL,
     MYSQL_TYPE_ENUM,
     MYSQL_TYPE_SET,
     MYSQL_TYPE_TINY_BLOB,
     MYSQL_TYPE_MEDIUM_BLOB,
     MYSQL_TYPE_LONG_BLOB,
     MYSQL_TYPE_BLOB,
     MYSQL_TYPE_VAR_STRING,
     MYSQL_TYPE_STRING,
     MYSQL_TYPE_GEOMETRY);
   pragma Convention (C, enum_field_types);
   for enum_field_types use
    (MYSQL_TYPE_DECIMAL     => 0,
     MYSQL_TYPE_TINY        => 1,
     MYSQL_TYPE_SHORT       => 2,
     MYSQL_TYPE_LONG        => 3,
     MYSQL_TYPE_FLOAT       => 4,
     MYSQL_TYPE_DOUBLE      => 5,
     MYSQL_TYPE_NULL        => 6,
     MYSQL_TYPE_TIMESTAMP   => 7,
     MYSQL_TYPE_LONGLONG    => 8,
     MYSQL_TYPE_INT24       => 9,
     MYSQL_TYPE_DATE        => 10,
     MYSQL_TYPE_TIME        => 11,
     MYSQL_TYPE_DATETIME    => 12,
     MYSQL_TYPE_YEAR        => 13,
     MYSQL_TYPE_NEWDATE     => 14,
     MYSQL_TYPE_VARCHAR     => 15,
     MYSQL_TYPE_BIT         => 16,
     MYSQL_TYPE_NEWDECIMAL  => 246,
     MYSQL_TYPE_ENUM        => 247,
     MYSQL_TYPE_SET         => 248,
     MYSQL_TYPE_TINY_BLOB   => 249,
     MYSQL_TYPE_MEDIUM_BLOB => 250,
     MYSQL_TYPE_LONG_BLOB   => 251,
     MYSQL_TYPE_BLOB        => 252,
     MYSQL_TYPE_VAR_STRING  => 253,
     MYSQL_TYPE_STRING      => 254,
     MYSQL_TYPE_GEOMETRY    => 255);

   --  True at least as far back as MySQL 5.1
   type MYSQL_FIELD is record
      name             : ICS.chars_ptr;
      org_name         : ICS.chars_ptr;
      table            : ICS.chars_ptr;
      org_table        : ICS.chars_ptr;
      db               : ICS.chars_ptr;
      catalog          : ICS.chars_ptr;
      def              : ICS.chars_ptr;
      length           : IC.unsigned_long;
      max_length       : IC.unsigned_long;
      name_length      : IC.unsigned;
      org_name_length  : IC.unsigned;
      table_length     : IC.unsigned;
      org_table_length : IC.unsigned;
      db_length        : IC.unsigned;
      catalog_length   : IC.unsigned;
      def_length       : IC.unsigned;
      flags            : IC.unsigned;
      decimals         : IC.unsigned;
      charsetnr        : IC.unsigned;
      field_type       : enum_field_types;
      extension        : System.Address;
   end record;
   pragma Convention (C, MYSQL_FIELD);

   type MY_CHARSET_INFO is record
      number           : IC.unsigned;
      state            : IC.unsigned;
      csname           : ICS.chars_ptr;
      name             : ICS.chars_ptr;
      comment          : ICS.chars_ptr;
      dir              : ICS.chars_ptr;
      mbminlen         : IC.unsigned;
      mbmaxlen         : IC.unsigned;
   end record;
   pragma Convention (C, MY_CHARSET_INFO);

   type mysql_option is
    (MYSQL_OPT_CONNECT_TIMEOUT,
     MYSQL_OPT_COMPRESS,
     MYSQL_OPT_NAMED_PIPE,
     MYSQL_INIT_COMMAND,
     MYSQL_READ_DEFAULT_FILE,
     MYSQL_READ_DEFAULT_GROUP,
     MYSQL_SET_CHARSET_DIR,
     MYSQL_SET_CHARSET_NAME,
     MYSQL_OPT_LOCAL_INFILE,
     MYSQL_OPT_PROTOCOL,
     MYSQL_SHARED_MEMORY_BASE_NAME,
     MYSQL_OPT_READ_TIMEOUT,
     MYSQL_OPT_WRITE_TIMEOUT,
     MYSQL_OPT_USE_RESULT,
     MYSQL_OPT_USE_REMOTE_CONNECTION,
     MYSQL_OPT_USE_EMBEDDED_CONNECTION,
     MYSQL_OPT_GUESS_CONNECTION,
     MYSQL_SET_CLIENT_IP,
     MYSQL_SECURE_AUTH,
     MYSQL_REPORT_DATA_TRUNCATION,
     MYSQL_OPT_RECONNECT,
     MYSQL_OPT_SSL_VERIFY_SERVER_CERT,
     MYSQL_PLUGIN_DIR,
     MYSQL_DEFAULT_AUTH);
   pragma Convention (C, mysql_option);

   type enum_mysql_set_option is
     (MYSQL_OPTION_MULTI_STATEMENTS_ON,
      MYSQL_OPTION_MULTI_STATEMENTS_OFF);
   pragma Convention (C, enum_mysql_set_option);

   type client_flag_order is
    (CLIENT_LONG_PASSWORD,
     CLIENT_FOUND_ROWS,
     CLIENT_LONG_FLAG,
     CLIENT_CONNECT_WITH_DB,
     CLIENT_NO_SCHEMA,
     CLIENT_COMPRESS,
     CLIENT_ODBC,
     CLIENT_LOCAL_FILES,
     CLIENT_IGNORE_SPACE,
     CLIENT_PROTOCOL_41,
     CLIENT_INTERACTIVE,
     CLIENT_SSL,
     CLIENT_IGNORE_SIGPIPE,
     CLIENT_TRANSACTIONS,
     CLIENT_RESERVED,
     CLIENT_SECURE_CONNECTION,
     CLIENT_MULTI_STATEMENTS,
     CLIENT_MULTI_RESULTS,
     CLIENT_PS_MULTI_RESULTS);

   type MYSQL      is limited private;
   type MYSQL_RES  is limited private;
   type MYSQL_STMT is limited private;

   type MYSQL_Access is access all MYSQL;
   pragma Convention (C, MYSQL_Access);

   type MYSQL_RES_Access is access all MYSQL_RES;
   pragma Convention (C, MYSQL_RES_Access);

   type MYSQL_STMT_Access is access all MYSQL_STMT;
   pragma Convention (C, MYSQL_STMT_Access);

   type MYSQL_FIELD_Access is access all MYSQL_FIELD;
   pragma Convention (C, MYSQL_FIELD_Access);

   type MY_CHARSET_INFO_Access is access all MY_CHARSET_INFO;
   pragma Convention (C, MY_CHARSET_INFO_Access);

   type my_bool      is new IC.signed_char;
   type my_ulong     is new IC.unsigned_long;
   type my_uint      is new IC.unsigned;
   type my_int       is new IC.int;
   type my_ulonglong is mod 2 ** 64;

   type my_ulong_access is access all my_ulong;

   type block_ulong  is array (Natural range <>) of my_ulong;
   type block_char   is array (Natural range <>) of ICS.chars_ptr;
   type MYSQL_LEN    is limited record
      len : block_ulong (1 .. 1);  -- number is arbitrary, unchecked conv
   end record;

   type MYSQL_LEN_Access is access all MYSQL_LEN;
   pragma Convention (C, MYSQL_LEN_Access);

   type MYSQL_ROW is limited record
      binary : block_char (1 .. 1);   -- number is arbitrary, unchecked conv
   end record;

   type MYSQL_ROW_access is access all MYSQL_ROW;
   pragma Convention (C, MYSQL_ROW_access);

   ---------------------
   --  Library calls  --
   ---------------------

   procedure mysql_close (handle : not null access MYSQL);
   pragma Import (C, mysql_close, "mysql_close");

   function mysql_commit (handle : not null access MYSQL) return my_bool;
   pragma Import (C, mysql_commit, "mysql_commit");

   function mysql_rollback (handle : not null access MYSQL) return my_bool;
   pragma Import (C, mysql_rollback, "mysql_rollback");

   function mysql_autocommit (handle : not null access MYSQL;
                              mode   : my_bool) return my_bool;
   pragma Import (C, mysql_autocommit, "mysql_autocommit");

   function mysql_insert_id (handle : not null access MYSQL)
                             return my_ulonglong;
   pragma Import (C, mysql_insert_id, "mysql_insert_id");

   function mysql_get_client_version return my_ulong;
   pragma Import (C, mysql_get_client_version, "mysql_get_client_version");

   function mysql_get_server_version (handle : not null access MYSQL)
                                      return my_ulong;
   pragma Import (C, mysql_get_server_version, "mysql_get_server_version");

   function mysql_get_client_info return ICS.chars_ptr;
   pragma Import (C, mysql_get_client_info, "mysql_get_client_info");

   function mysql_get_server_info (handle : not null access MYSQL)
                                   return ICS.chars_ptr;
   pragma Import (C, mysql_get_server_info, "mysql_get_server_info");

   function mysql_error (handle : not null access MYSQL) return ICS.chars_ptr;
   pragma Import (C, mysql_error, "mysql_error");

   function mysql_errno (handle : not null access MYSQL) return my_uint;
   pragma Import (C, mysql_errno, "mysql_errno");

   function mysql_sqlstate (handle : not null access MYSQL)
                            return ICS.chars_ptr;
   pragma Import (C, mysql_sqlstate, "mysql_sqlstate");

   function mysql_query (handle : not null access MYSQL;
                         stmt_str : ICS.chars_ptr) return my_int;
   pragma Import (C, mysql_query, "mysql_query");

   function mysql_real_query (handle   : not null access MYSQL;
                              stmt_str : ICS.chars_ptr;
                              length   : my_ulong) return my_int;
   pragma Import (C, mysql_real_query, "mysql_real_query");

   function mysql_init (handle : access MYSQL) return MYSQL_Access;
   pragma Import (C, mysql_init, "mysql_init");

   function mysql_options
    (handle : not null access MYSQL;
     option : mysql_option;
     arg    : IC.char_array) return my_int;
   pragma Import (C, mysql_options, "mysql_options");

   function mysql_real_connect
    (handle : not null access MYSQL;
     host   : ICS.chars_ptr;
     user   : ICS.chars_ptr;
     passwd : ICS.chars_ptr;
     db     : ICS.chars_ptr;
     port   : my_uint;
     unix_socket : ICS.chars_ptr;
     client_flag : my_ulong) return MYSQL_Access;
   pragma Import (C, mysql_real_connect, "mysql_real_connect");

   procedure mysql_free_result (handle : not null access MYSQL_RES);
   pragma Import (C, mysql_free_result, "mysql_free_result");

   function mysql_use_result (handle : not null access MYSQL)
                              return MYSQL_RES_Access;
   pragma Import (C, mysql_use_result, "mysql_use_result");

   function mysql_store_result (handle : not null access MYSQL)
                                return MYSQL_RES_Access;
   pragma Import (C, mysql_store_result, "mysql_store_result");

   function mysql_field_count (handle : not null access MYSQL) return my_uint;
   pragma Import (C, mysql_field_count, "mysql_field_count");

   function mysql_num_fields (handle : not null access MYSQL_RES)
                              return my_uint;
   pragma Import (C, mysql_num_fields, "mysql_num_fields");

   function mysql_num_rows (handle : not null access MYSQL_RES)
                            return my_ulonglong;
   pragma Import (C, mysql_num_rows, "mysql_num_rows");

   function mysql_affected_rows (handle : not null access MYSQL)
                                 return my_ulonglong;
   pragma Import (C, mysql_affected_rows, "mysql_affected_rows");

   function mysql_stmt_init (handle : not null access MYSQL)
                             return MYSQL_STMT_Access;
   pragma Import (C, mysql_stmt_init, "mysql_stmt_init");

   function mysql_stmt_close (handle : not null access MYSQL_STMT)
                              return my_bool;
   pragma Import (C, mysql_stmt_close, "mysql_stmt_close");

   function mysql_stmt_param_count (handle : not null access MYSQL_STMT)
                                    return my_ulong;
   pragma Import (C, mysql_stmt_param_count, "mysql_stmt_param_count");

   function mysql_stmt_insert_id (handle : not null access MYSQL_STMT)
                                  return my_ulonglong;
   pragma Import (C, mysql_stmt_insert_id, "mysql_stmt_insert_id");

   function mysql_stmt_sqlstate (handle : not null access MYSQL_STMT)
                                 return ICS.chars_ptr;
   pragma Import (C, mysql_stmt_sqlstate, "mysql_stmt_sqlstate");

   function mysql_stmt_errno (handle : not null access MYSQL_STMT)
                              return my_uint;
   pragma Import (C, mysql_stmt_errno, "mysql_stmt_errno");

   function mysql_stmt_error (handle : not null access MYSQL_STMT)
                              return ICS.chars_ptr;
   pragma Import (C, mysql_stmt_error, "mysql_stmt_error");

   function mysql_stmt_free_result (handle : not null access MYSQL_STMT)
                                    return my_bool;
   pragma Import (C, mysql_stmt_free_result, "mysql_stmt_free_result");

   function mysql_stmt_store_result (handle : not null access MYSQL_STMT)
                                     return my_int;
   pragma Import (C, mysql_stmt_store_result, "mysql_stmt_store_result");

   function mysql_fetch_field (handle : not null access MYSQL_RES)
                              return MYSQL_FIELD_Access;
   pragma Import (C, mysql_fetch_field, "mysql_fetch_field");

   function mysql_fetch_row (handle : not null access MYSQL_RES)
                             return MYSQL_ROW_access;
   pragma Import (C, mysql_fetch_row, "mysql_fetch_row");

   function mysql_fetch_lengths (result : not null access MYSQL_RES)
                                 return my_ulong_access;
   pragma Import (C, mysql_fetch_lengths, "mysql_fetch_lengths");

   function mysql_next_result (handle : not null access MYSQL) return my_int;
   pragma Import (C, mysql_next_result, "mysql_next_result");

   procedure mysql_get_character_set_info (handle : not null access MYSQL;
                                           cs     : access MY_CHARSET_INFO);
   pragma Import (C, mysql_get_character_set_info,
                  "mysql_get_character_set_info");

   function mysql_set_server_option (handle : not null access MYSQL;
                                     option : enum_mysql_set_option)
                                     return my_int;
   pragma Import (C, mysql_set_server_option, "mysql_set_server_option");

   function mysql_stmt_prepare (handle   : not null access MYSQL_STMT;
                                stmt_str : ICS.chars_ptr;
                                length   : my_ulong) return my_int;
   pragma Import (C, mysql_stmt_prepare, "mysql_stmt_prepare");


private

   type MYSQL      is limited null record;
   type MYSQL_RES  is limited null record;
   type MYSQL_STMT is limited null record;


end AdaBase.Bindings.MySQL;
