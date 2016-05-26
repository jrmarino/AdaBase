--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with CommonText;
with AdaBase.Connection.Base;
with AdaBase.Interfaces.Statement;
with AdaBase.Logger.Facility;
with AdaBase.Results.Sets;
with AdaBase.Results.Field;
with AdaBase.Results.Converters;
with AdaBase.Results.Generic_Converters;
with Ada.Calendar.Formatting;
with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;
with Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Characters.Conversions;
with Ada.Unchecked_Deallocation;

package AdaBase.Statement.Base is

   package CT  renames CommonText;
   package SUW renames Ada.Strings.Wide_Unbounded;
   package SWW renames Ada.Strings.Wide_Wide_Unbounded;
   package CAL renames Ada.Calendar;
   package CFM renames Ada.Calendar.Formatting;
   package ACC renames Ada.Characters.Conversions;
   package AR  renames AdaBase.Results;
   package ACB renames AdaBase.Connection.Base;
   package AIS renames AdaBase.Interfaces.Statement;
   package ALF renames AdaBase.Logger.Facility;
   package ARC renames AdaBase.Results.Converters;
   package RGC renames AdaBase.Results.Generic_Converters;
   package ARF renames AdaBase.Results.Field;
   package ARS renames AdaBase.Results.Sets;
   package ACH renames Ada.Characters.Handling;


   type SQL_Access is access all String;

   type Base_Statement is
     abstract new Base_Pure and AIS.iStatement with private;
   type Basic_Statement is access all Base_Statement'Class;

   type Stmt_Type is (direct_statement, prepared_statement);

   ILLEGAL_BIND_SQL         : exception;
   INVALID_FOR_DIRECT_QUERY : exception;
   INVALID_FOR_RESULT_SET   : exception;
   INVALID_COLUMN_INDEX     : exception;
   PRIOR_EXECUTION_FAILED   : exception;
   BINDING_COLUMN_NOT_FOUND : exception;
   BINDING_TYPE_MISMATCH    : exception;
   BINDING_SIZE_MISMATCH    : exception;
   STMT_PREPARATION         : exception;
   STMT_EXECUTION           : exception;
   MARKER_NOT_FOUND         : exception;

   overriding
   function rows_affected (Stmt : Base_Statement) return Affected_Rows;

   overriding
   function successful (Stmt : Base_Statement) return Boolean;

   overriding
   function data_discarded (Stmt : Base_Statement) return Boolean;

   overriding
   procedure iterate (Stmt    : out Base_Statement;
                      process : not null access procedure);

   overriding
   procedure iterate (Stmt    : out Base_Statement;
                      process : not null access procedure (row : ARS.Datarow));

   -------------------------------------------
   --      20 bind using integer index      --
   -------------------------------------------
   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.NByte0_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.NByte1_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.NByte2_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.NByte3_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.NByte4_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.NByte8_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.Byte1_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.Byte2_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.Byte3_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.Byte4_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.Byte8_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.Real9_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.Real18_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.Str1_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.Str2_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.Str4_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.Time_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.Chain_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.Enum_Access);

   procedure bind (Stmt  : out Base_Statement;
                   index : Positive;
                   vaxx  : AR.Settype_Access);


   -------------------------------------------
   --    20 bind using header for index     --
   -------------------------------------------
   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.NByte0_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.NByte1_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.NByte2_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.NByte3_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.NByte4_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.NByte8_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.Byte1_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.Byte2_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.Byte3_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.Byte4_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.Byte8_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.Real9_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.Real18_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.Str1_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.Str2_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.Str4_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.Time_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.Chain_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.Enum_Access);

   procedure bind (Stmt    : out Base_Statement;
                   heading : String;
                   vaxx    : AR.Settype_Access);


   --------------------------------------------
   --  20 assign/access using integer index  --
   --------------------------------------------
   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.NByte0_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.NByte1_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.NByte2_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.NByte3_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.NByte4_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.NByte8_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Byte1_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Byte2_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Byte3_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Byte4_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Byte8_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Real9_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Real18_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Str1_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Str2_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Str4_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Time_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Chain_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Enum_Access);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Settype_Access);

   ------------------------------------------------
   --  20 assign/access using moniker for index  --
   ------------------------------------------------
   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.NByte0_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.NByte1_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.NByte2_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.NByte3_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.NByte4_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.NByte8_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Byte1_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Byte2_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Byte3_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Byte4_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Byte8_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Real9_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Real18_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Str1_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Str2_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Str4_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Time_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Chain_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Enum_Access);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Settype_Access);


   -------------------------------------------
   --  21 assign/value using integer index  --
   -------------------------------------------
   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.NByte0);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.NByte1);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.NByte2);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.NByte3);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.NByte4);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.NByte8);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Byte1);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Byte2);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Byte3);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Byte4);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Byte8);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Real9);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Real18);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : String);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Textual);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Textwide);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Textsuper);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : CAL.Time);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Chain);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Enumtype);

   procedure assign (Stmt  : out Base_Statement;
                     index : Positive;
                     vaxx  : AR.Settype);


   -----------------------------------------------
   --  21 assign/value using moniker for index  --
   -----------------------------------------------
   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.NByte0);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.NByte1);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.NByte2);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.NByte3);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.NByte4);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.NByte8);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Byte1);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Byte2);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Byte3);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Byte4);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Byte8);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Real9);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Real18);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : String);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Textual);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Textwide);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Textsuper);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : CAL.Time);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Chain);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Enumtype);

   procedure assign (Stmt    : out Base_Statement;
                     moniker : String;
                     vaxx    : AR.Settype);

private

   logger_access : ALF.LogFacility_access;

   function Same_Strings (S, T : String) return Boolean;

   function transform_sql (Stmt : out Base_Statement; sql : String)
                           return String;

   procedure log_nominal (statement : Base_Statement;
                          category  : Log_Category;
                          message   : String);

   procedure free_datarow is new Ada.Unchecked_Deallocation
     (AR.Sets.Datarow, AR.Sets.Datarow_Access);

   procedure free_sql is new Ada.Unchecked_Deallocation
     (String, SQL_Access);

   procedure check_bound_column_access (absent : Boolean);

   package Markers is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Positive,
      Equivalent_Keys => Same_Strings,
      Hash            => Ada.Strings.Hash);

   function convert is new RGC.convert4str (IntType => AR.NByte1);
   function convert is new RGC.convert4str (IntType => AR.NByte2);
   function convert is new RGC.convert4str (IntType => AR.NByte3);
   function convert is new RGC.convert4str (IntType => AR.NByte4);
   function convert is new RGC.convert4str (IntType => AR.NByte8);
   function convert is new RGC.convert4str (IntType => AR.Byte1);
   function convert is new RGC.convert4str (IntType => AR.Byte2);
   function convert is new RGC.convert4str (IntType => AR.Byte3);
   function convert is new RGC.convert4str (IntType => AR.Byte4);
   function convert is new RGC.convert4str (IntType => AR.Byte8);
   function convert is new RGC.convert4st2 (RealType => AR.Real9);
   function convert is new RGC.convert4st2 (RealType => AR.Real18);
   function convert (nv : String) return AR.Textwide;
   function convert (nv : String) return AR.Textsuper;

   type bindrec (output_type : field_types := ft_nbyte0)
   is record
      bound     : Boolean := False;
      null_data : Boolean := False;
      case output_type is
         when ft_nbyte0    => a00 : AR.NByte0_Access;
                              v00 : AR.NByte0;
         when ft_nbyte1    => a01 : AR.NByte1_Access;
                              v01 : AR.NByte1;
         when ft_nbyte2    => a02 : AR.NByte2_Access;
                              v02 : AR.NByte2;
         when ft_nbyte3    => a03 : AR.NByte3_Access;
                              v03 : AR.NByte3;
         when ft_nbyte4    => a04 : AR.NByte4_Access;
                              v04 : AR.NByte4;
         when ft_nbyte8    => a05 : AR.NByte8_Access;
                              v05 : AR.NByte8;
         when ft_byte1     => a06 : AR.Byte1_Access;
                              v06 : AR.Byte1;
         when ft_byte2     => a07 : AR.Byte2_Access;
                              v07 : AR.Byte2;
         when ft_byte3     => a08 : AR.Byte3_Access;
                              v08 : AR.Byte3;
         when ft_byte4     => a09 : AR.Byte4_Access;
                              v09 : AR.Byte4;
         when ft_byte8     => a10 : AR.Byte8_Access;
                              v10 : AR.Byte8;
         when ft_real9     => a11 : AR.Real9_Access;
                              v11 : AR.Real9;
         when ft_real18    => a12 : AR.Real18_Access;
                              v12 : AR.Real18;
         when ft_textual   => a13 : AR.Str1_Access;
                              v13 : AR.Textual;
         when ft_widetext  => a14 : AR.Str2_Access;
                              v14 : AR.Textwide;
         when ft_supertext => a15 : AR.Str4_Access;
                              v15 : AR.Textsuper;
         when ft_timestamp => a16 : AR.Time_Access;
                              v16 : CAL.Time;
         when ft_chain     => a17 : AR.Chain_Access;
                              v17 : AR.Textual;
         when ft_enumtype  => a18 : AR.Enum_Access;
                              v18 : AR.Enumtype;
         when ft_settype   => a19 : AR.Settype_Access;
                              v19 : AR.Textual;
      end case;
   end record;

   procedure set_as_null (param : bindrec);


   --  For fetch_bound
   function bind_proceed (Stmt : Base_Statement; index : Positive)
                          return Boolean;

   function bind_index (Stmt : Base_Statement; heading : String)
                        return Positive;

   function assign_index (Stmt : Base_Statement; moniker : String)
                          return Positive;

   procedure auto_assign (Stmt  : out Base_Statement; index : Positive;
                          value : String);

   package bind_crate is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => bindrec);

   type Base_Statement is
     abstract new Base_Pure and AIS.iStatement with record
      successful_execution : Boolean       := False;
      result_present       : Boolean       := False;
      rows_leftover        : Boolean       := False;
      dialect              : Driver_Type   := foundation;
      impacted             : Affected_Rows := 0;
      connection           : ACB.Base_Connection_Access;
      alpha_markers        : Markers.Map;
      headings_map         : Markers.Map;
      crate                : bind_crate.Vector;
      realmccoy            : bind_crate.Vector;
   end record;

end AdaBase.Statement.Base;
