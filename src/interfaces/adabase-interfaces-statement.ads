--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Results.Sets;

package AdaBase.Interfaces.Statement is

   type iStatement is limited interface;

   function successful    (Stmt : iStatement) return Boolean is abstract;
   function column_count  (Stmt : iStatement) return Natural is abstract;

   function last_driver_message (Stmt : iStatement) return String is abstract;
   function last_insert_id      (Stmt : iStatement) return TraxID is abstract;
   function last_sql_state      (Stmt : iStatement) return TSqlState
                                 is abstract;
   function last_driver_code    (Stmt : iStatement) return DriverCodes
                                 is abstract;

   function data_discarded      (Stmt : iStatement) return Boolean is abstract;

   procedure discard_rest       (Stmt : out iStatement) is null;


   function execute         (Stmt : iStatement) return Boolean is abstract;
   function execute         (Stmt : iStatement; bind_piped : String)
                             return Boolean is abstract;

   function rows_affected   (Stmt : iStatement)
                             return AffectedRows is abstract;

   function rows_returned   (Stmt : iStatement)
                             return AffectedRows is abstract;

   function column_name     (Stmt : iStatement; index : Positive)
                             return String is abstract;

   function column_table    (Stmt : iStatement; index : Positive)
                             return String is abstract;

   function column_native_type (Stmt : iStatement; index : Positive)
                                return field_types is abstract;

   function fetch_next      (Stmt : iStatement)
                             return AdaBase.Results.Sets.DataRow_Access
                             is abstract;

   function fetch_all       (Stmt : iStatement)
                             return AdaBase.Results.Sets.DataRowSet
                             is abstract;

   function fetch_bound     (Stmt : iStatement) return Boolean is abstract;

   procedure fetch_next_set (Stmt : out iStatement;
                             success : out Boolean) is null;

   ------------------------------------------------------------------------
   --  Technically there should be 20 of these listed.  They are all
   --  implemented in the base class so they get inherited.  I'm too
   --  Lazy to add 40 prototypes here.  Same with Assign
   --
   --     procedure bind    (Stmt    : out Base_Statement;
   --                        index   : Positive;
   --                        vaxx    : AR.nbyte0_access);
   --     procedure bind    (Stmt    : out Base_Statement;
   --                        heading : String;
   --                        vaxx    : AR.nbyte0_access);
   --     procedure assign  (Stmt    : out Base_Statement;
   --                        index   : Positive;
   --                        vaxx    : AR.nbyte0_access);
   --     procedure assign  (Stmt    : out Base_Statement;
   --                        heading : String;
   --                        vaxx    : AR.nbyte0_access);
   -------------------------------------------------------------------------

end AdaBase.Interfaces.Statement;
