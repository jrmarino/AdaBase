---
title: Bind variables to specific columns for Fetching
---

<div class="leftside">
<pre class="code">
with CommonText;
with Ada.Calendar;
with Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;

package AdaBase.Results is

   package CT   renames CommonText;
   package AC   renames Ada.Calendar;
   package SUW  renames Ada.Strings.Wide_Unbounded;
   package SUWW renames Ada.Strings.Wide_Wide_Unbounded;

   subtype textual   is CT.Text;
   subtype textwide  is SUW.Unbounded_Wide_String;
   subtype textsuper is SUWW.Unbounded_Wide_Wide_String;

   -------------------------------------------
   --  Supported Field Types (Standardized) --
   -------------------------------------------

   type nbyte1 is mod 2 ** 8;
   type nbyte2 is mod 2 ** 16;
   type nbyte3 is mod 2 ** 24;
   type nbyte4 is mod 2 ** 32;
   type nbyte8 is mod 2 ** 64;
   type byte8  is range -2 ** 63 .. 2 ** 63 - 1;
   type byte4  is range -2 ** 31 .. 2 ** 31 - 1;
   type byte3  is range -2 ** 23 .. 2 ** 23 - 1;
   type byte2  is range -2 ** 15 .. 2 ** 15 - 1;
   type byte1  is range -2 **  7 .. 2 **  7 - 1;
   type real9  is digits 9;
   type real18 is digits 18;

   subtype nbyte0 is Boolean;

   type chain is array (Positive range <>) of nbyte1;
   type enumtype is record
      enumeration : textual;
      index       : Natural;
   end record;
   type settype is array (Positive range <>) of enumtype;

   type nbyte0_access  is access all nbyte0;
   type nbyte1_access  is access all nbyte1;
   type nbyte2_access  is access all nbyte2;
   type nbyte3_access  is access all nbyte3;
   type nbyte4_access  is access all nbyte4;
   type nbyte8_access  is access all nbyte8;
   type byte1_access   is access all byte1;
   type byte2_access   is access all byte2;
   type byte3_access   is access all byte3;
   type byte4_access   is access all byte4;
   type byte8_access   is access all byte8;
   type real9_access   is access all real9;
   type real18_access  is access all real18;
   type str1_access    is access all textual;
   type str2_access    is access all textwide;
   type str4_access    is access all textsuper;
   type time_access    is access all AC.Time;
   type chain_access   is access all chain;     --  stored as access
   type enum_access    is access all enumtype;
   type settype_access is access all settype;   --  stored as access

end AdaBase.Results;
</pre>
<p>
This page documents 40 similar bind functions, two for each standard data type.
These functions bind to the columns of fetched data rows.  The first 20 functions
reference the column by its numeric index, and the last 20 functions reference
the column through an associative array of the column's name.
</p>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.nbyte0_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.nbyte1_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.nbyte2_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.nbyte3_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.nbyte4_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.nbyte8_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.byte1_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.byte2_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.byte3_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.byte4_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.byte8_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.real9_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.real18_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.str1_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.str2_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.str4_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.time_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.chain_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.enum_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.settype_access)</h3>
<p>
Prior to issuing the <b>fetch_bound</b> command and after the query has been
executed, the desired columns must be bound to variables.  The data from unbound
columns is not accessible.  The variables to be bound must be the exact same
datatype as native type of the column itself (this can be determined with the
<b>native_type</b> function.  The first 20 of the 40 overloaded bind functions
accept an index starting with 1 that matches the column number of the result
row.  The <i>vaxx</i> argument accepts a pointer to one of the 20 native
data type.
</p>
<p>Binding is something that only has to be done once per query, regardless of
how many rows require fetching.  As soon as the <b>fetch_bound</b> function is
executed, the variables are updated with the values from the fetched row, which
leads to clean code free of typical variable assignments. Depending on the
scope of the bound variables, the Unchecked_Access attribute may be required
over the safer Access to avoid a "non-local pointer cannot point to local
object" error.
</p>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.nbyte0_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.nbyte1_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.nbyte2_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.nbyte3_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.nbyte4_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.nbyte8_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.byte1_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.byte2_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.byte3_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.byte4_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.byte8_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.real9_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.real18_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.str1_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.str2_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.str4_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.time_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.chain_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.enum_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.settype_access)</h3>
<p>
The second set of 20 functions are similar, but rather than referring to the column
position with a numeric index, it accepts a String which must match of the column
names of the result set.
</p>
<br/>
<p class="caption">See {{ page.query_select }} for a usage example.</p>
<br/>
<p>{{ page.supported_stmts }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.stmt_column_native_type }}</li>
    <li>{{ page.fetch_bound }}</li>
    <li>{{ page.fetch_all }}</li>
    <li>{{ page.fetch_next }}</li>
    <li>{{ page.query_select }}</li>
    <li>{{ page.query }}</li>
  </ul>
</div>
