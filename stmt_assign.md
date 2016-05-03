---
title: Assign values and variables to markers of Prepared Statements
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
This page documents 76 similar assign functions, four for each standard data type
(except for chain and settype which don't have the constant value versions).
These functions bind to the markers of a previously prepared statement.
</p>
<p>
The first 20 functions reference the marker by its numeric index starting from 1
and bind the marker to a variable matching the data type of the marker.
The next 20 functions do the same thing except they reference the marker by its name
(which requires the use of named parameters instead of question marks).
The values of the variables involved in these 40 functions are not evaluated until
the <b>execute</b> command is issues
</p>
<p>
The next 18 functions reference the marker by its numeric index and define its value
with a constant of the same data type of the marker.  There is no constant variation of
the function for chain and settype markers; those markers are limited to variables.
The final 18 functions are similar, but reference the marker by their names.
</p>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.nbyte0_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.nbyte1_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.nbyte2_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.nbyte3_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.nbyte4_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.nbyte8_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.byte1_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.byte2_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.byte3_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.byte4_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.byte8_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.real9_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.real18_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.str1_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.str2_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.str4_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.time_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.chain_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.enum_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.settype_access)</h3>
<p>
Prior to issuing the <b>execute</b> command of the statement object, the values of
the markers must be defined.  One method is to pass access to a variable of the same
type as the marker.  The first 20 of the 76 overloaded bind functions accept an index
starting with 1 that matches the column number of the result row.  The <i>vaxx</i>
argument accepts a pointer to one of the 20 native data type.
</p>
<p>Once a variable is assigned, the user can change the values of the variables before
each prepared statement execution; no further assignments are necessary past the first
time.  Depending on the scope of the bound variables, the Unchecked_Access attribute may
be required over the safer Access to avoid a "non-local pointer cannot point to local
object" error.
</p>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.nbyte0_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.nbyte1_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.nbyte2_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.nbyte3_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.nbyte4_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.nbyte8_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.byte1_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.byte2_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.byte3_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.byte4_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.byte8_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.real9_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.real18_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.str1_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.str2_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.str4_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.time_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.chain_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.enum_access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.settype_access)</h3>
<p>
The next set of 20 functions are similar, but rather than referring to the marker
position with a numeric index, it accepts a String which must match of the name of the
parameter defined in the original SQL string.  For example, if the SQL given to the
<b>prepare</b> function is "SELECT ALL * FROM fruits WHERE color = :color", the name
of the marker is "color".  It can be referred to by an index of 1 or its moniker "color".
</p>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.nbyte0)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.nbyte1)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.nbyte2)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.nbyte3)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.nbyte4)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.nbyte8)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.byte1)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.byte2)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.byte3)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.byte4)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.byte8)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.real9)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.real18)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.str1)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.str2)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.str4)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.time)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.enum)</h3>
<p>
These 18 functions assign values immediately to markers referenced by their numeric
index.  This option is not available for chain and settype markers.
</p>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.nbyte0)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.nbyte1)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.nbyte2)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.nbyte3)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.nbyte4)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.nbyte8)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.byte1)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.byte2)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.byte3)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.byte4)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.byte8)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.real9)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.real18)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.str1)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.str2)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.str4)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.time)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.enum)</h3>
<p>
These 18 functions assign values immediately to markers referenced by their names.
This option is not available for chain and settype markers.
</p>
<br/>
<p class="caption">See {{ page.prepare_select }} for a usage example.</p>
<br/>
<p>{{ page.supported_stmts }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.stmt_column_native_type }}</li>
    <li>{{ page.fetch_next }}</li>
    <li>{{ page.prepare_select }}</li>
    <li>{{ page.prepare }}</li>
  </ul>
</div>
