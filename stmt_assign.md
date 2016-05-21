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

   subtype Textual   is CT.Text;
   subtype Textwide  is SUW.Unbounded_Wide_String;
   subtype Textsuper is SUWW.Unbounded_Wide_Wide_String;

   -------------------------------------------
   --  Supported Field Types (Standardized) --
   -------------------------------------------

   type NByte1 is mod 2 ** 8;
   type NByte2 is mod 2 ** 16;
   type NByte3 is mod 2 ** 24;
   type NByte4 is mod 2 ** 32;
   type NByte8 is mod 2 ** 64;
   type Byte8  is range -2 ** 63 .. 2 ** 63 - 1;
   type Byte4  is range -2 ** 31 .. 2 ** 31 - 1;
   type Byte3  is range -2 ** 23 .. 2 ** 23 - 1;
   type Byte2  is range -2 ** 15 .. 2 ** 15 - 1;
   type Byte1  is range -2 **  7 .. 2 **  7 - 1;
   type Real9  is digits 9;
   type Real18 is digits 18;

   subtype NByte0 is Boolean;

   type Enumtype is record enumeration : Textual; end record;
   type Settype is array (Positive range <>) of Enumtype;
   type Chain is array (Positive range <>) of NByte1;

   type NByte0_Access  is access all NByte0;
   type NByte1_Access  is access all NByte1;
   type NByte2_Access  is access all NByte2;
   type NByte3_Access  is access all NByte3;
   type NByte4_Access  is access all NByte4;
   type NByte8_Access  is access all NByte8;
   type Byte1_Access   is access all Byte1;
   type Byte2_Access   is access all Byte2;
   type Byte3_Access   is access all Byte3;
   type Byte4_Access   is access all Byte4;
   type Byte8_Access   is access all Byte8;
   type Real9_Access   is access all Real9;
   type Real18_Access  is access all Real18;
   type Str1_Access    is access all Textual;
   type Str2_Access    is access all Textwide;
   type Str4_Access    is access all Textsuper;
   type Time_Access    is access all AC.Time;
   type Chain_Access   is access all Chain;
   type Enum_Access    is access all Enumtype;
   type Settype_Access is access all Settype;

end AdaBase.Results;
</pre>
<p>
This page documents 82 overloaded assign functions, four for each standard
data type and two additional ones for <i>String</i> variables which are
automatically converted to <i>textual</i> types.  These functions bind to the
markers of a previously prepared statement.
</p>
<p>
The first 20 functions reference the marker by its numeric index starting from 1
and bind the marker to a variable matching the data type of the marker.
The next 20 functions do the same thing except they reference the marker by its
name, which requires the use of named parameters instead of question marks.
The values of the variables involved in these 40 functions are not evaluated until
the <b>execute</b> command is issued.
</p>
<p>
The next 21 functions reference the marker by its numeric index and define its
value with a constant of the same data type of the marker.  The final 21
functions are similar, but reference the marker by their names.
</p>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.NByte0_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.NByte1_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.NByte2_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.NByte3_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.NByte4_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.NByte8_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Byte1_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Byte2_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Byte3_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Byte4_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Byte8_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Real9_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Real18_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Str1_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Str2_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Str4_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Time_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Chain_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Enum_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Settype_Access)</h3>
<p>
Prior to issuing the <b>execute</b> command of the statement object, the values of
the markers must be defined.  One method is to pass access to a variable of the same
type as the marker.  The first 20 of the 80 overloaded bind functions accept an index
starting with 1 that matches the column number of the result row.  The <i>vaxx</i>
argument accepts a pointer to one of the 20 native data type.  If the access type
assigned to <i>vaxx</i> is set to null, the driver will attempt to set the parameter
to NULL, e.g. insert NULL into a record's field rather than a value.
</p>
<p>Once a variable is assigned, the user can change the values of the variables before
each prepared statement execution; no further assignments are necessary past the first
time.  Depending on the scope of the bound variables, the Unchecked_Access attribute may
be required over the safer Access to avoid a "non-local pointer cannot point to local
object" error.
</p>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.NByte0_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.NByte1_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.NByte2_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.NByte3_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.NByte4_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.NByte8_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Byte1_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Byte2_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Byte3_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Byte4_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Byte8_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Real9_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Real18_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Str1_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Str2_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Str4_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Time_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Chain_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Enum_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Settype_Access)</h3>
<p>
The next set of 21 functions are similar, but rather than referring to the marker
position with a numeric index, it accepts a String which must match of the name of the
parameter defined in the original SQL string.  For example, if the SQL given to the
<b>prepare</b> function is "SELECT ALL * FROM fruits WHERE color = :color", the name
of the marker is "color".  It can be referred to by an index of 1 or its moniker "color".
</p>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.NByte0)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.NByte1)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.NByte2)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.NByte3)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.NByte4)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.NByte8)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Byte1)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Byte2)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Byte3)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Byte4)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Byte8)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Real9)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Real18)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : String)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Textual)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Textwide)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Textsuper)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : CAL.Time)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Chain)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Enum)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (index : Positive; vaxx : AR.Settype)</h3>
<p>
These 21 functions assign values immediately to markers referenced by their
numeric index.
</p>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.NByte0)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.NByte1)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.NByte2)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.NByte3)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.NByte4)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.NByte8)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Byte1)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Byte2)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Byte3)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Byte4)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Byte8)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Real9)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Real18)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : String)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Textual)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Textwide)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Textsuper)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : CAL.Time)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Chain)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Enum)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].assign (moniker : String; vaxx : AR.Settype)</h3>
<p>
These 20 functions assign values immediately to markers referenced by their
names.
</p>
<br/>
<p class="caption">See {{ page.prepare_select }} and {{ page.stmt_iterate }}
for a usage examples.</p>
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
