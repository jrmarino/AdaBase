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
This page documents 40 similar bind functions, two for each standard data type.
These functions bind to the columns of fetched data rows.  The first 20 functions
reference the column by its numeric index, and the last 20 functions reference
the column through an associative array of the column's name.
</p>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.NByte0_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.NByte1_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.NByte2_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.NByte3_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.NByte4_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.NByte8_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.Byte1_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.Byte2_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.Byte3_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.Byte4_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.Byte8_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.Real9_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.Real18_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.Str1_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.Str2_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.Str4_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.Time_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.Chain_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.Enum_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (index : Positive; vaxx : AR.Settype_Access)</h3>
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
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.NByte0_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.NByte1_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.NByte2_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.NByte3_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.NByte4_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.NByte8_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.Byte1_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.Byte2_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.Byte3_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.Byte4_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.Byte8_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.Real9_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.Real18_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.Str1_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.Str2_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.Str4_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.Time_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.Chain_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.Enum_Access)</h3>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].bind (heading : String; vaxx : AR.Settype_Access)</h3>
<p>
The second set of 20 functions are similar, but rather than referring to the column
position with a numeric index, it accepts a String which must match of the column
names of the result set.
</p>
<br/>
<p class="caption">See {{ page.query_select }} and {{ page.stmt_iterate }}
for usage examples.</p>
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
