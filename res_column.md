---
title: Result Column functions (field handlers)
---

<div class="leftside">
<pre class="code">
package AdaBase.Results.Field is
   type std_field is tagged private;
end AdaBase.Results.Field;

package AdaBase.Results.Sets is
   type DataRow is tagged limited private;
end AdaBase.Results.Sets;
</pre>
<h3>AdaBase.Results.Field.std_field function<br/>
AdaBase.Results.Sets.Datarow.column (index : Positive)</h3>
<p>
A DataRow object contains two variations of a <b>column</b> function.
The first accepts an <i>index</i> ranging from 1 to the number of
columns in the result set, and the second implements an associative
array using the name of the column as its hash.  The function returns
a std_field which can present the field data in up to 20 different ways.
</p>
<br/>
<h3>AdaBase.Results.Field.std_field function<br/>
AdaBase.Results.Sets.Datarow.column (heading : String)</h3>
<p>This the second variation that accepts a <i>heading</i> String instead
of a positive integer where the <i>heading</i> must be equal to one
of the column names.</p>
<br/>
<p class="caption">See {{ page.fetch_next }} for a usage example.</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.stmt_column_count }}</li>
    <li>{{ page.stmt_column_name }}</li>
    <li>{{ page.res_count }}</li>
    <li>{{ page.fetch_next }}</li>
    <li>{{ page.stmt_query }}</li>
    <li>{{ page.query }}</li>
  </ul>
</div>
