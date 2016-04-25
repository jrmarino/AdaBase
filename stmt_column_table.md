---
title: Table origin of field data
---

<div class="leftside">
<h3>String function<br/>
AdaBase.Statement.Base.[STMT].column_table (index : Positive)</h3>
<p>
This function returns the table origin of the data the populates the
given column.  For example, if the second column of the result set
is populated by the "last_name" column of the "people" table, then
the function will return "people" given an <i>index</i> of 2.
</p>
<br/>
<p class="caption">See {{ page.fetch_next }} for a usage example.</p>
<br/>
<p>{{ page.supported_stmts }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.stmt_column_count }}</li>
    <li>{{ page.stmt_column_name }}</li>
    <li>{{ page.stmt_column_native_type }}</li>
    <li>{{ page.res_column }}</li>
  </ul>
</div>
