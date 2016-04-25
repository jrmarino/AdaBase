---
title: Query (literal input)
---

<div class="leftside">
<h3>AdaBase.Statement.Base.[STMT]_access function<br/>
AdaBase.Driver.Base.[DB].query (sql : String)</h3>
<p>This function executes the literal SQL string it is given and returns
access to the statement object that contains the result set (if any).</p>
<p>For queries that require issuing multiple times, there is much better
performance if the <b>prepare</b> function is used which returns access to
a prepared statement object instead.</p>
<p>This call may fail if the previous query was incompletely fetched and
the resource is still open.  The <b>close_cursor</b> function should be
used to release the database resources associated the query in that case.</p>
<br/>
<p class="caption">See {{ page.fetch_next }} for a usage example.</p>
<br/>
<p>{{ page.supported_drivers }}</p>
<p>{{ page.supported_stmts }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.stmt_successful }}</li>
    <li>{{ page.stmt_close_cursor }}</li>
    <li>{{ page.prepare }}</li>
    <li>{{ page.prepare_select }}</li>
  </ul>
</div>
