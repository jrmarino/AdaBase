---
title: Last Insert ID (statement)
---

<div class="leftside">
<h3>Natural function<br/>
AdaBase.Statement.Base.[STMT].last_insert_id ()</h3>
<p>
This function returns the ID of the last inserted row as a result of
a prepared statement execute.  If the driver does not support the concept
of auto-increment fields, the return value will always be zero.
</p>
<p>
For the PostgreSQL driver, if the query is in the form of
INSERT INTO .. RETURNING, then the <b>last_insert_id</b> comes directly
from the result set of this INSERT statement.  If no RETURNING clause is
present, the <b>last_insert_id</b> is the result of "SELECT lastval()"
which is normally accurate, but not in every case.
</p>
<br/>
<p class="caption">See {{ page.prepare_select }} for a usage example.</p>
<br/>
<p>{{ page.supported_stmts }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.stmt_rows_affected }}</li>
    <li>{{ page.stmt_execute }}</li>
  </ul>
</div>
