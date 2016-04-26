---
title: Bind variables to specific columns for Fetching
---

<div class="leftside">
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].fetch_bound ()</h3>
<p>
Preceded by explicit binding of columns to variables of the same native type,
this function automatically updates the values of the variables when executed.
This results in clean code within an iterative loop.  Depending on the scope
of the bound variables, the Unchecked_Access attribute may be required over
the safer Access to avoid a "non-local pointer cannot point to local object"
error.  The function returns False when no more data is available.
</p>
<br/>
<p class="caption">See {{ page.query_select }} for a usage example.</p>
<br/>
<p>{{ page.supported_stmts }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.stmt_bind }}</li>
    <li>{{ page.fetch_all }}</li>
    <li>{{ page.fetch_next }}</li>
    <li>{{ page.query_select }}</li>
    <li>{{ page.query }}</li>
  </ul>
</div>
