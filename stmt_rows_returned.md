---
title: Rows returned with query
---

<div class="leftside">
<pre class="code">
package AdaBase is
   type TraxID          is mod 2 ** 64;
   subtype AffectedRows is TraxID;
end AdaBase;
</pre>
<h3>AffectedRows function<br/>
AdaBase.Statement.Base.[STMT].rows_returned ()</h3>
<p>
This function returns the number of rows in the result set. Support may vary
depending on driver.  For example, on MySQL, using this function results in an
exception if the connection was made with query buffering turned off, since
the value will be inaccurate in that case.  The SQLite driver always returns
zero because it can't determine the result size until all the rows have been
fetched.  An alternative for SQLite is to use the <b>fetch_all</b> function
and get the length of the resultant array of all rows in the result set.
</p>
<br/>
<p class="caption">See {{ page.fetch_next }} for a usage example.</p>
<br/>
<p>{{ page.supported_stmts }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.trait_buffers }}</li>
    <li>{{ page.query }}</li>
    <li>{{ page.stmt_query }}</li>
    <li>{{ page.fetch_all }}</li>
  </ul>
</div>