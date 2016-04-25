---
title: Row fetching complete
---

<div class="leftside">
<pre class="code">
package AdaBase.Results.Sets is
   type DataRow is tagged limited private;
   type DataRow_Access is access all DataRow;
end AdaBase.Results.Sets;
</pre>
<h3>Boolean function<br/>
AdaBase.Results.Sets.complete (DRA : DataRow_Access)</h3>
<p>
This function returns True if <i>DRA</i> is null.  The intended use is
to execute <b>fetch_next</b> inside a loop and use <b>complete</b> to
check its result in order to exit the loop because that indicates all
available data has already been extracted from the result set.
</p>
<br/>
<p class="caption">See {{ page.fetch_next }} for a usage example.</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.fetch_next }}</li>
    <li>{{ page.stmt_query }}</li>
    <li>{{ page.query }}</li>
  </ul>
</div>
