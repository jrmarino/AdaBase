---
title: Indication of discarded data
---

<div class="leftside">
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].data_discarded ()</h3>
<p>
After a new query, this function returns False until the <b>discard_rest</b>
procedure is executed.  That procedure releases the result resource.  If it
was still being used at the time it was released, the <b>data_discarded</b>
function returns True until it can be reset by the next query.
</p>
<br/>
<p class="caption">See {{ page.fetch_next }} for a usage example.</p>
<br/>
<p>{{ page.supported_stmts }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.stmt_discard_rest }}</li>
    <li>{{ page.stmt_query }}</li>
    <li>{{ page.query }}</li>
  </ul>
</div>
