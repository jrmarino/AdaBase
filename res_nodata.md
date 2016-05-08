---
title: DataRow End-of-Data indicator
---

<div class="leftside">
<h3>Boolean function<br/>
AdaBase.Results.Sets.DataRow.data_exhausted ()</h3>
<p>
This function returns False when a row of data is successfully retrieved
from the current result set and True when no data was obtained (meaning the
last row of data has already been retrieved and no more rows are available).
This is the typical check to exit an iterative loop.
</p>
<br/>
<p class="caption">See {{ page.fetch_next }} for a usage example.</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.res_column }}</li>
    <li>{{ page.fetch_next }}</li>
    <li>{{ page.fetch_bound }}</li>
  </ul>
</div>
