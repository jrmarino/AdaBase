---
title: Commit
---

<div class="leftside">
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].commit ()</h3>
<p>This procedure completes an ongoing transaction.  Errors will be logged
if this executed without an active database connection, or if the connection
is set to autocommit mode (which effectively prohibits transactions).</p>
<br/>
<p class="caption">See {{ page.custom_logger }} for a usage example.</p>
<br/>
<p>{{ page.supported_drivers }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.connect }}</li>
    <li>{{ page.trait_autocommit }}</li>
    <li>{{ page.execute }}</li>
    <li>{{ page.disconnect }}</li>
  </ul>
</div>
