---
title: Disconnect
---

<div class="leftside">
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].disconnect ()</h3>
<p>This procedure closes an active database connection.  It will log if
this is run when no connection to the database has yet been made.  When
the driver is destroyed, any active connection will automatically be
closed, so it's not strictly required to explicitly close a connection.</p>
<br/>
<p class="caption">See {{ page.execute }} for a usage example.</p>
<br/>
<p>{{ page.supported_drivers }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.connect }}</li>
    <li>{{ page.execute }}</li>
    <li>{{ page.query }}</li>
    <li>{{ page.commit }}</li>
    <li>{{ page.rollback }}</li>
  </ul>
</div>
