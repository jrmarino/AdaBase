---
title: Last Driver Code
---

<div class="leftside">
<pre class="code">
package AdaBase is
   subtype DriverCodes is Integer range -999 .. 1999;
end AdaBase;
</pre>
<h3>DriverCodes function<br/>
AdaBase.Driver.Base.[DB].last_driver_code ()</h3>
<p>This function returns a driver-specific error code regarding the last
operation performed by the database handle.</p>
<br/>
<p class="caption">See {{ page.driver_msg }} for a usage example.</p>
<br/>
<p>{{ page.supported_drivers }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.driver_msg }}</li>
    <li>{{ page.driver_state }}</li>
  </ul>
</div>
