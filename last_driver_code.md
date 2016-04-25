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
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].last_driver_code ()</h3>
<p>This function returns a driver-specific error code regarding the last
operation performed by the statement handle.</p>
</br>
<p class="caption">See {{ page.driver_msg }} and {{ page.p_discard_rest }}
for a usage examples.</p>
<br/>
<p>{{ page.supported_drivers }}</p>
<p>{{ page.supported_stmts }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.driver_msg }}</li>
    <li>{{ page.driver_state }}</li>
    <li>{{ page.stmt_driver_msg }}</li>
    <li>{{ page.stmt_driver_state }}</li>
  </ul>
</div>
