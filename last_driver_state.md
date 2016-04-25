---
title: Last Driver SQL State
---

<div class="leftside">
<pre class="code">
package AdaBase is
   subtype TSqlState is String (1 .. 5);
end AdaBase;
</pre>
<h3>TSqlState function<br/>
AdaBase.Driver.Base.[DB].last_sql_state ()</h3>
<p>This function returns the SQLSTATE error code regarding the last
operation performed by the database handle.  The SQLSTATE is a
5-character alphanumeric identifier defined in the ANSI SQL standard.</p>
<br/>
<h3>Boolean function<br/>
AdaBase.Statement.Base.[STMT].last_sql_state ()</h3>
<p>This function returns the SQLSTATE error code regarding the last
operation performed by the statement handle.</p>
</br>
<p class="caption">See {{ page.driver_msg }} and {{ page.p_discard_rest }}
for usage examples.</p>
<br/>
<p>{{ page.supported_drivers }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.driver_msg }}</li>
    <li>{{ page.driver_code }}</li>
  </ul>
</div>
