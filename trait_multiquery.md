---
title: MultiQuery Trait
---

<div class="leftside">
<h3>Boolean function<br/>
AdaBase.Driver.Base.[DB].trait_multiquery_enabled ()</h3>
<p>This is a connection attribute.  It returns True if the driver is
configured to accept multiple queries separated by semicolons in the
query string.  This feature is not supported by all drivers.  For the
MySQL driver, this attribute is <b>False</b> by default.</p>
<br/>
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].set_trait_multiquery_enabled (trait : Boolean)</h3>
<p>This procedure is used to set the multiquery support.  It can be set
anytime (before or after establishing the connection).</p>
<br/>
<p class="caption">See {{ page.trait_compressed }} for a usage example.</p>
<p class="caption">TBW: Reference example of multiquery support</p>
<br/>
<p>This trait is limited to specific drivers.  Specially [DB] is limited to "MySQL"</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.trait_client }}</li>
    <li>{{ page.trait_server }}</li>
    <li>{{ page.trait_driver }}</li>
    <li>{{ page.trait_autocommit }}</li>
    <li>{{ page.trait_column_case}}</li>
    <li>{{ page.trait_error_mode }}</li>
    <li>{{ page.trait_blob_size }}</li>
    <li>{{ page.trait_compressed }}</li>
    <li>{{ page.trait_buffers }}</li>
  </ul>
</div>
