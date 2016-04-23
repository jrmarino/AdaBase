---
title: Use Query Buffers Trait
---

<div class="leftside">
<h3>Boolean function<br/>
AdaBase.Driver.Base.[DB].trait_query_buffers_used ()</h3>
<p>This is a connection attribute.  It returns True if the driver is
configured to fetch the entire result set and store it in memory.
This feature is not supported by all drivers.  For the
MySQL driver, this attribute is <b>True</b> by default.</p>
<br/>
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].set_trait_query_buffers_used (trait : Boolean)</h3>
<p>This procedure is used to configure the use of result buffers.  It can be
set anytime (before or after establishing the connection).</p>
<br/>
<p>Some client libraries have the ability to store the entire result (all
rows) from the database server and release the resource.  This is known as
buffering, but this performance improvement can potentially consume a huge
amount of memory, so the application should likely disable buffering if the
result set is expected to be enormous. When buffering is off, the server
waits for the client to partially fetch the remaining data and this can
take several round trips within the protocol.</p>
<br/>
<p class="caption">See {{ page.trait_compressed }} for a usage example.</p>
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
    <li>{{ page.trait_multiquery }}</li>
  </ul>
</div>
