---
title: Maximum BLOB Size Settings
---

<div class="leftside">
<pre class="code">
package AdaBase is
   subtype BLOB_Maximum is Positive range 2 ** 12 .. 2 ** 30;
end AdaBase;
</pre>
<h3>BLOB_Maximum function<br/>
AdaBase.Driver.Base.[DB].trait_max_blob_size ()</h3>
<p>This is a connection attribute.  It returns the driver setting limiting
the maximum size of Binary Large OBjects (BLOB).  If the contents of a binary
data field exceeds this limit, the data will be truncated.  The default value
for this setting is 4096 (4 kb) so this attribute must be updated if the
data can possibly be larger this this.  Currently the maximum BLOB that AdaBase
can handle is one gigabyte.</p>
<br/>
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].set_trait_max_blob_size (trait : BLOB_Maximum)</h3>
<p>This procedure is used to set the maximum BLOB size. It can be set
anytime.</p>
<br/>
<p class="caption">See {{ page.trait_client }} for a usage example.</p>
<br/>
<p>{{ page.supported_drivers }}</p>
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
    <li>{{ page.trait_compressed }}</li>
    <li>{{ page.trait_multiquery }}</li>
    <li>{{ page.trait_buffers }}</li>
  </ul>
</div>
