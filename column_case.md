---
title: Column Case Settings
---

<div class="leftside">
<pre class="code">
package AdaBase is
   type Case_Modes is (lower_case, natural_case, upper_case);
end AdaBase;
</pre>
<h3>Case_Modes function<br/>
AdaBase.Driver.Base.[DB].trait_column_case ()</h3>
<p>This is a connection attribute.  It returns the driver setting
dictating how the column names are referenced.  The default is
<b>natural_case</b> which means the column names are returned and referenced
exactly as they are defined in the database. Alternatively, the column names
can be transformed to all capital letters (<b>upper_case</b>) or all lower
case letters (<b>lower_case</b>).</p>
<br/>
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].set_trait_column_case (trait : Case_Modes)</h3>
<p>This procedure is used to set the column case behavior.</p>
<br/>
<p class="caption">See {{ page.trait_client }} and {{ page.query_select }}
for usage examples.</p>
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
    <li>{{ page.trait_error_mode }}</li>
    <li>{{ page.trait_blob_size }}</li>
    <li>{{ page.trait_compressed }}</li>
    <li>{{ page.trait_multiquery }}</li>
    <li>{{ page.trait_buffers }}</li>
  </ul>
</div>
