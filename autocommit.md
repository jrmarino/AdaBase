---
title: Auto-commit Settings
---

<div class="leftside">
<h3>Boolean function<br/>
AdaBase.Driver.Base.[DB].trait_autocommit ()</h3>
<p>This is a connection attribute.  It returns True when the driver is
configured to commit queries immediately (e.g. one query transactions).
By default, explicit transactions are assumed, thus by default this
attribute is False.</p>
<br/>
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].set_trait_autocommit (trait : Boolean)</h3>
<p>This procedure is used to change the autocommit behavior.  There is
only a noticable effect when the trait setting is opposite of the
driver's current setting.  Passing True will ensure each query is
followed by a commit while passing False will ensure no query is
realized until a commit command is executed.</p>
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
    <li>{{ page.trait_column_case}}</li>
    <li>{{ page.trait_error_mode }}</li>
    <li>{{ page.trait_blob_size }}</li>
  </ul>
</div>
