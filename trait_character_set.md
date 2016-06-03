---
title: Set Character encoding
---

<div class="leftside">
<h3>String function<br/>
AdaBase.Driver.Base.[DB].trait_character_set ()</h3>
<p>
This is a connection attribute.  It queries the connection directly to
get the client-side encoding.  The encoding is forced to UTF-8 by default,
unless its overridden by <b>set_trait_character_set</b> before the connection
is made.
</p>
<br/>
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].set_trait_character_site (trait : String)</h3>
<p>This procedure is used to set the client-side character encoding. It can only
be set prior to the connection being established.  If the <i>trait</i> argument
is an empty string, then the connection will not be forced to UTF-8 and the result
is that the encoding will mirror the database default.  This can be checked using
the <b>trait_character_set</b> which queries the value in real time.  Most of the
<i>trait</i> encoding values between PostgreSQL and MySQL will be the same, but
some may be particular to the server.  For SQLite, the encoding cannot by set.
Any trait value other than "UTF8" or an empty string will result in an exception
being thrown.</p>
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
    <li>{{ page.trait_blob_size }}</li>
    <li>{{ page.trait_column_case}}</li>
    <li>{{ page.trait_error_mode }}</li>
    <li>{{ page.trait_multiquery }}</li>
    <li>{{ page.trait_compressed }}</li>
    <li>{{ page.trait_buffers }}</li>
  </ul>
</div>
