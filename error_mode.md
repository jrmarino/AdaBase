---
title: Error Mode Settings
---

<div class="leftside">
<pre class="code">
package AdaBase is
   type ErrorMode is (silent, warning, raise_exception);
   ERRMODE_EXCEPTION : exception;
end AdaBase;
</pre>
<h3>ErrorMode function<br/>
AdaBase.Driver.Base.[DB].trait_error_mode ()</h3>
<p>This is a connection attribute.  It returns the driver setting
dictating how errors are handled when encountered.  The default is
<b>warning</b> which means error messages will be logged by any active
loggers, but processing will continue.  If this attribute is set to
<b>silent</b> then nothing will be logged and processing will continue.
In either case, the application has to be designed to handle the
associated failed results, otherwise runtime failures will soon follow.
The final value of <b>raise_exception</b> will cause the
AdaBase.ERRMODE_EXCEPTION exception to be raised whenever an error is
encountered.</p>
<br/>
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].set_trait_error_mode (trait : ErrorMode)</h3>
<p>This procedure is used to set the error mode.  It can be set
anytime.</p>
<br/>
<p class="caption">See {{ page.trait_client }} and {{ page.last_driver_msg }}
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
