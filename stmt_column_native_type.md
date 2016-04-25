---
title: Native data type of field
---

<div class="leftside">
<pre class="code">
package AdaBase is
  type field_types is (ft_nbyte0, ft_nbyte1, ft_nbyte2, ft_nbyte3, ft_nbyte4,
                        ft_nbyte8, ft_byte1, ft_byte2, ft_byte3, ft_byte4,
                        ft_byte8, ft_real9, ft_real18, ft_textual,
                        ft_widetext, ft_supertext, ft_timestamp,
                        ft_chain, ft_enumtype, ft_settype);
end AdaBase;
</pre>
<h3>field_types function<br/>
AdaBase.Statement.Base.[STMT].column_native_type (index : Positive)</h3>
<p>
This function returns the native type of the data that populates the
given column.  For example, if the second column of the result set
is the equivalent of a double float, then the function will return the
<i>ft_real18</i> enumeration given the <i>index</i> of 2.
</p>
<br/>
<p class="caption">See {{ page.fetch_next }} for a usage example.</p>
<br/>
<p>{{ page.supported_stmts }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.stmt_column_count }}</li>
    <li>{{ page.stmt_column_table }}</li>
    <li>{{ page.stmt_column_name }}</li>
    <li>{{ page.res_column }}</li>
  </ul>
</div>
