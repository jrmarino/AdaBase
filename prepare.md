---
title: Prepare (literal input)
---

<div class="leftside">
<h3>AdaBase.Statement.Base.[STMT]_access function<br/>
AdaBase.Driver.Base.[DB].prepare (sql : String)</h3>
<p>This function executes the literal SQL string it is given and returns
access to the statement object that hasn't yet been executed.  The user
has the option to bind values and variables to the templated SQL prior
to the query execution.  This is allowed when the SQL has one or more
named (:<i>name</i>) or question mark (?) parameters in the string,
and both types can be present as the named parameters are internally
converted the question marks in the same order.  The named parameters
can only be used in one location though.
</p>
<p>
One the statement has been prepared, it can be executed repeatedly after
updating the parameter bindings before each execution.  THis is a big
performance improvement over executing similar statements directly
and individually.  Another major benefit is that SQL injection attacks
are prevented by using parameter bindings instead of passing text strings.
</p>
<br/>
<p class="caption">See {{ page.prepare_select }} for a usage example.</p>
<br/>
<p>{{ page.supported_drivers }}</p>
<p>{{ page.supported_stmts }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.stmt_successful }}</li>
    <li>{{ page.stmt_discard_rest }}</li>
    <li>{{ page.query }}</li>
    <li>{{ page.query_select }}</li>  </ul>
    <li>{{ page.prepare_select }}</li>
</div>
