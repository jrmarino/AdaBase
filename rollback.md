---
title: Rollback
---

<div class="leftside">
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].rollback ()</h3>
<p>This procedure rolls back an ongoing transaction.  It will fail if
there is no active transaction, if autocommit mode is active, or if
no connection to the database has yet been made.  There is no
explicit "begin transaction" command; a transaction starts when a
command is issued to the SQL server and autocommit is not set.</p>
<p>Some databases, including MySQL automatically issue an implicit
<b>commit</b> when a Database Definition Language (DDL) statement such as
"DROP TABLE" or "CREATE TABLE" is issued within a transaction.  The implicit
<b>commit</b> will prevent the rollback of the other changes within the
transaction boundary.</p>
<p class="caption">See {{ page.execute }} for a usage example.</p>
<br/>
<p>{{ page.supported_drivers }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.connect }}</li>
    <li>{{ page.disconnect }}</li>
    <li>{{ page.execute }}</li>
    <li>{{ page.query }}</li>
    <li>{{ page.commit }}</li>
  </ul>
</div>
