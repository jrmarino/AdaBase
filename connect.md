---
title: Connect to database
---

<div class="leftside">
<pre class="code">
package AdaBase is
   subtype PosixPort is Natural range 0 .. 65535;
   portless : constant PosixPort := 0;
end AdaBase;
</pre>
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].basic_connect (database,
username : String := "",
password : String := "",
hostname : String := "";
socket   : String := "";
port : PosixPort := portless)</h3>
<p>This procedure initiates a persistent connection to the database server.
The connection can be explicitly closed using the <b>disconnect</b> procedure,
and it will be automatically closed if the driver object is destroyed.
Attempting to open a connecton when one is already open will be ignored
and the attempt logged.  All connection failures will also be logged, followed
by an exception which cannot be ignored.</p>
<p>The UNIX socket and TCP/IP port options are mutually exclusive;
if you set one, it's not necessary to set the other (the socket takes
priority if both are set).  The only required entry is the database name
(which is a file location for SQLite), although most entries are required
for drivers that interact with true servers.
<pre class="code">
--  Used for all testcases for MySQL driver
--  DR is a variable of type AdaBase.Driver.Base.MySQL.MySQL_Driver

package body Connect is

   procedure connect_database is
   begin
      DR.basic_connect (database => "adabase_examples",
                        username => "root",
                        password => "",
                        hostname => "localhost",
                        port     => 3306);
   end connect_database;

end Connect;
</pre>
<p class="caption">See {{ page.execute }} for a usage example coupled with code above.</p>
<br/>
<p>{{ page.supported_drivers }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.disconnect }}</li>
    <li>{{ page.execute }}</li>
    <li>{{ page.query }}</li>
    <li>{{ page.commit }}</li>
    <li>{{ page.rollback }}</li>
  </ul>
</div>
