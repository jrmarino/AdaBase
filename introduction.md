---
title: Introduction to AdaBase
---

<h3>Thick database bindings to MySQL, PostgreSQL, and SQLite for Ada</h3>
<p>
There are several options for Ada enthusiasts to connect their programs
to databases.  What makes AdaBase special?</p>
<ol>
<li>It doesn't come with the kitchen sink.  Several of the database
bindings are part of large library with many unneeded components presented
in a "take it or leave it" fashion.  AdaBase is only concerned with providing
drivers to databases, and it can be built in a modular fashion to only
support databases of interest, resulting in a lean library for the developer.
</li>
<li>
It has been released on a developer- and commerce-friendly
license (ISC).
</li>
<li>
It's a thick binding.  Due to the differences of each database's
dialect, it's practically difficult to write SQL code where the program can
have interchangeable database backends, but it's possible with AdaBase which
can assembly SQL targetting specific databases.
</li>
<li>
Good documentation (You're reading it now) using real examples that
are present in the repository.  The database dumps are also provided so
developers can build and run the test cases.
</li>
<li>
Extensible.  With interest, other drivers can be added and existing
drivers can be extended.  For abstraction purposes, it's ideal that functions
and procedures apply to all drivers, but should a database client library
provide a valuable feature, that feature can be supported at the price of
locking the program to that database backend.
</li>
<li>
Familiar.  AdaBase was inspired in part by PHP's PDO classes and the
resemblence should be apparent to those familiar with using database with
PHP.
</li>
<li>
Consistent.  Through the enforcement of interfaces, each database driver
and statement handler behaves identically regardless of which database is
being used.
</li>
<li>
Open: It's been hosted on GitHub to encourage visibilities and contributions
from Ada enthusiasts.
</li>
</ol>
<br/>
<a href="index.html">Go back to Index</a>
