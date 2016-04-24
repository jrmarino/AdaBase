---
title: AdaBase by jrmarino
---

<h3>Thick database bindings to MySQL, PostgreSQL, and SQLite for Ada</h3>
<p>
There are several options for Ada enthusiasts to connect their programs
to databases.  What makes AdaBase special?</p>
<p>One: It doesn't come with the kitchen sink.  Several of the database
bindings are part of large library with many unneeded components presented
in a "take it or leave it" fashion.  AdaBase is only concerned with providing
drivers to databases, and it can be built in a modular fashion to only
support databases of interest, resulting in a lean library for the developer.
</p>
<p>
Two: It has been released on a developer- and commerce-friendly
license (ISC).
</p>
<p>
Three: It's a thick binding.  Due to the differences of each database's
dialect, it's practically difficult to write SQL code where the program can
have interchangeable database backends, but it's possible with AdaBase which
can assembly SQL targetting specific databases.
</p>
<p>
Four: Good documentation (You're reading it now) using real examples that
are present in the repository.  The database dumps are also provided so
developers can build and run the test cases.
</p>
<p>
Five: Extensible.  With interest, other drivers can be added and existing
drivers can be extended.  For abstraction purposes, it's ideal that functions
and procedures apply to all drivers, but should a database client library
provide a valuable feature, that feature can be supported at the price of
locking the program to that database backend.
</p>
<p>
Six: Familiar.  AdaBase was inspired in part by PHP's PDO classes and the
resemblence should be apparent to those familiar with using database with
PHP.
</p>
<p>
Seven: Consistent.  Through the enforcement of interfaces, each database driver
and statement handler behaves identically regardless of which database is
being used.
</p>
<p>
Open: It's been hosted on GitHub to encourage visibilities and contributions
from Ada enthusiasts.
</p>
<br/>

<div class="twocol">
<h3>AdaBase.Driver (core)</h3>
<ul>
<li>{{ page.p_connect }}</li>
<li>{{ page.p_disconnect }}</li>
<li>{{ page.p_execute }}</li>
<li>{{ page.p_commit }}</li>
<li>{{ page.p_rollback }}</li>
<li>{{ page.f_driver_msg }}</li>
<li>{{ page.f_driver_code }}</li>
<li>{{ page.f_driver_state }}</li>
</ul>

<h3>AdaBase.Driver (Abstract SQL)</h3>
<ul>
<li>{{ page.p_query_clear_table }}</li>
<li>{{ page.p_query_drop_table }}</li>
</ul>

<h3>AdaBase.Driver (statement generation)</h3>
<ul>
<li>{{ page.f_query }}</li>
</ul>

<h3>AdaBase.Driver (Get Attributes)</h3>
<ul>
<li>{{ page.f_trait_client_info }}</li>
<li>{{ page.f_trait_client_version }}</li>
<li>{{ page.f_trait_server_info }}</li>
<li>{{ page.f_trait_server_version }}</li>
<li>{{ page.f_trait_driver }}</li>
<li>{{ page.f_trait_autocommit }}</li>
<li>{{ page.f_trait_column_case }}</li>
<li>{{ page.f_trait_error_mode }}</li>
<li>{{ page.f_trait_blob_size }}</li>
<li>{{ page.f_trait_compressed }}</li>
<li>{{ page.f_trait_multiquery }}</li>
<li>{{ page.f_trait_buffers }}</li>
</ul>

<h3>AdaBase.Driver (Set Attributes)</h3>
<ul>
<li>{{ page.p_trait_autocommit }}</li>
<li>{{ page.p_trait_column_case }}</li>
<li>{{ page.p_trait_error_mode }}</li>
<li>{{ page.p_trait_blob_size }}</li>
<li>{{ page.p_trait_compressed }}</li>
<li>{{ page.p_trait_multiquery }}</li>
<li>{{ page.p_trait_buffers }}</li>
</ul>

<h3>AdaBase.Driver (Logger functionality)</h3>
<ul>
<li>{{ page.p_standard_logger }}</li>
<li>{{ page.p_logger_filename }}</li>
<li>{{ page.p_attach_logger }}</li>
<li>{{ page.p_detach_logger }}</li>
</ul>

</div>

<div class="twocol">
<h3>AdaBase.Statement</h3>
<ul>
<li>func rows_affected</li>
<li>func rows_returned </li>
<li>func successful </li>
<li>func discards_possible </li>
<li>func column_count </li>
<li>func last_insert_id </li>
<li>func last_sql_state </li>
<li>func last_driver_code </li>
<li>func last_driver_message </li>
<li>proc discard_rest </li>
<li>proc execute (x2) </li>
<li>func column_name </li>
<li>func column_table </li>
<li>func column_native_type </li>
<li>func fetch_next </li>
<li>func fetch_all </li>
<li>func fetch_bound </li>
<li>## conversions transfer ##</li>
<li>proc bind (x20)</li>
<li>proc assign (x20)</li>
<li>proc fetch_next_set</li>
<li>proc iterate</li>
<li>proc iterate_bound</li>
</ul>

</div>
