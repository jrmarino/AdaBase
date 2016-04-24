---
title: AdaBase by jrmarino
---

<h3>Thick database bindings to MySQL, PostgreSQL, and SQLite for Ada</h3>
{{ page.introduction }}
<br/>
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
