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
<li>{{ page.f_last_insert_id }}</li>
<li>{{ page.f_driver_msg }}</li>
<li>{{ page.f_driver_code }}</li>
<li>{{ page.f_driver_state }}</li>
</ul>

<h3>AdaBase.Driver (no result queries)</h3>
<ul>
<li>{{ page.p_query_clear_table }}</li>
<li>{{ page.p_query_drop_table }}</li>
</ul>

<h3>AdaBase.Driver (statement generation)</h3>
<ul>
<li>{{ page.f_prepare }}</li>
<li>{{ page.f_prepare_select }}</li>
<li>{{ page.f_query }}</li>
<li>{{ page.f_query_select }}</li>
</ul>

<h3>AdaBase.Driver (get attributes)</h3>
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
<li>{{ page.f_trait_multiquery }}</li>
<li>{{ page.f_trait_compressed }}</li>
<li>{{ page.f_trait_buffers }}</li>
</ul>

<h3>AdaBase.Driver (set attributes)</h3>
<ul>
<li>{{ page.p_trait_autocommit }}</li>
<li>{{ page.p_trait_column_case }}</li>
<li>{{ page.p_trait_error_mode }}</li>
<li>{{ page.p_trait_blob_size }}</li>
<li>{{ page.p_trait_multiquery }}</li>
<li>{{ page.p_trait_compressed }}</li>
<li>{{ page.p_trait_buffers }}</li>
</ul>

<h3>AdaBase.Driver (logger functionality)</h3>
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
<li>{{ page.f_stmt_affected }}</li>
<li>{{ page.f_rows_returned }}</li>
<li>{{ page.f_successful }}</li>
<li>{{ page.f_data_discarded }}</li>
<li>{{ page.f_column_count }}</li>
<li>{{ page.f_stmt_last_insert_id }}</li>
<li>{{ page.f_stmt_driver_state }}</li>
<li>{{ page.f_stmt_driver_code }}</li>
<li>{{ page.f_stmt_driver_msg }}</li>
<li>{{ page.p_discard_rest }}</li>
<li>{{ page.f_stmt_execute }}</li>
<li>{{ page.f_column_table }}</li>
<li>{{ page.f_column_name }}</li>
<li>{{ page.f_native_type }}</li>
<li>{{ page.f_fetch_next }}</li>
<li>{{ page.f_fetch_all }}</li>
<li>{{ page.f_fetch_bound }}</li>
<li>{{ page.f_fetch_next_set }}</li>
<li>{{ page.f_bind }}</li>
<li>{{ page.f_assign }}</li>
<li>{{ page.p_stmt_iterate }}</li>
</ul>

<h3>AdaBase.Results.Sets</h3>
<ul>
<li>{{ page.f_res_count }}</li>
<li>{{ page.f_column }}</li>
<li>{{ page.f_res_data_exhausted }}</li>
</ul>

<h3>AdaBase.Results.Field.std_field</h3>
<ul>
<li>{{ page.f_field_as_byte1 }}</li>
<li>{{ page.f_field_as_byte2 }}</li>
<li>{{ page.f_field_as_byte3 }}</li>
<li>{{ page.f_field_as_byte4 }}</li>
<li>{{ page.f_field_as_byte8 }}</li>
<li>{{ page.f_field_as_nbyte0 }}</li>
<li>{{ page.f_field_as_nbyte1 }}</li>
<li>{{ page.f_field_as_nbyte2 }}</li>
<li>{{ page.f_field_as_nbyte3 }}</li>
<li>{{ page.f_field_as_nbyte4 }}</li>
<li>{{ page.f_field_as_nbyte8 }}</li>
<li>{{ page.f_field_as_real9 }}</li>
<li>{{ page.f_field_as_real18 }}</li>
<li>{{ page.f_field_as_string }}</li>
<li>{{ page.f_field_as_wstring }}</li>
<li>{{ page.f_field_as_wwstring }}</li>
<li>{{ page.f_field_as_time }}</li>
<li>{{ page.f_field_as_chain }}</li>
<li>{{ page.f_field_as_enumtype }}</li>
<li>{{ page.f_field_as_settype }}</li>
<li>{{ page.f_field_is_null }}</li>
<li>{{ page.f_field_native_type }}</li>
<li>{{ page.f_spawn_field }}</li>
</ul>

</div>
