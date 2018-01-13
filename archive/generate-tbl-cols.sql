-- launch the script with a parameter USER_NAME e.g. IDTV_37_DPI_DEV_OFFAIR
set heading off
set feedback off
set linesize 1000
set verify off
spool temp-all-tbl.sql
select 'set linesize 150' from dual;
select 'spool temp-all-tbl-cols.sql' from dual;
select 'select ''--' || upper(table_name) || ''' from dual;' || chr(10) || 'select column_name, data_type, data_length from all_tab_columns where lower(owner) = lower(''&1'') and upper(table_name) = ''' || upper(table_name) || ''' order by column_name;' from all_tables where lower(owner) = lower('&1') and upper(table_name) like '%DT3%';
select 'exit;' from dual;
spool off;
@temp-all-tbl.sql;
exit;
