-- Setup telegraf_monitor user with proper permissions
-- Run this as a database superuser or the owner of cexplorer database

-- Create user if it doesn't exist
DO $$
BEGIN
  IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname = 'telegraf_monitor') THEN
    CREATE USER telegraf_monitor;
    RAISE NOTICE 'Created user: telegraf_monitor';
  ELSE
    RAISE NOTICE 'User telegraf_monitor already exists';
  END IF;
END
$$;

-- Enable pg_stat_statements extension for query performance tracking
-- This requires shared_preload_libraries='pg_stat_statements' in postgresql.conf
-- If you get an error, add to postgresql.conf and restart PostgreSQL
CREATE EXTENSION IF NOT EXISTS pg_stat_statements;

-- Grant pg_monitor role (gives access to pg_stat_* views)
GRANT pg_monitor TO telegraf_monitor;

-- Grant access to pg_stat_statements view
GRANT SELECT ON pg_stat_statements TO telegraf_monitor;

-- Grant access to all tables in public schema (cardano-db-sync tables)
GRANT SELECT ON ALL TABLES IN SCHEMA public TO telegraf_monitor;

-- Grant access to all sequences (needed for some table metadata queries)
GRANT SELECT ON ALL SEQUENCES IN SCHEMA public TO telegraf_monitor;

-- Make the grants permanent for future tables
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO telegraf_monitor;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON SEQUENCES TO telegraf_monitor;

-- Verify permissions
\echo 'Setup complete!'
\echo 'Checking sample permissions for telegraf_monitor...'
SELECT 
  schemaname,
  tablename,
  has_table_privilege('telegraf_monitor', schemaname||'.'||tablename, 'SELECT') as can_select
FROM pg_tables
WHERE schemaname = 'public'
  AND tablename IN ('block', 'tx', 'epoch_sync_time', 'tx_out', 'tx_in')
ORDER BY tablename;
