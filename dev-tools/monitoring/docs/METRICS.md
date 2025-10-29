# Cardano DB Sync - Available Metrics

This document describes the metrics exposed by the monitoring suite for cardano-db-sync.

## Metric Sources

### 1. PostgreSQL Metrics (postgres_exporter)

Exposed on port **9187** via `postgres_exporter`.

#### Database Size Metrics
- `pg_database_size_bytes{datname="cexplorer"}` - Total database size in bytes

#### Connection Metrics
- `pg_stat_database_numbackends` - Number of active connections
- `pg_stat_database_xact_commit` - Total transactions committed
- `pg_stat_database_xact_rollback` - Total transactions rolled back
- `pg_stat_database_deadlocks` - Number of deadlocks detected

#### Table Metrics
- `pg_stat_user_tables_seq_scan` - Sequential scans on tables
- `pg_stat_user_tables_idx_scan` - Index scans on tables
- `pg_stat_user_tables_n_tup_ins` - Rows inserted
- `pg_stat_user_tables_n_tup_upd` - Rows updated
- `pg_stat_user_tables_n_tup_del` - Rows deleted
- `pg_stat_user_tables_n_live_tup` - Estimated live rows
- `pg_stat_user_tables_n_dead_tup` - Estimated dead rows

#### Index Metrics
- `pg_stat_user_indexes_idx_scan` - Index scans performed
- `pg_stat_user_indexes_idx_tup_read` - Index entries returned
- `pg_stat_user_indexes_idx_tup_fetch` - Live rows fetched by index scans

#### Transaction/WAL Metrics
- `pg_stat_database_blks_read` - Disk blocks read
- `pg_stat_database_blks_hit` - Disk blocks found in cache (buffer hit)
- `pg_stat_database_tup_returned` - Rows returned by queries
- `pg_stat_database_tup_fetched` - Rows fetched by queries
- `pg_stat_database_tup_inserted` - Rows inserted
- `pg_stat_database_tup_updated` - Rows updated
- `pg_stat_database_tup_deleted` - Rows deleted

#### Replication Metrics (if applicable)
- `pg_stat_replication_lag` - Replication lag in bytes

### 2. System Metrics (node_exporter)

Exposed on port **9100** via `node_exporter`.

#### CPU Metrics
- `node_cpu_seconds_total` - CPU time spent in various modes (user, system, idle, etc.)
- `node_load1`, `node_load5`, `node_load15` - System load averages

#### Memory Metrics
- `node_memory_MemTotal_bytes` - Total memory
- `node_memory_MemFree_bytes` - Free memory
- `node_memory_MemAvailable_bytes` - Available memory
- `node_memory_Buffers_bytes` - Memory used for buffers
- `node_memory_Cached_bytes` - Memory used for cache
- `node_memory_SwapTotal_bytes` - Total swap space
- `node_memory_SwapFree_bytes` - Free swap space

#### Disk Metrics
- `node_disk_read_bytes_total` - Total bytes read from disk
- `node_disk_written_bytes_total` - Total bytes written to disk
- `node_disk_read_time_seconds_total` - Time spent reading
- `node_disk_write_time_seconds_total` - Time spent writing
- `node_filesystem_size_bytes` - Filesystem size
- `node_filesystem_avail_bytes` - Filesystem space available
- `node_filesystem_free_bytes` - Filesystem space free

#### Network Metrics
- `node_network_receive_bytes_total` - Network bytes received
- `node_network_transmit_bytes_total` - Network bytes transmitted
- `node_network_receive_errs_total` - Network receive errors
- `node_network_transmit_errs_total` - Network transmit errors

### 3. Cardano DB Sync Application Metrics

Exposed on port **8080** via Prometheus endpoint in cardano-db-sync.

**Status**: To be implemented when Prometheus endpoint is added to cardano-db-sync.

#### Planned Sync Metrics
- `dbsync_block_height` - Current synced block height
- `dbsync_slot_number` - Current synced slot number
- `dbsync_epoch_number` - Current synced epoch number
- `dbsync_sync_progress_percent` - Sync progress percentage
- `dbsync_blocks_per_second` - Block processing rate
- `dbsync_tx_per_second` - Transaction processing rate
- `dbsync_rollback_count` - Number of rollbacks performed
- `dbsync_rollback_depth` - Depth of most recent rollback

#### Planned Database Operation Metrics
- `dbsync_db_insert_duration_seconds` - Histogram of insert operation durations
- `dbsync_db_query_duration_seconds` - Histogram of query operation durations
- `dbsync_db_bulk_insert_size` - Size of bulk insert batches
- `dbsync_grouped_data_flush_duration_seconds` - Time spent flushing grouped data
- `dbsync_grouped_data_size_bytes` - Size of grouped data in memory

#### Planned Cache Metrics
- `dbsync_cache_hits_total` - Cache hits by cache type
- `dbsync_cache_misses_total` - Cache misses by cache type
- `dbsync_cache_size_entries` - Current cache size in entries
- `dbsync_cache_size_bytes` - Estimated cache size in bytes
- `dbsync_cache_evictions_total` - Number of cache evictions

#### Planned Memory/GC Metrics (from ghc-debug integration)
- `dbsync_memory_heap_size_bytes` - Current heap size
- `dbsync_memory_live_bytes` - Live data in heap
- `dbsync_memory_gc_count` - Number of GC collections
- `dbsync_memory_gc_cpu_seconds` - CPU time spent in GC
- `dbsync_memory_gc_wall_seconds` - Wall time spent in GC
- `dbsync_memory_max_live_bytes` - Maximum live data observed
- `dbsync_memory_allocated_bytes_total` - Total bytes allocated

#### Planned Ledger State Metrics
- `dbsync_ledger_state_size_bytes` - Size of ledger state
- `dbsync_ledger_snapshot_duration_seconds` - Time to save ledger snapshot
- `dbsync_ledger_events_processed_total` - Ledger events processed

## See Also

- [Prometheus Query Documentation](https://prometheus.io/docs/prometheus/latest/querying/basics/)
- [PostgreSQL Statistics Views](https://www.postgresql.org/docs/current/monitoring-stats.html)
- [Node Exporter Metrics](https://github.com/prometheus/node_exporter#enabled-by-default)
