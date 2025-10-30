# Cardano DB Sync - Monitoring Tool

A local development monitoring suite for cardano-db-sync using Prometheus, Grafana, and system exporters.

## Overview

This tool provides comprehensive monitoring for cardano-db-sync instances during development and testing:

- **PostgreSQL Metrics**: Database performance, query stats, connection metrics
- **System Metrics**: CPU, memory, disk I/O, network
- **Application Metrics**: Sync progress, throughput, cache performance (when implemented)

## Quick Start

### Prerequisites

Install required tools:

```bash
# macOS
brew install tmux prometheus postgres_exporter node_exporter grafana

# Linux (apt)
sudo apt-get install tmux prometheus postgres-exporter prometheus-node-exporter grafana

# Linux (yum)
sudo yum install tmux prometheus postgres_exporter node_exporter grafana
```

**Need detailed installation instructions?**
- **Grafana**: https://grafana.com/docs/grafana/latest/setup-grafana/installation/
- **Prometheus**: https://prometheus.io/docs/prometheus/latest/installation/
- **postgres_exporter**: https://github.com/prometheus-community/postgres_exporter#installation
- **node_exporter**: https://prometheus.io/docs/guides/node-exporter/

### Setup PostgreSQL Exporter User

Create a monitoring user in PostgreSQL:

```sql
-- Connect to your database
psql -U postgres -d cexplorer

-- Create monitoring user
CREATE USER postgres_exporter WITH PASSWORD 'secure_password';

-- Grant necessary permissions
GRANT pg_monitor TO postgres_exporter;
GRANT CONNECT ON DATABASE cexplorer TO postgres_exporter;

-- For PostgreSQL < 10, grant these instead:
-- GRANT SELECT ON pg_stat_database TO postgres_exporter;
```

### Start Monitoring

```bash
# From the repository root
cd dev-tools/monitoring

# Start the monitoring suite
./scripts/start-monitoring.sh
```

This will launch a tmux session with:
- **Pane 0**: postgres_exporter (PostgreSQL metrics)
- **Pane 1**: prometheus (metrics storage and querying)
- **Pane 2**: node_exporter (system metrics)
- **Pane 3**: Information and quick links

### Access Monitoring

- **Prometheus UI**: http://localhost:9090
- **Metrics Explorer**: http://localhost:9090/graph
- **Targets Status**: http://localhost:9090/targets

## Grafana Setup

### 1. Start Grafana

```bash
# macOS
brew services start grafana

# Linux (systemd)
sudo systemctl start grafana-server
```

### 2. Access Grafana

Open http://localhost:3000

Default credentials: `admin` / `admin` (you'll be prompted to change)

### 3. Add Prometheus Data Source

1. Click **Configuration** (gear icon) → **Data Sources**
2. Click **Add data source**
3. Select **Prometheus**
4. Configure:
   - **Name**: Prometheus
   - **URL**: `http://localhost:9090`
   - **Access**: Browser
5. Click **Save & Test**

### 4. Import Dashboard

1. Click **Dashboards** (squares icon) → **Import**
2. Click **Upload JSON file**
3. Select `dev-tools/monitoring/config/grafana-db-sync.json`
4. Select **Prometheus** as the data source
5. Click **Import**

The dashboard includes:
- **System metrics**: CPU usage, memory usage (from node_exporter)
- **PostgreSQL settings**: Shared buffers, work mem, max connections
- **Database stats**: Active/idle sessions, transactions, insert/update/delete rates
- **Performance**: Cache hit rate, buffer stats, checkpoint stats
- **Lock monitoring**: Lock tables and conflicts/deadlocks
- **Epoch sync duration**: Timeline of how long each epoch took to sync

### Quick Reference: Grafana Commands

**Start Grafana:**
```bash
# macOS
brew services start grafana

# Linux
sudo systemctl start grafana-server
```

**Stop Grafana:**
```bash
# macOS
brew services stop grafana

# Linux
sudo systemctl stop grafana-server
```

**Check Grafana status:**
```bash
# macOS
brew services list | grep grafana

# Linux
sudo systemctl status grafana-server
```

**Access:** http://localhost:3000 (default: admin/admin)

## Configuration

### Environment Variables

Customize the monitoring setup with environment variables:

```bash
# Database connection
export DB_HOST=127.0.0.1
export DB_PORT=5432
export DB_NAME=cexplorer
export DB_USER=postgres_exporter

# Exporter ports
export POSTGRES_EXPORTER_PORT=9187
export NODE_EXPORTER_PORT=9100
export PROMETHEUS_PORT=9090

# Then start
./scripts/start-monitoring.sh
```

### Prometheus Configuration

Edit `config/prometheus.yml` to:
- Add additional scrape targets
- Adjust scrape intervals
- Configure alerting rules

After changing, restart the monitoring suite:

```bash
tmux kill-session -t cardano-monitoring
./scripts/start-monitoring.sh
```

## Available Metrics

See [docs/METRICS.md](docs/METRICS.md) for a complete list of available metrics and useful queries.

### Key Metrics to Monitor

#### Database Performance
- `pg_stat_database_xact_commit` - Transaction commit rate
- `pg_stat_database_blks_hit` / `pg_stat_database_blks_read` - Buffer cache hit ratio
- `pg_database_size_bytes` - Database size growth

#### System Performance
- `node_cpu_seconds_total` - CPU usage
- `node_memory_MemAvailable_bytes` - Available memory
- `node_disk_read_bytes_total` / `node_disk_written_bytes_total` - Disk I/O

#### Application Performance (when implemented)
- `dbsync_block_height` - Current sync height
- `dbsync_blocks_per_second` - Sync throughput
- `dbsync_memory_heap_size_bytes` - Heap memory usage
- `dbsync_cache_hits_total` - Cache performance

## Common Workflows

### 1. Monitor Sync Performance

```promql
# Block processing rate (blocks/sec)
rate(dbsync_block_height[5m])

# Transaction throughput (tx/sec)
rate(dbsync_tx_per_second[5m])

# Database write rate (rows/sec)
rate(pg_stat_user_tables_n_tup_ins{relname="tx"}[5m])
```

### 2. Identify Memory Issues

```promql
# Memory usage percentage
100 - ((node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes) * 100)

# Heap growth rate (bytes/sec)
rate(dbsync_memory_heap_size_bytes[5m])

# GC overhead percentage
rate(dbsync_memory_gc_cpu_seconds[5m]) / rate(process_cpu_seconds_total[5m]) * 100
```

### 3. Check Database Health

```promql
# Buffer cache hit ratio (should be >99%)
rate(pg_stat_database_blks_hit[5m]) /
(rate(pg_stat_database_blks_hit[5m]) + rate(pg_stat_database_blks_read[5m]))

# Dead tuple percentage (high = need VACUUM)
pg_stat_user_tables_n_dead_tup /
(pg_stat_user_tables_n_live_tup + pg_stat_user_tables_n_dead_tup)

# Active connections
pg_stat_database_numbackends{datname="cexplorer"}
```

### 4. Analyze Disk I/O

```promql
# Total disk I/O (MB/s)
(rate(node_disk_read_bytes_total[5m]) + rate(node_disk_written_bytes_total[5m])) / 1024 / 1024

# Disk utilization percentage
100 - ((node_filesystem_avail_bytes / node_filesystem_size_bytes) * 100)
```

## Integration with Profiling

The monitoring tool complements the profiling tool:

1. **Start monitoring** to establish baseline metrics
2. **Start profiling** to capture memory snapshots
3. **Correlate findings**: Use Prometheus timestamps to match metrics with profiling snapshots
4. **Export profiling metrics** to Prometheus (future enhancement)

Example workflow:

```bash
# Terminal 1: Start monitoring
cd dev-tools/monitoring
./scripts/start-monitoring.sh

# Terminal 2: Start profiling
cd dev-tools/profiling
./scripts/start-profiling.sh

# Terminal 3: Connect ghc-debug after a few hours
ghc-debug-brick /tmp/cardano-db-sync.ghc-debug

# In Grafana: Note the timestamp when you take snapshots
# This allows correlation of heap snapshots with metric spikes
```

## Troubleshooting

### postgres_exporter fails to connect

**Problem**: `error: pq: password authentication failed`

**Solution**:
1. Create the postgres_exporter user (see Setup section)
2. Set `DATA_SOURCE_NAME` environment variable with correct credentials
3. Or edit `scripts/start-monitoring.sh` to use your credentials

### No data in Prometheus

**Problem**: Targets show as "DOWN" in http://localhost:9090/targets

**Solution**:
1. Check exporters are running: `ps aux | grep exporter`
2. Test endpoints manually:
   - `curl http://localhost:9187/metrics` (postgres_exporter)
   - `curl http://localhost:9100/metrics` (node_exporter)
3. Check Prometheus config: `cat config/prometheus.yml`
4. Restart monitoring suite

### Grafana shows "No Data"

**Problem**: Dashboard panels show "No data"

**Solution**:
1. Verify Prometheus data source is configured correctly
2. Check Prometheus has data: http://localhost:9090/graph
3. Verify time range in Grafana (top right)
4. Check dashboard panel queries match your metric names

### Port already in use

**Problem**: `bind: address already in use`

**Solution**:
```bash
# Find and kill processes using ports
lsof -ti:9090 | xargs kill  # Prometheus
lsof -ti:9100 | xargs kill  # node_exporter
lsof -ti:9187 | xargs kill  # postgres_exporter

# Or use different ports
export PROMETHEUS_PORT=9091
export NODE_EXPORTER_PORT=9101
export POSTGRES_EXPORTER_PORT=9188
./scripts/start-monitoring.sh
```

## Directory Structure

```
monitoring/
├── README.md                       # This file
├── scripts/
│   └── start-monitoring.sh         # Launch monitoring suite
├── config/
│   ├── prometheus.yml              # Prometheus configuration
│   └── grafana-db-sync.json        # Grafana dashboard for cardano-db-sync
├── data/                           # Prometheus TSDB (gitignored)
│   └── .gitignore
└── docs/
    └── METRICS.md                  # Available metrics reference
```

## Next Steps

1. **Import the Grafana dashboard** from `config/grafana-db-sync.json`
2. **Customize dashboard panels** as needed for your specific investigation
3. **Set up alerting** in Prometheus for critical metrics (see docs/METRICS.md)
4. **Export profiling data** to Prometheus for unified view (future enhancement)
5. **Create saved queries** in Prometheus for common investigations

## See Also

- [Profiling Tool](../profiling/README.md) - Memory profiling with ghc-debug
- [METRICS.md](docs/METRICS.md) - Complete metrics reference
- [Prometheus Documentation](https://prometheus.io/docs/)
- [Grafana Documentation](https://grafana.com/docs/)
- [postgres_exporter](https://github.com/prometheus-community/postgres_exporter)
- [node_exporter](https://github.com/prometheus/node_exporter)
