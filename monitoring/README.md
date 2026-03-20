# Cardano DB Sync - Monitoring

Monitor cardano-db-sync performance with Telegraf, Prometheus, and Grafana.

## Quick Start

### 1. Install Dependencies

**macOS:**
```bash
brew install zellij telegraf prometheus grafana
```

**Linux (Ubuntu/Debian):**
```bash
sudo apt-get update
sudo apt-get install zellij telegraf prometheus grafana
```

**Linux (Fedora/RHEL/CentOS):**
```bash
sudo yum install zellij telegraf prometheus grafana
```

Note: You may need to add package repositories first. See [Telegraf](https://docs.influxdata.com/telegraf/latest/install/), [Grafana](https://grafana.com/docs/grafana/latest/setup-grafana/installation/) docs for details.

### 2. Configure PostgreSQL

Enable `pg_stat_statements` in `postgresql.conf`:
```ini
shared_preload_libraries = 'pg_stat_statements'
```

Restart PostgreSQL and run setup:
```bash
brew services restart postgresql@14  # macOS
sudo systemctl restart postgresql    # Linux

psql -U postgres -d cexplorer -f monitoring/scripts/grant-telegraf-permissions.sql
```

### 3. Start Monitoring

```bash
./scripts/run-everything-zellij.sh
```

This starts cardano-node, cardano-db-sync, Telegraf, and Prometheus in a Zellij session.

### 4. Setup Grafana

Open http://localhost:3000 (admin/admin) and run:
```bash
./monitoring/scripts/setup-grafana.sh
```

Or manually add data sources (Prometheus at `:9090`, PostgreSQL at `:5432`) and import `monitoring/grafana/dashboards/cardano-db-sync-complete.json`.

## Troubleshooting

Run the verification script:
```bash
./monitoring/scripts/verify-monitoring.sh
```

**Common fixes:**
- Telegraf can't connect: Check `telegraf_monitor` user exists and `pg_hba.conf` allows local connections
- Prometheus targets down: Verify services are running with `ps aux | grep -E 'telegraf|prometheus|cardano-db-sync'`
- Port conflicts: `lsof -ti:9090 | xargs kill` (or :9273, :8080, :3000)

**Access:**
- Grafana: http://localhost:3000
- Prometheus: http://localhost:9090
- Metrics: http://localhost:9273/metrics (Telegraf), http://localhost:8080 (db-sync) 

**Resources:**
- [Telegraf](https://docs.influxdata.com/telegraf/latest/) | [Prometheus](https://prometheus.io/docs/) | [Grafana](https://grafana.com/docs/)
