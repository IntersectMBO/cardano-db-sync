# Cardano DB Sync - Monitoring

Local development monitoring using Prometheus and Grafana for PostgreSQL and system metrics.

## Quick Setup

### 1. Install Tools

```bash
# macOS
brew install prometheus postgres_exporter node_exporter grafana

# Linux (apt)
sudo apt-get install prometheus postgres-exporter prometheus-node-exporter grafana

# Linux (yum)
sudo yum install prometheus postgres_exporter node_exporter grafana
```

### 2. Setup PostgreSQL Monitoring User

```sql
-- Connect to your database
psql -U postgres -d cexplorer

-- Create monitoring user
CREATE USER postgres_exporter WITH PASSWORD 'secure_password';
GRANT pg_monitor TO postgres_exporter;
GRANT CONNECT ON DATABASE cexplorer TO postgres_exporter;
```

### 3. Start Monitoring

```bash
cd dev-tools/monitoring
./scripts/start-monitoring.sh
```

This launches a tmux session with Prometheus and exporters.

### 4. Start Grafana

```bash
# macOS
brew services start grafana

# Linux
sudo systemctl start grafana-server
```

## Access Dashboards

- **Prometheus**: http://localhost:9090
- **Grafana**: http://localhost:3000 (default: admin/admin)

## Grafana Setup

1. Open http://localhost:3000
2. Add Prometheus data source:
   - Configuration → Data Sources → Add data source
   - Select Prometheus
   - URL: `http://localhost:9090`
   - Save & Test
3. Import dashboard:
   - Dashboards → Import
   - Upload `config/grafana-db-sync.json`
   - Select Prometheus data source

## Key Metrics

- **System**: CPU, memory usage (node_exporter)
- **PostgreSQL**: Connections, transaction rates, cache hit ratio
- **Sync Progress**: Block height, epoch duration

## Troubleshooting

**postgres_exporter fails to connect**:
- Verify monitoring user exists and has permissions
- Set `DATA_SOURCE_NAME` with correct credentials in `scripts/start-monitoring.sh`

**Prometheus shows "DOWN" targets**:
- Check exporters are running: `ps aux | grep exporter`
- Test endpoints: `curl http://localhost:9187/metrics` and `curl http://localhost:9100/metrics`

**Port already in use**:
```bash
# Kill processes using ports
lsof -ti:9090 | xargs kill  # Prometheus
lsof -ti:9100 | xargs kill  # node_exporter
lsof -ti:9187 | xargs kill  # postgres_exporter
```

## Stop Services

```bash
# Stop monitoring
tmux kill-session -t cardano-monitoring

# Stop Grafana
brew services stop grafana           # macOS
sudo systemctl stop grafana-server  # Linux
```

## Files

- `scripts/start-monitoring.sh` - Launch monitoring suite
- `config/prometheus.yml` - Prometheus configuration
- `config/grafana-db-sync.json` - Grafana dashboard
- `docs/METRICS.md` - Complete metrics reference

## See Also

- [Prometheus Documentation](https://prometheus.io/docs/)
- [Grafana Documentation](https://grafana.com/docs/)
