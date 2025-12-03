#!/bin/bash

set -euo pipefail

# Cardano DB Sync - Monitoring Suite Launcher
# Starts Prometheus, postgres_exporter, and node_exporter in a tmux session

SESSION="cardano-monitoring"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MONITORING_DIR="$(dirname "$SCRIPT_DIR")"
CONFIG_DIR="$MONITORING_DIR/config"
DATA_DIR="$MONITORING_DIR/data"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

print_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check dependencies
check_dependencies() {
    local missing=()

    command -v tmux >/dev/null 2>&1 || missing+=("tmux")
    command -v prometheus >/dev/null 2>&1 || missing+=("prometheus")
    command -v postgres_exporter >/dev/null 2>&1 || missing+=("postgres_exporter")
    command -v node_exporter >/dev/null 2>&1 || missing+=("node_exporter")

    if [ ${#missing[@]} -gt 0 ]; then
        print_error "Missing required dependencies: ${missing[*]}"
        print_info "Install with:"
        echo "  brew install tmux prometheus postgres_exporter node_exporter"
        exit 1
    fi
}

# Check if PostgreSQL is running
check_postgres() {
    if ! pg_isready -h 127.0.0.1 -p 5432 >/dev/null 2>&1; then
        print_warn "PostgreSQL does not appear to be running on localhost:5432"
        print_info "Start PostgreSQL before running monitoring suite"
        read -p "Continue anyway? [y/N] " -n 1 -r
        echo
        [[ ! $REPLY =~ ^[Yy]$ ]] && exit 1
    fi
}

# Create data directory
ensure_data_dir() {
    mkdir -p "$DATA_DIR"
    print_info "Data directory: $DATA_DIR"
}

# Kill existing session if it exists
cleanup_existing() {
    if tmux has-session -t "$SESSION" 2>/dev/null; then
        print_warn "Killing existing session: $SESSION"
        tmux kill-session -t "$SESSION"

        # Kill any orphaned exporters/prometheus
        pkill -f postgres_exporter 2>/dev/null || true
        pkill -f node_exporter 2>/dev/null || true
        pkill -f prometheus 2>/dev/null || true

        sleep 1
    fi
}

# Database connection settings
DB_HOST="${DB_HOST:-127.0.0.1}"
DB_PORT="${DB_PORT:-5432}"
DB_NAME="${DB_NAME:-cexplorer}"
DB_USER="${DB_USER:-postgres_exporter}"

# Exporter ports
POSTGRES_EXPORTER_PORT="${POSTGRES_EXPORTER_PORT:-9187}"
NODE_EXPORTER_PORT="${NODE_EXPORTER_PORT:-9100}"
PROMETHEUS_PORT="${PROMETHEUS_PORT:-9090}"

print_info "Starting Cardano DB Sync Monitoring Suite"
print_info "=========================================="

check_dependencies
check_postgres
ensure_data_dir
cleanup_existing

# Create new tmux session
tmux new-session -d -s "$SESSION"

# Configure layout: 4 panes
#  ┌─────────┬─────────┐
#  │    0    │    2    │
#  │ postgres│  node   │
#  │ exporter│ exporter│
#  ├─────────┼─────────┤
#  │    1    │    3    │
#  │prometheus│  info   │
#  └─────────┴─────────┘

tmux rename-window -t "$SESSION" "monitoring"
tmux split-window -h -t "$SESSION"
tmux split-window -v -t "$SESSION:0.0"
tmux split-window -v -t "$SESSION:0.2"

# Pane 0: postgres_exporter
print_info "Starting postgres_exporter on port $POSTGRES_EXPORTER_PORT"
tmux send-keys -t "$SESSION:0.0" "cd '$MONITORING_DIR'" C-m
tmux send-keys -t "$SESSION:0.0" "echo '=== PostgreSQL Exporter ==='" C-m
tmux send-keys -t "$SESSION:0.0" "export DATA_SOURCE_NAME='postgresql://${DB_USER}@${DB_HOST}:${DB_PORT}/${DB_NAME}?sslmode=disable'" C-m
tmux send-keys -t "$SESSION:0.0" "postgres_exporter --web.listen-address=:${POSTGRES_EXPORTER_PORT}" C-m

# Pane 1: prometheus
print_info "Starting Prometheus on port $PROMETHEUS_PORT"
tmux send-keys -t "$SESSION:0.1" "cd '$MONITORING_DIR'" C-m
tmux send-keys -t "$SESSION:0.1" "echo '=== Prometheus ==='" C-m
tmux send-keys -t "$SESSION:0.1" "prometheus --config.file='$CONFIG_DIR/prometheus.yml' --storage.tsdb.path='$DATA_DIR' --web.listen-address=:${PROMETHEUS_PORT}" C-m

# Pane 2: node_exporter
print_info "Starting node_exporter on port $NODE_EXPORTER_PORT"
tmux send-keys -t "$SESSION:0.2" "cd '$MONITORING_DIR'" C-m
tmux send-keys -t "$SESSION:0.2" "echo '=== Node Exporter ==='" C-m
tmux send-keys -t "$SESSION:0.2" "node_exporter --web.listen-address=:${NODE_EXPORTER_PORT} --no-collector.thermal" C-m

# Pane 3: Info pane
tmux send-keys -t "$SESSION:0.3" "cd '$MONITORING_DIR'" C-m
tmux send-keys -t "$SESSION:0.3" "clear" C-m
tmux send-keys -t "$SESSION:0.3" "cat << 'EOF'
╔═══════════════════════════════════════════════════════════════╗
║         Cardano DB Sync Monitoring Suite                     ║
╚═══════════════════════════════════════════════════════════════╝

Monitoring Status:
  ✓ PostgreSQL Exporter  : http://localhost:${POSTGRES_EXPORTER_PORT}
  ✓ Node Exporter        : http://localhost:${NODE_EXPORTER_PORT}
  ✓ Prometheus           : http://localhost:${PROMETHEUS_PORT}

Quick Links:
  - Prometheus UI        : http://localhost:${PROMETHEUS_PORT}
  - Metrics Explorer     : http://localhost:${PROMETHEUS_PORT}/graph
  - Targets Status       : http://localhost:${PROMETHEUS_PORT}/targets

Grafana Setup:
  1. Install Grafana: brew install grafana
  2. Start Grafana: brew services start grafana
  3. Access: http://localhost:3000 (admin/admin)
  4. Add Prometheus datasource: http://localhost:${PROMETHEUS_PORT}
  5. Import dashboard from docs/grafana-dashboard.json

Useful Commands:
  - View Prometheus config : cat config/prometheus.yml
  - View logs              : Check panes above
  - Stop monitoring        : tmux kill-session -t ${SESSION}
  - Detach from session    : Ctrl+b, d
  - Re-attach to session   : tmux attach -t ${SESSION}

Database Connection:
  Host     : ${DB_HOST}
  Port     : ${DB_PORT}
  Database : ${DB_NAME}
  User     : ${DB_USER}

Documentation:
  - See docs/README.md for full setup guide
  - See docs/METRICS.md for available metrics
  - See docs/grafana-dashboard.json for Grafana template

EOF
" C-m

sleep 2

print_info ""
print_info "Monitoring suite started successfully!"
print_info ""
print_info "  Prometheus UI : http://localhost:${PROMETHEUS_PORT}"
print_info "  Targets       : http://localhost:${PROMETHEUS_PORT}/targets"
print_info ""
print_info "Attaching to tmux session '$SESSION'..."
print_info "(Press Ctrl+b, d to detach; tmux attach -t $SESSION to reattach)"
print_info ""

# Attach to session
tmux select-pane -t "$SESSION:0.3"
tmux attach-session -t "$SESSION"
