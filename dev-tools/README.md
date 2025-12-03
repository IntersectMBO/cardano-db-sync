# Cardano DB Sync - Developer Tools

Local development tools for monitoring and profiling cardano-db-sync.

## Overview

Two complementary tools for understanding cardano-db-sync performance:

### 🔍 [Monitoring](monitoring/README.md)
Real-time metrics with Prometheus/Grafana
- PostgreSQL metrics (queries, cache, connections)
- System metrics (CPU, memory, disk I/O)
- Time-series visualization

### 🧠 [Profiling](profiling/README.md)
Memory profiling with ghc-debug
- Interactive heap exploration
- Memory leak detection
- Retainer chain analysis

## Installation

### Monitoring

```bash
# macOS
brew install tmux prometheus postgres_exporter node_exporter grafana

# Linux (apt)
sudo apt-get install tmux prometheus postgres-exporter prometheus-node-exporter grafana

# Linux (yum)
sudo yum install tmux prometheus postgres_exporter node_exporter grafana
```

### Profiling

```bash
# Install ghc-debug-brick
git clone https://gitlab.haskell.org/ghc/ghc-debug.git
cd ghc-debug/brick
cabal install ghc-debug-brick

# Instrument cardano-db-sync (see profiling/README.md for details)
```

## Running

### Start Monitoring

```bash
cd dev-tools/monitoring
./scripts/start-monitoring.sh
```

Access at:
- Prometheus: http://localhost:9090
- Grafana: http://localhost:3000 (if started separately)

### Start Profiling

```bash
cd dev-tools/profiling
./scripts/start-profiling.sh

# In another terminal (after several hours):
ghc-debug-brick /tmp/cardano-db-sync.ghc-debug
```

### Run Both Together

```bash
# Terminal 1: Monitoring
cd dev-tools/monitoring && ./scripts/start-monitoring.sh

# Terminal 2: Profiling
cd dev-tools/profiling && ./scripts/start-profiling.sh
```

## Directory Structure

```
dev-tools/
├── README.md                   # This file
│
├── monitoring/                 # Prometheus/Grafana monitoring
│   ├── README.md              # Full monitoring documentation
│   ├── scripts/
│   │   └── start-monitoring.sh
│   ├── config/
│   │   └── prometheus.yml
│   ├── data/                  # Prometheus data (gitignored)
│   └── docs/
│       └── METRICS.md         # Available metrics
│
└── profiling/                 # ghc-debug profiling
    ├── README.md              # Full profiling documentation
    ├── scripts/
    │   ├── start-profiling.sh
    │   └── analyze-memory.sh
    ├── snapshots/             # Heap snapshots (gitignored)
    ├── logs/                  # Memory logs (gitignored)
    └── reports/               # Analysis reports (commit these)
```

## Documentation

- [Monitoring Setup Guide](monitoring/README.md)
- [Profiling Setup Guide](profiling/README.md)
- [Available Metrics Reference](monitoring/docs/METRICS.md)
