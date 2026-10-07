#!/bin/bash
# Verify monitoring setup for Cardano DB Sync

echo "=== Cardano DB Sync Monitoring Verification ==="
echo ""

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check PostgreSQL connection
echo "1. Checking PostgreSQL connection..."
if psql -U postgres -d cexplorer -c "SELECT 1" > /dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} PostgreSQL is accessible"
else
    echo -e "${RED}✗${NC} Cannot connect to PostgreSQL"
    exit 1
fi

# Check pg_stat_statements extension
echo ""
echo "2. Checking pg_stat_statements extension..."
PG_STAT=$(psql -U postgres -d cexplorer -tAc "SELECT COUNT(*) FROM pg_extension WHERE extname = 'pg_stat_statements'")
if [ "$PG_STAT" = "1" ]; then
    echo -e "${GREEN}✓${NC} pg_stat_statements extension is enabled"
    
    # Check if it's collecting data
    STMT_COUNT=$(psql -U postgres -d cexplorer -tAc "SELECT COUNT(*) FROM pg_stat_statements" 2>/dev/null)
    if [ -n "$STMT_COUNT" ] && [ "$STMT_COUNT" -gt "0" ]; then
        echo -e "${GREEN}✓${NC} pg_stat_statements is collecting data ($STMT_COUNT queries tracked)"
    else
        echo -e "${YELLOW}⚠${NC} pg_stat_statements is enabled but not collecting data yet"
        echo "   Run some queries or wait for cardano-db-sync to execute queries"
    fi
else
    echo -e "${RED}✗${NC} pg_stat_statements extension is NOT enabled"
    echo "   Run: psql -U postgres -d cexplorer -c 'CREATE EXTENSION pg_stat_statements;'"
    echo "   Note: Requires 'shared_preload_libraries = pg_stat_statements' in postgresql.conf"
fi

# Check telegraf_monitor user
echo ""
echo "3. Checking telegraf_monitor user..."
TELEGRAF_USER=$(psql -U postgres -d cexplorer -tAc "SELECT COUNT(*) FROM pg_roles WHERE rolname = 'telegraf_monitor'")
if [ "$TELEGRAF_USER" = "1" ]; then
    echo -e "${GREEN}✓${NC} telegraf_monitor user exists"
    
    # Check permissions
    HAS_PERMS=$(psql -U postgres -d cexplorer -tAc "SELECT has_table_privilege('telegraf_monitor', 'block', 'SELECT')")
    if [ "$HAS_PERMS" = "t" ]; then
        echo -e "${GREEN}✓${NC} telegraf_monitor has SELECT permissions on tables"
    else
        echo -e "${RED}✗${NC} telegraf_monitor missing SELECT permissions"
    fi
else
    echo -e "${RED}✗${NC} telegraf_monitor user does NOT exist"
    echo "   Run: psql -U postgres -d cexplorer -f monitoring/scripts/grant-telegraf-permissions.sql"
fi

# Check if Telegraf is running
echo ""
echo "4. Checking if Telegraf is running..."
if pgrep -x "telegraf" > /dev/null; then
    echo -e "${GREEN}✓${NC} Telegraf is running"
    
    # Check if metrics endpoint is accessible
    if curl -s http://localhost:9273/metrics > /dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} Telegraf metrics endpoint accessible (http://localhost:9273/metrics)"
        
        # Check for pg_stat_statements metrics
        if curl -s http://localhost:9273/metrics | grep -q "pg_stat_statements_queries"; then
            echo -e "${GREEN}✓${NC} Query-level metrics are being collected"
        else
            echo -e "${YELLOW}⚠${NC} Query-level metrics not found in Telegraf output"
            echo "   This is normal if pg_stat_statements has no data yet"
        fi
    else
        echo -e "${RED}✗${NC} Telegraf metrics endpoint not accessible"
    fi
else
    echo -e "${RED}✗${NC} Telegraf is NOT running"
    echo "   Start with: telegraf --config monitoring/config/telegraf.conf"
fi

# Check if Prometheus is running
echo ""
echo "5. Checking if Prometheus is running..."
if pgrep -x "prometheus" > /dev/null; then
    echo -e "${GREEN}✓${NC} Prometheus is running"
    
    # Check if Prometheus can scrape Telegraf
    if curl -s http://localhost:9090/-/healthy > /dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} Prometheus web UI accessible (http://localhost:9090)"
    else
        echo -e "${RED}✗${NC} Prometheus web UI not accessible"
    fi
else
    echo -e "${RED}✗${NC} Prometheus is NOT running"
fi

# Check if Grafana is running
echo ""
echo "6. Checking if Grafana is running..."
if curl -s http://localhost:3000/api/health > /dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} Grafana is running (http://localhost:3000)"
else
    echo -e "${YELLOW}⚠${NC} Grafana is not accessible at http://localhost:3000"
    echo "   Start with: brew services start grafana (macOS) or sudo systemctl start grafana-server (Linux)"
fi

# Check cardano-db-sync metrics
echo ""
echo "7. Checking cardano-db-sync metrics..."
if pgrep -f "cardano-db-sync" > /dev/null; then
    echo -e "${GREEN}✓${NC} cardano-db-sync is running"
    
    if curl -s http://localhost:8080 > /dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} cardano-db-sync metrics endpoint accessible"
    else
        echo -e "${YELLOW}⚠${NC} cardano-db-sync metrics endpoint not accessible"
    fi
else
    echo -e "${YELLOW}⚠${NC} cardano-db-sync is not running"
fi

echo ""
echo "=== Verification Complete ==="
echo ""
echo "Next steps:"
echo "1. If pg_stat_statements is missing, edit postgresql.conf and add:"
echo "   shared_preload_libraries = 'pg_stat_statements'"
echo "2. Restart PostgreSQL: brew services restart postgresql@14"
echo "3. Run: psql -U postgres -d cexplorer -f monitoring/scripts/grant-telegraf-permissions.sql"
echo "4. Import dashboard: monitoring/grafana/dashboards/cardano-db-sync-complete.json"
