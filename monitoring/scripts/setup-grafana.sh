#!/bin/bash

# Setup Grafana datasources and dashboards for Cardano DB-Sync monitoring
# Requires Grafana to be running on localhost:3000

GRAFANA_URL="http://localhost:3000"
GRAFANA_USER="admin"
GRAFANA_PASS="pass"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MONITORING_DIR="$(dirname "$SCRIPT_DIR")"

echo "=== Setting up Grafana for Cardano DB-Sync Monitoring ==="
echo ""

# Check if Grafana is running
echo "Checking if Grafana is accessible at $GRAFANA_URL..."
if ! curl -s "$GRAFANA_URL/api/health" > /dev/null 2>&1; then
    echo "ERROR: Grafana is not accessible at $GRAFANA_URL"
    echo "Please make sure Grafana is running."
    exit 1
fi
echo "✓ Grafana is running"
echo ""

# Create Prometheus datasource
echo "Setting up Prometheus datasource..."
curl -s -X POST \
  -H "Content-Type: application/json" \
  -u "$GRAFANA_USER:$GRAFANA_PASS" \
  -d '{
    "name": "Prometheus",
    "type": "prometheus",
    "url": "http://localhost:9090",
    "access": "proxy",
    "isDefault": true,
    "jsonData": {
      "timeInterval": "10s",
      "queryTimeout": "60s",
      "httpMethod": "POST"
    }
  }' \
  "$GRAFANA_URL/api/datasources" > /dev/null 2>&1 \
&& echo "✓ Prometheus datasource created (or already exists)" \
|| echo "✓ Prometheus datasource already exists (ignoring error)"
echo ""

# Import dashboard
echo "Importing Cardano DB-Sync Complete dashboard..."
DASHBOARD_JSON="$MONITORING_DIR/grafana/dashboards/cardano-db-sync-complete.json"

if [ ! -f "$DASHBOARD_JSON" ]; then
    echo "ERROR: Dashboard file not found: $DASHBOARD_JSON"
    exit 1
fi

# Wrap dashboard JSON in the required format
DASHBOARD_PAYLOAD=$(jq '{dashboard: ., overwrite: true, message: "Imported via setup script"}' < "$DASHBOARD_JSON")

curl -s -X POST \
  -H "Content-Type: application/json" \
  -u "$GRAFANA_USER:$GRAFANA_PASS" \
  -d "$DASHBOARD_PAYLOAD" \
  "$GRAFANA_URL/api/dashboards/db" | jq -r '.status, .url' | while read -r line; do
    if [ "$line" = "success" ]; then
        echo "✓ Dashboard imported successfully"
    elif [[ "$line" == /d/* ]]; then
        echo "✓ Dashboard URL: $GRAFANA_URL$line"
    fi
done

echo ""
echo "=== Setup Complete ==="
echo ""
echo "Access your dashboard at:"
echo "  $GRAFANA_URL (navigate to Dashboards to find the imported dashboard)"
echo ""
echo "Default credentials:"
echo "  Username: admin"
echo "  Password: pass"
echo ""
echo "Make sure the following services are running:"
echo "  - Prometheus on http://localhost:9090"
echo "  - Telegraf collecting metrics"
echo "  - cardano-db-sync with metrics enabled"
echo ""
