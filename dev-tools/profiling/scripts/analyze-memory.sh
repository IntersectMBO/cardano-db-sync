#!/bin/bash
# analyze-memory.sh
# Analyze memory logs from profiling sessions

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROFILING_DIR="$(dirname "$SCRIPT_DIR")"
LOG_DIR="$PROFILING_DIR/logs"

if [ ! -d "$LOG_DIR" ]; then
    echo "ERROR: Profiling logs directory not found: $LOG_DIR"
    exit 1
fi

# Find the most recent memory log
MEMORY_LOG=$(ls -t "$LOG_DIR"/memory-*.csv 2>/dev/null | head -1)

if [ -z "$MEMORY_LOG" ]; then
    echo "ERROR: No memory log files found in $LOG_DIR"
    exit 1
fi

echo "======================================"
echo "Memory Analysis Report"
echo "======================================"
echo "Log file: $MEMORY_LOG"
echo ""

# Skip header and get stats
TOTAL_LINES=$(tail -n +2 "$MEMORY_LOG" | wc -l | tr -d ' ')

if [ "$TOTAL_LINES" -eq 0 ]; then
    echo "ERROR: No data in log file"
    exit 1
fi

echo "Data points: $TOTAL_LINES"
echo ""

# Calculate statistics using awk
awk -F',' 'NR>1 {
    rss[NR]=$2
    vsz[NR]=$3
    cpu[NR]=$4
    sum_rss+=$2
    sum_vsz+=$3
    sum_cpu+=$4
    if ($2 > max_rss) { max_rss=$2; max_rss_time=$1 }
    if ($2 < min_rss || min_rss==0) { min_rss=$2; min_rss_time=$1 }
    count++
}
END {
    avg_rss = sum_rss/count
    avg_vsz = sum_vsz/count
    avg_cpu = sum_cpu/count

    # Calculate standard deviation for RSS
    for (i in rss) {
        diff = rss[i] - avg_rss
        var_rss += diff * diff
    }
    stddev_rss = sqrt(var_rss/count)

    print "RSS (Resident Set Size):"
    printf "  Min:     %10.0f KB (%10.2f MB) at %s\n", min_rss, min_rss/1024, min_rss_time
    printf "  Max:     %10.0f KB (%10.2f MB) at %s\n", max_rss, max_rss/1024, max_rss_time
    printf "  Average: %10.0f KB (%10.2f MB)\n", avg_rss, avg_rss/1024
    printf "  StdDev:  %10.0f KB (%10.2f MB)\n", stddev_rss, stddev_rss/1024
    printf "  Growth:  %10.0f KB (%10.2f MB)\n", max_rss-min_rss, (max_rss-min_rss)/1024
    print ""
    print "VSZ (Virtual Memory Size):"
    printf "  Average: %10.0f KB (%10.2f MB)\n", avg_vsz, avg_vsz/1024
    print ""
    print "CPU Usage:"
    printf "  Average: %6.2f%%\n", avg_cpu
    print ""

    # Detect trends
    first_rss = rss[1]
    last_rss = rss[count]
    growth_rate = (last_rss - first_rss) / first_rss * 100

    print "Trends:"
    printf "  Starting RSS: %10.0f KB (%10.2f MB)\n", first_rss, first_rss/1024
    printf "  Ending RSS:   %10.0f KB (%10.2f MB)\n", last_rss, last_rss/1024
    printf "  Growth Rate:  %6.2f%%\n", growth_rate
    print ""

    # Issue detection
    print "Analysis:"
    if (growth_rate > 50) {
        print "  ⚠ WARNING: Significant memory growth detected (>50%)"
        print "     This may indicate a memory leak or unbounded accumulation."
    } else if (growth_rate > 20) {
        print "  ⚠ NOTICE: Moderate memory growth detected (>20%)"
        print "     Monitor for continued growth patterns."
    } else if (growth_rate < -10) {
        print "  ✓ Memory usage decreased over time."
    } else {
        print "  ✓ Memory usage relatively stable."
    }

    if (stddev_rss > avg_rss * 0.3) {
        print "  ⚠ High memory variance detected"
        print "     Memory usage fluctuates significantly."
    }

    if (max_rss > 8 * 1024 * 1024) {
        printf "  ⚠ High memory usage: %.2f GB\n", max_rss/1024/1024
    }
}' "$MEMORY_LOG"

echo ""
echo "======================================"
echo "Time-Series Data (last 10 points)"
echo "======================================"
tail -10 "$MEMORY_LOG" | awk -F',' 'NR==1 {print "Time                    RSS(MB)  VSZ(MB)   CPU%"} NR>1 {printf "%-20s  %8.2f %8.2f %6.2f\n", $1, $2/1024, $3/1024, $4}'

echo ""
echo "======================================"
echo "Recommendations"
echo "======================================"

# Get growth rate for recommendations
GROWTH_RATE=$(awk -F',' 'NR==2 {first=$2} END {if (first>0) print (($2-first)/first*100); else print 0}' "$MEMORY_LOG")

if (( $(echo "$GROWTH_RATE > 50" | bc -l) )); then
    echo "1. Run ghc-debug-brick to identify memory leaks"
    echo "   ghc-debug-brick /tmp/cardano-db-sync.ghc-debug"
    echo ""
    echo "2. Search for common suspects:"
    echo "   - TxOut, TxIn (transaction data)"
    echo "   - LRUCache, FIFOCache, EpochCache"
    echo "   - BlockGroupedData"
    echo "   - Connection (database connections)"
    echo ""
    echo "3. Look for lazy fields retaining large structures"
    echo ""
elif (( $(echo "$GROWTH_RATE > 20" | bc -l) )); then
    echo "1. Continue monitoring for a longer period"
    echo "2. Take snapshots at regular intervals"
    echo "3. Compare snapshots to identify growing structures"
    echo ""
else
    echo "Memory usage appears stable."
    echo "Consider profiling to identify optimization opportunities:"
    echo "  - Cache sizing"
    echo "  - Grouped data flush frequency"
    echo "  - Database connection pooling"
    echo ""
fi

echo "For detailed analysis, run:"
echo "  ghc-debug-brick /tmp/cardano-db-sync.ghc-debug"
echo ""
echo "To generate a graph (requires gnuplot):"
echo "  gnuplot -e \"set terminal png; set output 'memory.png'; set datafile separator ','; plot '$MEMORY_LOG' using 0:(\$2/1024) with lines title 'RSS (MB)'\" "
echo ""
