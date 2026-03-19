-- Epoch Duration Query for Grafana PostgreSQL Panel
-- 
-- Queries epoch_sync_time using the synced_at timestamp (when db-sync processed the epoch)
-- to plot each epoch at its actual sync time on the timeline.
-- Uses Grafana's time range variables so users can zoom in/out.
-- Graph updates automatically when new epochs complete.

SELECT 
  est.synced_at as time,
  est.seconds as "Duration (seconds)",
  est.no as "Epoch Number"
FROM epoch_sync_time est
WHERE est.synced_at IS NOT NULL
  AND est.synced_at BETWEEN $__timeFrom() AND $__timeTo()
ORDER BY est.synced_at;
