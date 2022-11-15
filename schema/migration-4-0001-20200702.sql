
-- This Index takes a lot of time to be created.
CREATE INDEX IF NOT EXISTS idx_tx_out_address ON tx_out USING hash (address);
