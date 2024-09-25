--
-- The following index creations only happen when not using "use_address_table" config in tx-out.
-- This is because the following uses columns that get moved to the `address` table .
--

CREATE INDEX IF NOT EXISTS idx_tx_out_payment_cred ON tx_out(payment_cred);
CREATE INDEX IF NOT EXISTS idx_tx_out_stake_address_id ON tx_out(stake_address_id);

-- Left here for reference, it's removed to speed up restoring from a snapshot as this index is very slow to create.
-- CREATE INDEX IF NOT EXISTS idx_tx_out_address ON tx_out USING hash (address);
