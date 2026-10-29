-- EXPERIMENTAL (Dijkstra): indexes for the provisional Dijkstra tables; see the note at
-- the top of doc/schema.md.

CREATE INDEX IF NOT EXISTS idx_leios_cert_block_id ON leios_cert(block_id) ;
CREATE INDEX IF NOT EXISTS idx_direct_deposit_tx_id ON direct_deposit(tx_id) ;
CREATE INDEX IF NOT EXISTS idx_direct_deposit_addr_id ON direct_deposit(addr_id) ;
CREATE INDEX IF NOT EXISTS idx_tx_guard_tx_id ON tx_guard(tx_id) ;
CREATE INDEX IF NOT EXISTS idx_tx_guard_credential ON tx_guard(credential) ;
CREATE INDEX IF NOT EXISTS idx_tx_required_guard_tx_id ON tx_required_guard(tx_id) ;
CREATE INDEX IF NOT EXISTS idx_tx_required_guard_credential ON tx_required_guard(credential) ;
