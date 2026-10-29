-- EXPERIMENTAL (Dijkstra): indexes for the provisional Dijkstra tables; see the note at
-- the top of doc/schema.md.

CREATE INDEX IF NOT EXISTS idx_leios_cert_block_id ON leios_cert(block_id) ;
