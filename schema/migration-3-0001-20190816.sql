-- Create all the views.

-- The views are always created because all views are deleted at the start ot the
-- migration process.

-- Conventions:
--  * Views use `_view` as a suffix to show they are views rather than table.

-- A utxo view which shows all unspent transaction outputs including the un-redeemed redeem
-- addresses.
create view utxo_byron_view as select
	tx_out.*
  from tx_out left outer join tx_in
	on tx_out.tx_id = tx_in.tx_out_id and tx_out.index = tx_in.tx_out_index
  where tx_in.tx_in_id is null ;


-- A utxo view which shows all unspent transaction outputs *excluding* the un-redeemed redeem
-- addresses.
-- This should produce the same query results as the above `utxo_byron_view` for Shelley addresses
-- and non-redeem Byron addresses.
create view utxo_view as select
	tx_out.*
  from tx_out
	left outer join tx_in on tx_out.tx_id = tx_in.tx_out_id and tx_out.index = tx_in.tx_out_index
	left outer join tx on tx.id = tx_out.tx_id
	left outer join block on tx.block_id = block.id
  where tx_in.tx_in_id is null and block.epoch_no is not null ;
