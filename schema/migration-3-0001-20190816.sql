-- Create all the views.

-- The views are always created because all views are deleted at the start ot the
-- migration process.

-- Conventions:
--  * Views use `_view` as a suffix to show they are views rather than table.

-- The utxo view which shows all unspent transaction outputs.
create view utxo_view as select
	tx_out.*
  from tx_out left outer join tx_in
	on tx_out.tx_id = tx_in.tx_out_id and tx_out.index = tx_in.tx_out_index
  where tx_in.tx_in_id is null ;
