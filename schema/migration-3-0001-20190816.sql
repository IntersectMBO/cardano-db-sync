-- Create all the VIEWs.

-- The VIEWs are always created because all VIEWs are deleted at the start ot the
-- migration process.

-- The standard utxo view which shows all unspent transaction outputs.
create view utxo as select tx_out.*
  from tx_out left outer join tx_in
	on tx_out.tx_id = tx_in.tx_out_id and tx_out.index = tx_in.tx_out_index
  where tx_in.tx_in_id is null ;
