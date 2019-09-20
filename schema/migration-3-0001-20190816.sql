-- Create all the views.

-- The views are always created because all views are deleted at the start ot the
-- migration process.

-- Conventions:
--  * VIEWs for the Javascript GraphQL interface use CamelCase names.
--  * Other views use `_view` as a suffix to show they are views rather than table.

-- The standard utxo view which shows all unspent transaction outputs.
create view utxo_view as select
	tx_out.*
  from tx_out left outer join tx_in
	on tx_out.tx_id = tx_in.tx_out_id and tx_out.index = tx_in.tx_out_index
  where tx_in.tx_in_id is null ;


-- Views for the explorer Javascript GraphQL interface.
create view TransactionOutput as select
	address,
	tx.hash as txId,
	index
  from tx inner join tx_out
	on tx.id = tx_out.tx_id ;

create view TransactionInput as select
	address,
	tx.hash as sourceTxId,
	tx_in.tx_out_index as sourceTxIndex,
	value
  from tx inner join tx_out on tx.id = tx_out.tx_id
	inner join tx_in on tx_in.tx_out_id = tx.id ;
