-- START: cardano-graphql views
create view "Epoch" as
select
  epoch.out_sum as "output",
  epoch.no as "number",
  epoch.tx_count as "transactionsCount",
  epoch.start_time as "startedAt",
  epoch.end_time as "lastBlockTime",
  epoch.blk_count as "blocksCount"
from epoch
-- END: cardano-graphql views