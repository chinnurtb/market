-record(marketOrder, 
{
  id                  :: none | string(),
  user                :: integer(),
  symbol              :: atom(),
  type                :: bid | ask,
  limit               :: none | integer(),
  quantity            :: integer(),
  quantity_constraint :: none | all,
  time_in_force       :: day | cancelled | immediate | fill,
  timestamp           :: integer(),
  state               :: new | booked | cancelled | closed | locked,
  retries             :: undefined | integer()
}).
-record(marketTxn,
{
  id                :: integer(),
  buy               :: integer(),
  sell              :: integer(),
  price             :: integer(),
  quantity          :: integer()
}).
-define(SYMBOLS, [ cocaine, heroin, speed, mdma, acid, shrooms, pot, oxycodone, valium, ketamine ] ).
