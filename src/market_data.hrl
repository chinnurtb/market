-define(SYMBOLS, [ cocaine, heroin, speed, mdma, acid, shrooms, pot, oxycodone, valium, ketamine ] ).
-record(marketOrder, 
{
  id                  :: none | string(),
  user                :: binary(),
  symbol              :: atom(),
  type                :: bid | ask,
  limit               :: integer(),
  quantity            :: integer(),
  quantity_constraint :: none | all, 
  time_in_force       :: day | cancelled | immediate | fill,
  timestamp           :: integer(),
  state               :: new | booked | cancelled | closed | locked,
  retries             :: integer()
}).
