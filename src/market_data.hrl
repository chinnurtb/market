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
  timestamp           :: integer()
}).
-define(SYMBOLS, [ cocaine, heroin, speed, mdma, acid, shrooms, pot, oxycodone, valium, ketamine ] ).
