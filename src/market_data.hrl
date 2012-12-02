-record(marketOrder, 
{
  symbol              :: string(),
  type                :: buy | sell,
  limit               :: none | integer(),
  quantity            :: integer(),
  quantity_constraint :: none | all,
  time_in_force       :: day | canceled | immediate | fill
}).
