-module(ppme).

-export([save_to_ppm/4, test_run/4, gradient/4]).

save_to_ppm(FileName, DataList, Width, Height) when is_list(DataList) ->
  {ok, IoDevice} = file:open(FileName, [write]),
  ok = io:fwrite(IoDevice, "P6~n~p ~p 255~n", [Width, Height]),
  Data = lists:map(fun(Color) -> <<Color:24>> end, DataList),
  ok = file:write(IoDevice, Data),
  ok = file:close(IoDevice).

test_run(FileName, Width, Height, ColorTriplet) ->
  DataList = lists:duplicate(Width * Height, ColorTriplet),
  save_to_ppm(FileName, DataList, Width, Height).

gradient(FileName, Width, Height, BaseColorTriplet) ->
  DataList = [lists:duplicate(Width, round(BaseColorTriplet + (S / Height) * 255)) || S <- lists:seq(0, Height)],
  save_to_ppm(FileName, lists:flatten(DataList), Width, Height).
