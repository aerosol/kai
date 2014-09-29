-module(test_helpers).
-export([set_common_data_dir/2,
         load_fixture/2,
         all_tests/1]).

-include_lib("common_test/include/ct.hrl").

all_tests(Module) ->
    lists:filtermap(
      fun({F, _}) ->
              Name = atom_to_list(F),
              case Name of
                  [$t,$_|_] -> {true, F};
                  _ -> false
              end
      end, Module:module_info(exports)).

set_common_data_dir(Test, Config) ->
    case code:which(Test) of
        Filename when is_list(Filename) ->
            CommonDir = filename:dirname(Filename) ++ "/common_data/",
            [{common_data_dir, CommonDir}|Config]
    end.

load_fixture(Config, Filename) ->
    F = filename:join(?config(common_data_dir, Config), Filename),
    {ok, Fixture} = file:read_file(F),
    Fixture.
