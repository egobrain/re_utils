re_utils
========

Regexp utils that extends erlang re module.

groups/1
--------

Groups function parse re string and return group names as atoms.

```Erlang
-spec groups(binary() | string()) -> [atom()].
```
**Usage:**
```Erlang
1> re_utils:groups("^(?<login>[A-z]+) password is (?<password>[A-z0-9]+)$").
[login, password].
```

captures/2,3
------------

Run regex and return named groups and values if match.

```Erlang
-spec captures(Subject, Re, Options) -> {match, [{atom(), binary()}]} | nomatch when
	  Subject :: iodata(),
	  Re :: iodata() | capture_re(),
	  Options :: re_opts().
```

Options are the same as for re module

**Usage:**
```Erlang
2> re_utils:captures("Tom password is qwerty", "^(?<login>[A-z]+) password is (?<password>[A-z0-9]+)$").
{match,[{login,<<"Tom">>},{password,<<"qwerty">>}]}
```

compile/1
---------

```Erlang
-spec compile(Re :: iodata()) -> {ok, capture_re()} | {error, ErrSpec} when
	  ErrSpec :: {ErrString, Position},
	  ErrString :: string(),
	  Position :: non_neg_integer().
```

Precompile regexp and parse re groups for later usage.

**Usage:**
```Erlang
3> {ok, Re} = re_utils:compile("^(?<login>[A-z]+) password is (?<password>[A-z0-9]+)$").
4> re_utils:captures("Tom password is qwerty", Re).
{match,[{login,<<"Tom">>},{password,<<"qwerty">>}]}
```
