%%%----------------------------------------------------------------------
%%% File    : mod_vcard_admin.erl
%%% Author  : Keewon Seo <oedalpha at gmail>
%%% Purpose : Tools for additional vCard administration
%%% Created : 11 Jun 2011 by Keewon Seo <oedalpha at gmail>
%%%----------------------------------------------------------------------

-module(mod_vcard_admin).
-author('oedalpha@gmail.com').

-behaviour(gen_mod).

-export([
	 start/2, stop/1, % gen_mod API
	 web_menu_host/3, web_page_host/3
	]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-include("web/ejabberd_http.hrl").
-include("web/ejabberd_web_admin.hrl").
-include("ejabberd_commands.hrl").

%% copied from mod_vcard.erl
-record(vcard_search, {us,
		       user,     luser,
		       fn,	 lfn,
		       family,	 lfamily,
		       given,	 lgiven,
		       middle,	 lmiddle,
		       nickname, lnickname,
		       bday,	 lbday,
		       ctry,	 lctry,
		       locality, llocality,
		       email,	 lemail,
		       orgname,	 lorgname,
		       orgunit,	 lorgunit
		      }).
%% -record(vcard, {us, vcard}).

%%----------------------------
%% gen_mod
%%----------------------------

start(Host, _Opts) ->
    %%ejabberd_commands:register_commands(commands()),
    ejabberd_hooks:add(webadmin_menu_host, Host, ?MODULE, web_menu_host, 50),
    ejabberd_hooks:add(webadmin_page_host, Host, ?MODULE, web_page_host, 50).

stop(Host) ->
    %%ejabberd_commands:unregister_commands(commands()),
    ejabberd_hooks:delete(webadmin_menu_host, Host, ?MODULE, web_menu_host, 50),
    ejabberd_hooks:delete(webadmin_page_host, Host, ?MODULE, web_page_host, 50).

%%----------------------------
%% Web Admin
%%----------------------------

%%---------------
%% Web Admin Menu

web_menu_host(Acc, _Host, Lang) ->
    Acc ++ [{"vcard", ?T("vCard")}].


%%---------------
%% Web Admin Page

web_page_host(_, Host,
	      #request{path = ["vcard"],
		       q = Q,
		       lang = Lang} = _Request) ->
    {User, Email, Nick} = parse_query(Q, ["user", "email", "nick"]),
    List = do_query(Q, Host),
    SearchResult = case List of
	not_found   -> ?XC("p", "Not found");
	none	    -> ?XC("p", "");
	_	    -> build_table(List)
    end,
    Res = [?XC("h1", "vCard"),
	   ?XC("h3", "Search"),
	   SearchResult,
	   ?XAE("form", [{"action", ""}, {"method", "post"}],
		   [?XE("table",
		       [?XE("tr",
			   [?XC("td", ?T("JID") ++ ":"),
			   ?XE("td", [?INPUT("text", "user", User)]),
			   ?XE("td", [?C([" @ ", Host])])
			   ]),
		       ?XE("tr",
			   [?XC("td", ?T("Email") ++ ":"),
			   ?XE("td", [?INPUT("text", "email", Email)]),
			   ?X("td")
			   ]),
		       ?XE("tr",
			   [?XC("td", ?T("Nickname") ++ ":"),
			   ?XE("td", [?INPUT("text", "nick", Nick)]),
			   ?X("td")
			   ]),
		       ?XE("tr",
			   [?X("td"),
			   ?XAE("td", [{"class", "alignright"}],
			       [?INPUTT("submit", "search", "Search")]),
			   ?X("td")
			   ])]),
		   ?P])
	  ],
    {stop, Res};
web_page_host(Acc, _, _) -> Acc.

parse_query(Query, Args) ->
    List = lists:foldl(
	fun(X, L) -> 
	    case lists:keysearch(X, 1, Query) of
	    {value, {_, V}} ->
		L ++ [V];
	    _ ->
		L ++ [""]
	    end
	end,
	[], Args),
    list_to_tuple(List).

do_query(Query, Host) ->
    case lists:keysearch("search", 1, Query) of
    {value, _} ->
	Data = lists:foldl(
	    fun(X, L) -> 
		case lists:keysearch(X, 1, Query) of
		{value, {_, V}} ->
		    L ++ [{X, [V]}];
		_ ->
		    L
		end
	    end,
	    [], ["user", "email", "nick"]),

	Res = search(Host, Data),
	case Res of
	    [] -> not_found;
	    _ -> Res
	end;
    _ ->
	none
    end.


%% copied from mod_vcard.erl
search(LServer, Data) ->
    MatchSpec = make_matchspec(LServer, Data),

    case catch mnesia:dirty_select(vcard_search,
				   [{MatchSpec, [], ['$_']}]) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]),
	    [];
	Rs ->
	    Rs
    end.


make_matchspec(LServer, Data) ->
    GlobMatch = #vcard_search{_ = '_'},
    Match = filter_fields(Data, GlobMatch, LServer),
    Match.

filter_fields([], Match, _LServer) ->
    Match;
filter_fields([{SVar, [Val]} | Ds], Match, LServer)
  when is_list(Val) and (Val /= "") ->
    LVal = stringprep:tolower(Val),
    NewMatch = case SVar of
                   "user"     -> Match#vcard_search{us        = {make_val(LVal), LServer}};
                   "fn"       -> Match#vcard_search{lfn       = make_val(LVal)};
                   "last"     -> Match#vcard_search{lfamily   = make_val(LVal)};
                   "first"    -> Match#vcard_search{lgiven    = make_val(LVal)};
                   "middle"   -> Match#vcard_search{lmiddle   = make_val(LVal)};
                   "nick"     -> Match#vcard_search{lnickname = make_val(LVal)};
                   "bday"     -> Match#vcard_search{lbday     = make_val(LVal)};
                   "ctry"     -> Match#vcard_search{lctry     = make_val(LVal)};
                   "locality" -> Match#vcard_search{llocality = make_val(LVal)};
                   "email"    -> Match#vcard_search{lemail    = make_val(LVal)};
                   "orgname"  -> Match#vcard_search{lorgname  = make_val(LVal)};
                   "orgunit"  -> Match#vcard_search{lorgunit  = make_val(LVal)};
		   _          -> Match
	       end,
    filter_fields(Ds, NewMatch, LServer);
filter_fields([_ | Ds], Match, LServer) ->
    filter_fields(Ds, Match, LServer).

make_val(Val) ->
    case lists:suffix("*", Val) of
	true ->
	    lists:sublist(Val, length(Val) - 1) ++ '_';
	_ ->
	    Val
    end.

build_table([]) ->
    ?XE("table", []);
build_table(List) ->
    Els = lists:map(
	fun(X) ->
	    %?INFO_MSG("us: ~p", [X#vcard_search.us]),
	    {User, Server} = X#vcard_search.us,
	    ?XE("tr", [
		?XE("td", [?AC("../user/" ++ User, User ++ "@" ++ Server)]),
		?XE("td", [?C(X#vcard_search.email)]),
		?XE("td", [?C(X#vcard_search.nickname)])
	    ])
	end,
	List),
    case Els of
	[] ->
	    ?XE("table", Els);
	_ ->
	    Header = ?XE("tr", [
		?XE("td", [?C("JID")]), 
		?XE("td", [?C("Email")]), 
		?XE("td", [?C("Nick")]) 
	    ]),
	    ?XE("table", [Header] ++ Els)
    end.
