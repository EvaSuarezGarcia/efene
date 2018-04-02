-module(fn_ast_SUITE).
-compile(export_all).

all() -> [unary_op_test, op_test, attr_test, kv_test, tag_test, expr_test, call_mf_test,
            call_f_test, call_test, e_when_else_test, e_when_cond_test, e_when_test,
            e_case_else_test, e_case_match_test, e_case_test, e_fn_test, s_cons_test,
            s_map_test, s_list_test, s_tuple_test, v_var_test, v_fn_ref_test, v_atom_test,
            v_bool_test, v_str_test, v_float_test, v_int_test, unary_ops_test,
            comparison_ops_test, ladd_lsub_test, binary_ops_test, arithmetic_ops_test,
            logic_ops_test, op_send_test, op_match_test].

%% Suite information function. Sets a time limit for tests cases so they will fail in case of infinite loop
suite() ->
    [{timetrap, {seconds, 10}}].

unary_op_test(Config) ->
    Op = "op",
    Val = val,
    Line = 7,
    {unary_op, 1, Op, Val} = fn_ast:unary_op(Op, Val),
    {unary_op, Line, Op, Val} = fn_ast:unary_op(Op, Line, Val).

op_test(Config) ->
    Op = op,
    Left = 3,
    Right = {right},
    Line = "7",
    {op, 1, Op, Left, Right} = fn_ast:op(Op, Left, Right),
    {op, Line, Op, Left, Right} = fn_ast:op(Op, Line, Left, Right).

attr_test(Config) ->
    fn_ast:attr(1, "attr_test").

kv_test(Config) ->
    Line = 0,
    Key = 4,
    Val = val,
    {kv, 1, Key, Val} = fn_ast:kv(Key, Val),
    {kv, Line, Key, Val} = fn_ast:kv(Line, Key, Val).

tag_test(Config) ->
    Line = line,
    Tag = "tag",
    Val = {1, 2, 3},
    {tag, 1, Tag, Val} = fn_ast:tag(Tag, Val),
    {tag, Line, Tag, Val} = fn_ast:tag(Line, Tag, Val).

expr_test(Config) ->
    Line = "line",
    Type = type,
    Args = [true, {7}],
    {expr, Line, Type, Args} = fn_ast:expr(Line, Type, Args).

call_mf_test(Config) ->
    Line = 5.8,
    Mod = "mod",
    Fun = fun(X) -> X end,
    Args = [x],
    {expr, 1, call, {{Mod, Fun}, Args}} = fn_ast:call_mf(Mod, Fun, Args),
    {expr, Line, call, {{Mod, Fun}, Args}} = fn_ast:call_mf(Line, Mod, Fun, Args).

call_f_test(Config) ->
    Line = line,
    Fun = "fun",
    Args = 0,
    {expr, 1, call, {Fun, Args}} = fn_ast:call_f(Fun, Args),
    {expr, Line, call, {Fun, Args}} = fn_ast:call_f(Line, Fun, Args).

call_test(Config) ->
    Line = 0,
    MF = fun(X) -> X+1 end,
    Args = [arg1, arg2, arg3],
    {expr, 1, call, {MF, Args}} = fn_ast:call(MF, Args),
    {expr, Line, call, {MF, Args}} = fn_ast:call(Line, MF, Args).

e_when_else_test(Config) ->
    Line = 100,
    Body = body,
    {welse, 1, Body} = fn_ast:e_when_else(Body),
    {welse, Line, Body} = fn_ast:e_when_else(Line, Body).

e_when_cond_test(Config) ->
    Line = line,
    Cond = "cond",
    Body = ["body", 2, {last}],
    {wcond, 1, Cond, Body} = fn_ast:e_when_cond(Cond, Body),
    {wcond, Line, Cond, Body} = fn_ast:e_when_cond(Line, Cond, Body).

e_when_test(Config) ->
    Line = 23,
    Conds = [{cond1}, {cond2, 3}],
    {expr, 1, 'when', Conds} = fn_ast:e_when(Conds),
    {expr, Line, 'when', Conds} = fn_ast:e_when(Line, Conds).

e_case_else_test(Config) ->
    Line = 42,
    Body = body,
    {celse, Line, Body} = fn_ast:e_case_else(Line, Body).

e_case_match_test(Config) ->
    Line = {1},
    Match = "match",
    When = fun(X) -> X > 0 end,
    Body = body,
    {cmatch, Line, {Match, nowhen, Body}} = fn_ast:e_case_match(Line, Match, Body),
    {cmatch, Line, {Match, When, Body}} = fn_ast:e_case_match(Line, Match, When, Body).

e_case_test(Config) ->
    Line = 0,
    Matches = ["match1", "match2"],
    {expr, Line, 'case', Matches} = fn_ast:e_case(Line, Matches).

e_fn_test(Config) ->
    Line = {},
    Cases = [case1, case2, case3],
    {expr, Line, fn, Cases} = fn_ast:e_fn(Line, Cases).

s_cons_test(Config) ->
    Line = 7,
    Head = [head],
    Tail = {"tail"},
    {seq, 1, cons, {Head, Tail}} = fn_ast:s_cons(Head, Tail),
    {seq, Line, cons, {Head, Tail}} = fn_ast:s_cons(Line, Head, Tail).

s_map_test(Config) ->
    Line = 34,
    Val = {val},
    {seq, 1, map, Val} = fn_ast:s_map(Val),
    {seq, Line, map, Val} = fn_ast:s_map(Line, Val).

s_list_test(Config) ->
    Line = 927,
    Val = val,
    {seq, 1, list, Val} = fn_ast:s_list(Val),
    {seq, Line, list, Val} = fn_ast:s_list(Line, Val).

s_tuple_test(Config) ->
    Line = [true, false],
    Val = "val",
    {seq, 1, tuple, Val} = fn_ast:s_tuple(Val),
    {seq, Line, tuple, Val} = fn_ast:s_tuple(Line, Val).

v_var_test(Config) ->
    Line = 8,
    Val = false,
    {val, 1, var, Val} = fn_ast:v_var(Val),
    {val, Line, var, Val} = fn_ast:v_var(Line, Val).

v_fn_ref_test(Config) ->
    Line = 6,
    ModName = "modname",
    FName = "fname",
    Arity = 7,
    {val, Line, fn_ref, {FName, Arity}} = fn_ast:v_fn_ref(Line, FName, Arity),
    {val, Line, fn_ref, {{ModName, FName}, Arity}} = fn_ast:v_fn_ref(Line, ModName, FName, Arity).

v_atom_test(Config) ->
    Line = 11,
    Val = val,
    {val, 1, atom, Val} = fn_ast:v_atom(Val),
    {val, Line, atom, Val} = fn_ast:v_atom(Line, Val).

v_bool_test(Config) ->
    Line = -2,
    Val = true,
    {val, 1, boolean, Val} = fn_ast:v_bool(Val),
    {val, Line, boolean, Val} = fn_ast:v_bool(Line, Val).

v_str_test(Config) ->
    Line = 20139,
    Val = "val",
    {val, 1, string, Val} = fn_ast:v_str(Val),
    {val, Line, string, Val} = fn_ast:v_str(Line, Val).

v_float_test(Config) ->
    Line = 3,
    Val = 0.5,
    {val, 1, float, Val} = fn_ast:v_float(Val),
    {val, Line, float, Val} = fn_ast:v_float(Line, Val).

v_int_test(Config) ->
    Line = 10,
    Val = -7,
    {val, 1, integer, Val} = fn_ast:v_int(Val),
    {val, Line, integer, Val} = fn_ast:v_int(Line, Val).

unary_ops_test(Config) ->
    Line = 19,
    Val = 8,

    {unary_op, 1, '+', Val} = fn_ast:op_pos(Val),
    {unary_op, Line, '+', Val} = fn_ast:op_pos(Line, Val),

    {unary_op, 1, '-', Val} = fn_ast:op_neg(Val),
    {unary_op, Line, '-', Val} = fn_ast:op_neg(Line, Val),

    {unary_op, 1, '~', Val} = fn_ast:op_bnot(Val),
    {unary_op, Line, '~', Val} = fn_ast:op_bnot(Line, Val),

    {unary_op, 1, 'not', Val} = fn_ast:op_not(Val),
    {unary_op, Line, 'not', Val} = fn_ast:op_not(Line, Val).

comparison_ops_test(Config) ->
    Line = 11,
    Left = 0.8,
    Right = -242,

    {op, 1, 'isnt', Left, Right} = fn_ast:op_ene(Left, Right),
    {op, Line, 'isnt', Left, Right} = fn_ast:op_ene(Line, Left, Right),

    {op, 1, 'is', Left, Right} = fn_ast:op_eeq(Left, Right),
    {op, Line, 'is', Left, Right} = fn_ast:op_eeq(Line, Left, Right),

    {op, 1, '!=', Left, Right} = fn_ast:op_ne(Left, Right),
    {op, Line, '!=', Left, Right} = fn_ast:op_ne(Line, Left, Right),

    {op, 1, '==', Left, Right} = fn_ast:op_eq(Left, Right),
    {op, Line, '==', Left, Right} = fn_ast:op_eq(Line, Left, Right),

    {op, 1, '>=', Left, Right} = fn_ast:op_ge(Left, Right),
    {op, Line, '>=', Left, Right} = fn_ast:op_ge(Line, Left, Right),

    {op, 1, '>', Left, Right} = fn_ast:op_gt(Left, Right),
    {op, Line, '>', Left, Right} = fn_ast:op_gt(Line, Left, Right),

    {op, 1, '<=', Left, Right} = fn_ast:op_le(Left, Right),
    {op, Line, '<=', Left, Right} = fn_ast:op_le(Line, Left, Right),

    {op, 1, '<', Left, Right} = fn_ast:op_lt(Left, Right),
    {op, Line, '<', Left, Right} = fn_ast:op_lt(Line, Left, Right).

ladd_lsub_test(Config) ->
    Line = 36,
    Left = 0,
    Right = -1,

    {op, 1, '++', Left, Right} = fn_ast:op_ladd(Left, Right),
    {op, Line, '++', Left, Right} = fn_ast:op_ladd(Line, Left, Right),

    {op, 1, '--', Left, Right} = fn_ast:op_lsub(Left, Right),
    {op, Line, '--', Left, Right} = fn_ast:op_lsub(Line, Left, Right).

binary_ops_test(Config) ->
    Line = 70,
    Left = <<10,20>>,
    Right = <<"ABC">>,

    {op, 1, '>>', Left, Right} = fn_ast:op_shr(Left, Right),
    {op, Line, '>>', Left, Right} = fn_ast:op_shr(Line, Left, Right),

    {op, 1, '<<', Left, Right} = fn_ast:op_shl(Left, Right),
    {op, Line, '<<', Left, Right} = fn_ast:op_shl(Line, Left, Right),

    {op, 1, '^', Left, Right} = fn_ast:op_bxor(Left, Right),
    {op, Line, '^', Left, Right} = fn_ast:op_bxor(Line, Left, Right),

    {op, 1, '|', Left, Right} = fn_ast:op_bor(Left, Right),
    {op, Line, '|', Left, Right} = fn_ast:op_bor(Line, Left, Right),

    {op, 1, '&', Left, Right} = fn_ast:op_band(Left, Right),
    {op, Line, '&', Left, Right} = fn_ast:op_band(Line, Left, Right).

arithmetic_ops_test(Config) ->
    Line = 0,
    Left = 574,
    Right = 23,

    {op, 1, '%', Left, Right} = fn_ast:op_rem(Left, Right),
    {op, Line, '%', Left, Right} = fn_ast:op_rem(Line, Left, Right),

    {op, 1, '//', Left, Right} = fn_ast:op_idiv(Left, Right),
    {op, Line, '//', Left, Right} = fn_ast:op_idiv(Line, Left, Right),

    {op, 1, '/', Left, Right} = fn_ast:op_div(Left, Right),
    {op, Line, '/', Left, Right} = fn_ast:op_div(Line, Left, Right),

    {op, 1, '*', Left, Right} = fn_ast:op_mul(Left, Right),
    {op, Line, '*', Left, Right} = fn_ast:op_mul(Line, Left, Right),

    {op, 1, '-', Left, Right} = fn_ast:op_sub(Left, Right),
    {op, Line, '-', Left, Right} = fn_ast:op_sub(Line, Left, Right),

    {op, 1, '+', Left, Right} = fn_ast:op_add(Left, Right),
    {op, Line, '+', Left, Right} = fn_ast:op_add(Line, Left, Right).

logic_ops_test(Config) ->
    Line = 35,
    Left = true,
    Right = false,

    {op, 1, 'orr', Left, Right} = fn_ast:op_orr(Left, Right),
    {op, Line, 'orr', Left, Right} = fn_ast:op_orr(Line, Left, Right),

    {op, 1, 'andd', Left, Right} = fn_ast:op_andd(Left, Right),
    {op, Line, 'andd', Left, Right} = fn_ast:op_andd(Line, Left, Right),

    {op, 1, 'xor', Left, Right} = fn_ast:op_xor(Left, Right),
    {op, Line, 'xor', Left, Right} = fn_ast:op_xor(Line, Left, Right),

    {op, 1, 'or', Left, Right} = fn_ast:op_or(Left, Right),
    {op, Line, 'or', Left, Right} = fn_ast:op_or(Line, Left, Right),

    {op, 1, 'and', Left, Right} = fn_ast:op_and(Left, Right),
    {op, Line, 'and', Left, Right} = fn_ast:op_and(Line, Left, Right).

op_send_test(Config) ->
    Line = 13,
    Left = left,
    Right = right,
    {op, 1, '!', Left, Right} = fn_ast:op_send(Left, Right),
    {op, Line, '!', Left, Right} = fn_ast:op_send(Line, Left, Right).

op_match_test(Config) ->
    Line = 1324,
    Left = left,
    Right = right,
    {op, 1, '=', Left, Right} = fn_ast:op_match(Left, Right),
    {op, Line, '=', Left, Right} = fn_ast:op_match(Line, Left, Right).
