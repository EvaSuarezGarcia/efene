-module(fn_ast_SUITE).
-compile(export_all).
-include_lib("proper/include/proper.hrl").

-export([prop_unary_op/0]).
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

%% Auxiliary function for checking properties and making the test fail if the property fails
check_prop(Prop) ->
    Result = proper:quickcheck(Prop),
    if
        Result == false -> ct:fail(proper:counterexample());
        Result == true -> true;
        true -> ct:fail(Result)
    end.

unary_op_test(Config) ->
    check_prop(prop_unary_op()),
    check_prop(prop_unary_op2()).

prop_unary_op() ->
    ?FORALL({Op, Val}, {any(), any()},
    {unary_op, 1, Op, Val} == fn_ast:unary_op(Op, Val)).

prop_unary_op2() ->
    ?FORALL({Line, Op, Val}, {pos_integer(), any(), any()},
    {unary_op, Line, Op, Val} == fn_ast:unary_op(Op, Line, Val)).

op_test(Config) ->
    check_prop(prop_op()),
    check_prop(prop_op2()).

prop_op() ->
    ?FORALL({Op, Left, Right}, {atom(), any(), any()},
    {op, 1, Op, Left, Right} == fn_ast:op(Op, Left, Right)).

prop_op2() ->
    ?FORALL({Line, Op, Left, Right}, {pos_integer(), atom(), any(), any()},
    {op, Line, Op, Left, Right} == fn_ast:op(Op, Line, Left, Right)).

attr_test(Config) ->
    fn_ast:attr(1, "attr_test").

kv_test(Config) ->
    check_prop(prop_kv()),
    check_prop(prop_kv2()).

prop_kv() ->
    ?FORALL({Key, Val}, {any(), any()},
    {kv, 1, Key, Val} == fn_ast:kv(Key, Val)).

prop_kv2() ->
    ?FORALL({Line, Key, Val}, {pos_integer(), any(), any()},
    {kv, Line, Key, Val} == fn_ast:kv(Line, Key, Val)).

tag_test(Config) ->
    check_prop(prop_tag()),
    check_prop(prop_tag2()).

prop_tag() ->
    ?FORALL({Tag, Val}, {string(), any()},
    {tag, 1, Tag, Val} == fn_ast:tag(Tag, Val)).

prop_tag2() ->
    ?FORALL({Line, Tag, Val}, {pos_integer(), string(), any()},
    {tag, Line, Tag, Val} == fn_ast:tag(Line, Tag, Val)).

expr_test(Config) ->
    check_prop(prop_expr()).

prop_expr() ->
    ?FORALL({Line, Type, Args}, {pos_integer(), atom(), list()}, {expr, Line, Type, Args} == fn_ast:expr(Line, Type, Args)).

call_mf_test(Config) ->
    check_prop(prop_call_mf()),
    check_prop(prop_call_mf2()).

prop_call_mf() ->
    ?FORALL({Mod, Fun, Args}, {string(), string(), list()},
    {expr, 1, call, {{Mod, Fun}, Args}} == fn_ast:call_mf(Mod, Fun, Args)).

prop_call_mf2() ->
    ?FORALL({Line, Mod, Fun, Args}, {pos_integer(), string(), string(), list()},
    {expr, Line, call, {{Mod, Fun}, Args}} == fn_ast:call_mf(Line, Mod, Fun, Args)).

call_f_test(Config) ->
    check_prop(prop_call_f()),
    check_prop(prop_call_f2()).

prop_call_f() ->
    ?FORALL({Fun, Args}, {string(), any()},
    {expr, 1, call, {Fun, Args}} == fn_ast:call_f(Fun, Args)).

prop_call_f2() ->
    ?FORALL({Line, Fun, Args}, {pos_integer(), string(), any()},
    {expr, Line, call, {Fun, Args}} == fn_ast:call_f(Line, Fun, Args)).

call_test(Config) ->
    check_prop(prop_call()),
    check_prop(prop_call2()).

prop_call() ->
    ?FORALL({MF, Args}, {string(), list()},
    {expr, 1, call, {MF, Args}} == fn_ast:call(MF, Args)).

prop_call2() ->
    ?FORALL({Line, MF, Args}, {pos_integer(), string(), list()},
    {expr, Line, call, {MF, Args}} == fn_ast:call(Line, MF, Args)).

e_when_else_test(Config) ->
    check_prop(prop_e_when_else()),
    check_prop(prop_e_when_else2()).

prop_e_when_else() ->
    ?FORALL({Body}, {any()},
    {welse, 1, Body} == fn_ast:e_when_else(Body)).

prop_e_when_else2() ->
    ?FORALL({Line, Body}, {pos_integer(), any()},
    {welse, Line, Body} == fn_ast:e_when_else(Line, Body)).

e_when_cond_test(Config) ->
    check_prop(prop_e_when_cond()),
    check_prop(prop_e_when_cond2()).

prop_e_when_cond() ->
    ?FORALL({Cond, Body}, {any(), any()},
    {wcond, 1, Cond, Body} == fn_ast:e_when_cond(Cond, Body)).

prop_e_when_cond2() ->
    ?FORALL({Line, Cond, Body}, {pos_integer(), any(), any()},
    {wcond, Line, Cond, Body} == fn_ast:e_when_cond(Line, Cond, Body)).

e_when_test(Config) ->
    check_prop(prop_e_when()),
    check_prop(prop_e_when2()).

prop_e_when() ->
    ?FORALL({Conds}, {list()},
    {expr, 1, 'when', Conds} == fn_ast:e_when(Conds)).

prop_e_when2() ->
    ?FORALL({Line, Conds}, {pos_integer(), list()},
    {expr, Line, 'when', Conds} == fn_ast:e_when(Line, Conds)).

e_case_else_test(Config) ->
    check_prop(prop_e_case_else()).

prop_e_case_else() ->
    ?FORALL({Line, Body}, {pos_integer(), any()}, {celse, Line, Body} == fn_ast:e_case_else(Line, Body)).

e_case_match_test(Config) ->
    check_prop(prop_e_case_match()),
    check_prop(prop_e_case_match2()).

prop_e_case_match() ->
    ?FORALL({Line, Match, Body}, {pos_integer(), any(), any()},
    {cmatch, Line, {Match, nowhen, Body}} == fn_ast:e_case_match(Line, Match, Body)).

prop_e_case_match2() ->
    ?FORALL({Line, Match, When, Body}, {pos_integer(), any(), any(), any()},
    {cmatch, Line, {Match, When, Body}} == fn_ast:e_case_match(Line, Match, When, Body)).

e_case_test(Config) ->
    check_prop(prop_e_case()).

prop_e_case() ->
    ?FORALL({Line, Matches}, {pos_integer(), list()}, {expr, Line, 'case', Matches} == fn_ast:e_case(Line, Matches)).

e_fn_test(Config) ->
    check_prop(prop_e_fn()).

prop_e_fn() ->
    ?FORALL({Line, Cases}, {pos_integer(), list()}, {expr, Line, fn, Cases} == fn_ast:e_fn(Line, Cases)).

s_cons_test(Config) ->
    check_prop(prop_s_cons()),
    check_prop(prop_s_cons2()).

prop_s_cons() ->
    ?FORALL({Head, Tail}, {any(), list()},
    {seq, 1, cons, {Head, Tail}} == fn_ast:s_cons(Head, Tail)).

prop_s_cons2() ->
    ?FORALL({Line, Head, Tail}, {pos_integer(), any(), list()},
    {seq, Line, cons, {Head, Tail}} == fn_ast:s_cons(Line, Head, Tail)).

s_map_test(Config) ->
    check_prop(prop_s_map()),
    check_prop(prop_s_map2()).

prop_s_map() ->
    ?FORALL({Val}, {any()},
    {seq, 1, map, Val} == fn_ast:s_map(Val)).

prop_s_map2() ->
    ?FORALL({Line, Val}, {pos_integer(), any()},
    {seq, Line, map, Val} == fn_ast:s_map(Line, Val)).

s_list_test(Config) ->
    check_prop(prop_s_list()),
    check_prop(prop_s_list2()).

prop_s_list() ->
    ?FORALL({Val}, {list()},
    {seq, 1, list, Val} == fn_ast:s_list(Val)).

prop_s_list2() ->
    ?FORALL({Line, Val}, {pos_integer(), list()},
    {seq, Line, list, Val} == fn_ast:s_list(Line, Val)).

s_tuple_test(Config) ->
    check_prop(prop_s_tuple()),
    check_prop(prop_s_tuple2()).

prop_s_tuple() ->
    ?FORALL({Val}, {tuple()},
    {seq, 1, tuple, Val} == fn_ast:s_tuple(Val)).

prop_s_tuple2() ->
    ?FORALL({Line, Val}, {pos_integer(), tuple()},
    {seq, Line, tuple, Val} == fn_ast:s_tuple(Line, Val)).

v_var_test(Config) ->
    check_prop(prop_v_var()),
    check_prop(prop_v_var2()).

prop_v_var() ->
    ?FORALL({Val}, {any()},
    {val, 1, var, Val} == fn_ast:v_var(Val)).

prop_v_var2() ->
    ?FORALL({Line, Val}, {pos_integer(), any()},
    {val, Line, var, Val} == fn_ast:v_var(Line, Val)).

v_fn_ref_test(Config) ->
    check_prop(prop_v_fn_ref()),
    check_prop(prop_v_fn_ref2()).

prop_v_fn_ref() ->
    ?FORALL({Line,FName, Arity}, {pos_integer(), string(), nat()},
    {val, Line, fn_ref, {FName, Arity}} == fn_ast:v_fn_ref(Line, FName, Arity)).

prop_v_fn_ref2() ->
    ?FORALL({Line, ModName, FName, Arity}, {pos_integer(), string(), string(), nat()},
    {val, Line, fn_ref, {{ModName, FName}, Arity}} == fn_ast:v_fn_ref(Line, ModName, FName, Arity)).

v_atom_test(Config) ->
    check_prop(prop_v_atom()),
    check_prop(prop_v_atom2()).

prop_v_atom() ->
    ?FORALL({Val}, {bool()},
    {val, 1, atom, Val} == fn_ast:v_atom(Val)).

prop_v_atom2() ->
    ?FORALL({Line, Val}, {pos_integer(), bool()},
    {val, Line, atom, Val} == fn_ast:v_atom(Line, Val)).

v_bool_test(Config) ->
    check_prop(prop_v_bool()),
    check_prop(prop_v_bool2()).

prop_v_bool() ->
    ?FORALL({Val}, {bool()},
    {val, 1, boolean, Val} == fn_ast:v_bool(Val)).

prop_v_bool2() ->
    ?FORALL({Line, Val}, {pos_integer(), bool()},
    {val, Line, boolean, Val} == fn_ast:v_bool(Line, Val)).

v_str_test(Config) ->
    check_prop(prop_v_str()),
    check_prop(prop_v_str2()).

prop_v_str() ->
    ?FORALL({Val}, {string()},
    {val, 1, string, Val} == fn_ast:v_str(Val)).

prop_v_str2() ->
    ?FORALL({Line, Val}, {pos_integer(), string()},
    {val, Line, string, Val} == fn_ast:v_str(Line, Val)).

v_float_test(Config) ->
    check_prop(prop_v_float()),
    check_prop(prop_v_float2()).

prop_v_float() ->
    ?FORALL({Val}, {float()},
    {val, 1, float, Val} == fn_ast:v_float(Val)).

prop_v_float2() ->
    ?FORALL({Line, Val}, {pos_integer(), float()},
    {val, Line, float, Val} == fn_ast:v_float(Line, Val)).

v_int_test(Config) ->
    check_prop(prop_v_int()),
    check_prop(prop_v_int2()).

prop_v_int() ->
    ?FORALL({Val}, {integer()},
    {val, 1, integer, Val} == fn_ast:v_int(Val)).

prop_v_int2() ->
    ?FORALL({Line, Val}, {pos_integer(), integer()},
    {val, Line, integer, Val} == fn_ast:v_int(Line, Val)).

unary_ops_test(Config) ->
    check_prop(prop_unary_pos()),
    check_prop(prop_unary_pos2()),
    check_prop(prop_unary_neg()),
    check_prop(prop_unary_neg2()),
    check_prop(prop_unary_bnot()),
    check_prop(prop_unary_bnot2()),
    check_prop(prop_unary_not()),
    check_prop(prop_unary_not2()).

prop_unary_pos() ->
    ?FORALL({Val}, {any()},
    {unary_op, 1, '+', Val} == fn_ast:op_pos(Val)).

prop_unary_pos2() ->
    ?FORALL({Line, Val}, {pos_integer(), any()},
    {unary_op, Line, '+', Val} == fn_ast:op_pos(Line, Val)).

prop_unary_neg() ->
    ?FORALL({Val}, {any()},
    {unary_op, 1, '-', Val} == fn_ast:op_neg(Val)).

prop_unary_neg2() ->
    ?FORALL({Line, Val}, {pos_integer(), any()},
    {unary_op, Line, '-', Val} == fn_ast:op_neg(Line, Val)).

prop_unary_bnot() ->
    ?FORALL({Val}, {any()},
    {unary_op, 1, '~', Val} == fn_ast:op_bnot(Val)).

prop_unary_bnot2() ->
    ?FORALL({Line, Val}, {pos_integer(), any()},
    {unary_op, Line, '~', Val} == fn_ast:op_bnot(Line, Val)).

prop_unary_not() ->
    ?FORALL({Val}, {any()},
    {unary_op, 1, 'not', Val} == fn_ast:op_not(Val)).

prop_unary_not2() ->
    ?FORALL({Line, Val}, {pos_integer(), any()},
    {unary_op, Line, 'not', Val} == fn_ast:op_not(Line, Val)).

comparison_ops_test(Config) ->
    check_prop(prop_comparison_ops_ene()),
    check_prop(prop_comparison_ops_ene2()),
    check_prop(prop_comparison_ops_eeq()),
    check_prop(prop_comparison_ops_eeq2()),
    check_prop(prop_comparison_ops_ne()),
    check_prop(prop_comparison_ops_ne2()),
    check_prop(prop_comparison_ops_eq()),
    check_prop(prop_comparison_ops_eq2()),
    check_prop(prop_comparison_ops_ge()),
    check_prop(prop_comparison_ops_ge2()),
    check_prop(prop_comparison_ops_gt()),
    check_prop(prop_comparison_ops_gt2()),
    check_prop(prop_comparison_ops_le()),
    check_prop(prop_comparison_ops_le2()),
    check_prop(prop_comparison_ops_lt()),
    check_prop(prop_comparison_ops_lt2()).

prop_comparison_ops_ene() ->
    ?FORALL({Left, Right}, {any(), any()},
    {op, 1, 'isnt', Left, Right} == fn_ast:op_ene(Left, Right)).

prop_comparison_ops_ene2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), any(), any()},
    {op, Line, 'isnt', Left, Right} == fn_ast:op_ene(Line, Left, Right)).

prop_comparison_ops_eeq() ->
    ?FORALL({Left, Right}, {any(), any()},
    {op, 1, 'is', Left, Right} == fn_ast:op_eeq(Left, Right)).

prop_comparison_ops_eeq2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), any(), any()},
    {op, Line, 'is', Left, Right} == fn_ast:op_eeq(Line, Left, Right)).

prop_comparison_ops_ne() ->
    ?FORALL({Left, Right}, {any(), any()},
    {op, 1, '!=', Left, Right} == fn_ast:op_ne(Left, Right)).

prop_comparison_ops_ne2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), any(), any()},
    {op, Line, '!=', Left, Right} == fn_ast:op_ne(Line, Left, Right)).

prop_comparison_ops_eq() ->
    ?FORALL({Left, Right}, {any(), any()},
    {op, 1, '==', Left, Right} == fn_ast:op_eq(Left, Right)).

prop_comparison_ops_eq2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), any(), any()},
    {op, Line, '==', Left, Right} == fn_ast:op_eq(Line, Left, Right)).

prop_comparison_ops_ge() ->
    ?FORALL({Left, Right}, {any(), any()},
    {op, 1, '>=', Left, Right} == fn_ast:op_ge(Left, Right)).

prop_comparison_ops_ge2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), any(), any()},
    {op, Line, '>=', Left, Right} == fn_ast:op_ge(Line, Left, Right)).

prop_comparison_ops_gt() ->
    ?FORALL({Left, Right}, {any(), any()},
    {op, 1, '>', Left, Right} == fn_ast:op_gt(Left, Right)).

prop_comparison_ops_gt2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), any(), any()},
    {op, Line, '>', Left, Right} == fn_ast:op_gt(Line, Left, Right)).

prop_comparison_ops_le() ->
    ?FORALL({Left, Right}, {any(), any()},
    {op, 1, '<=', Left, Right} == fn_ast:op_le(Left, Right)).

prop_comparison_ops_le2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), any(), any()},
    {op, Line, '<=', Left, Right} == fn_ast:op_le(Line, Left, Right)).

prop_comparison_ops_lt() ->
    ?FORALL({Left, Right}, {any(), any()},
    {op, 1, '<', Left, Right} == fn_ast:op_lt(Left, Right)).

prop_comparison_ops_lt2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), any(), any()},
    {op, Line, '<', Left, Right} == fn_ast:op_lt(Line, Left, Right)).

ladd_lsub_test(Config) ->
    check_prop(prop_ladd()),
    check_prop(prop_ladd2()),
    check_prop(prop_lsub()),
    check_prop(prop_lsub2()).

prop_ladd() ->
    ?FORALL({Left, Right}, {list(), list()},
    {op, 1, '++', Left, Right} == fn_ast:op_ladd(Left, Right)).

prop_ladd2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), list(), list()},
    {op, Line, '++', Left, Right} == fn_ast:op_ladd(Line, Left, Right)).

prop_lsub() ->
    ?FORALL({Left, Right}, {list(), list()},
    {op, 1, '--', Left, Right} == fn_ast:op_lsub(Left, Right)).

prop_lsub2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), list(), list()},
    {op, Line, '--', Left, Right} == fn_ast:op_lsub(Line, Left, Right)).

binary_ops_test(Config) ->
    check_prop(prop_binary_ops_shr()),
    check_prop(prop_binary_ops_shr2()),
    check_prop(prop_binary_ops_shl()),
    check_prop(prop_binary_ops_shl2()),
    check_prop(prop_binary_ops_bxor()),
    check_prop(prop_binary_ops_bxor2()),
    check_prop(prop_binary_ops_bor()),
    check_prop(prop_binary_ops_bor2()),
    check_prop(prop_binary_ops_band()),
    check_prop(prop_binary_ops_band2()).

prop_binary_ops_shr() ->
    ?FORALL({Left, Right}, {binary(), binary()},
    {op, 1, '>>', Left, Right} == fn_ast:op_shr(Left, Right)).

prop_binary_ops_shr2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), binary(), binary()},
    {op, Line, '>>', Left, Right} == fn_ast:op_shr(Line, Left, Right)).

prop_binary_ops_shl() ->
    ?FORALL({Left, Right}, {binary(), binary()},
    {op, 1, '<<', Left, Right} == fn_ast:op_shl(Left, Right)).

prop_binary_ops_shl2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), binary(), binary()},
    {op, Line, '<<', Left, Right} == fn_ast:op_shl(Line, Left, Right)).

prop_binary_ops_bxor() ->
    ?FORALL({Left, Right}, {binary(), binary()},
    {op, 1, '^', Left, Right} == fn_ast:op_bxor(Left, Right)).

prop_binary_ops_bxor2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), binary(), binary()},
    {op, Line, '^', Left, Right} == fn_ast:op_bxor(Line, Left, Right)).

prop_binary_ops_bor() ->
    ?FORALL({Left, Right}, {binary(), binary()},
    {op, 1, '|', Left, Right} == fn_ast:op_bor(Left, Right)).

prop_binary_ops_bor2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), binary(), binary()},
    {op, Line, '|', Left, Right} == fn_ast:op_bor(Line, Left, Right)).

prop_binary_ops_band() ->
    ?FORALL({Left, Right}, {binary(), binary()},
    {op, 1, '&', Left, Right} == fn_ast:op_band(Left, Right)).

prop_binary_ops_band2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), binary(), binary()},
    {op, Line, '&', Left, Right} == fn_ast:op_band(Line, Left, Right)).

arithmetic_ops_test(Config) ->
    check_prop(prop_arithmetic_ops_rem()),
    check_prop(prop_arithmetic_ops_rem2()),
    check_prop(prop_arithmetic_ops_idiv()),
    check_prop(prop_arithmetic_ops_idiv2()),
    check_prop(prop_arithmetic_ops_div()),
    check_prop(prop_arithmetic_ops_div2()),
    check_prop(prop_arithmetic_ops_mul()),
    check_prop(prop_arithmetic_ops_mul2()),
    check_prop(prop_arithmetic_ops_sub()),
    check_prop(prop_arithmetic_ops_sub2()),
    check_prop(prop_arithmetic_ops_add()),
    check_prop(prop_arithmetic_ops_add2()).

prop_arithmetic_ops_rem() ->
    ?FORALL({Left, Right}, {integer(), integer()},
    {op, 1, '%', Left, Right} == fn_ast:op_rem(Left, Right)).

prop_arithmetic_ops_rem2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), integer(), integer()},
    {op, Line, '%', Left, Right} == fn_ast:op_rem(Line, Left, Right)).

prop_arithmetic_ops_idiv() ->
    ?FORALL({Left, Right}, {integer(), integer()},
    {op, 1, '//', Left, Right} == fn_ast:op_idiv(Left, Right)).

prop_arithmetic_ops_idiv2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), integer(), integer()},
    {op, Line, '//', Left, Right} == fn_ast:op_idiv(Line, Left, Right)).

prop_arithmetic_ops_div() ->
    ?FORALL({Left, Right}, {number(), number()},
    {op, 1, '/', Left, Right} == fn_ast:op_div(Left, Right)).

prop_arithmetic_ops_div2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), number(), number()},
    {op, Line, '/', Left, Right} == fn_ast:op_div(Line, Left, Right)).

prop_arithmetic_ops_mul() ->
    ?FORALL({Left, Right}, {number(), number()},
    {op, 1, '*', Left, Right} == fn_ast:op_mul(Left, Right)).

prop_arithmetic_ops_mul2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), number(), number()},
    {op, Line, '*', Left, Right} == fn_ast:op_mul(Line, Left, Right)).

prop_arithmetic_ops_sub() ->
    ?FORALL({Left, Right}, {number(), number()},
    {op, 1, '-', Left, Right} == fn_ast:op_sub(Left, Right)).

prop_arithmetic_ops_sub2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), number(), number()},
    {op, Line, '-', Left, Right} == fn_ast:op_sub(Line, Left, Right)).

prop_arithmetic_ops_add() ->
    ?FORALL({Left, Right}, {number(), number()},
    {op, 1, '+', Left, Right} == fn_ast:op_add(Left, Right)).

prop_arithmetic_ops_add2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), number(), number()},
    {op, Line, '+', Left, Right} == fn_ast:op_add(Line, Left, Right)).

logic_ops_test(Config) ->
    check_prop(prop_logic_ops_orr()),
    check_prop(prop_logic_ops_orr2()),
    check_prop(prop_logic_ops_andd()),
    check_prop(prop_logic_ops_andd2()),
    check_prop(prop_logic_ops_xor()),
    check_prop(prop_logic_ops_xor2()),
    check_prop(prop_logic_ops_or()),
    check_prop(prop_logic_ops_or2()),
    check_prop(prop_logic_ops_and()),
    check_prop(prop_logic_ops_and2()).

prop_logic_ops_orr() ->
    ?FORALL({Left, Right}, {any(), any()},
    {op, 1, 'orr', Left, Right} == fn_ast:op_orr(Left, Right)).

prop_logic_ops_orr2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), any(), any()},
    {op, Line, 'orr', Left, Right} == fn_ast:op_orr(Line, Left, Right)).

prop_logic_ops_andd() ->
    ?FORALL({Left, Right}, {any(), any()},
    {op, 1, 'andd', Left, Right} == fn_ast:op_andd(Left, Right)).

prop_logic_ops_andd2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), any(), any()},
    {op, Line, 'andd', Left, Right} == fn_ast:op_andd(Line, Left, Right)).

prop_logic_ops_xor() ->
    ?FORALL({Left, Right}, {any(), any()},
    {op, 1, 'xor', Left, Right} == fn_ast:op_xor(Left, Right)).

prop_logic_ops_xor2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), any(), any()},
    {op, Line, 'xor', Left, Right} == fn_ast:op_xor(Line, Left, Right)).

prop_logic_ops_or() ->
    ?FORALL({Left, Right}, {any(), any()},
    {op, 1, 'or', Left, Right} == fn_ast:op_or(Left, Right)).

prop_logic_ops_or2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), any(), any()},
    {op, Line, 'or', Left, Right} == fn_ast:op_or(Line, Left, Right)).

prop_logic_ops_and() ->
    ?FORALL({Left, Right}, {any(), any()},
    {op, 1, 'and', Left, Right} == fn_ast:op_and(Left, Right)).

prop_logic_ops_and2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), any(), any()},
    {op, Line, 'and', Left, Right} == fn_ast:op_and(Line, Left, Right)).

op_send_test(Config) ->
    check_prop(prop_send()),
    check_prop(prop_send2()).

prop_send() ->
    ?FORALL({Left, Right}, {atom(), atom()},
    {op, 1, '!', Left, Right} == fn_ast:op_send(Left, Right)).

prop_send2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), atom(), atom()},
    {op, Line, '!', Left, Right} == fn_ast:op_send(Line, Left, Right)).

op_match_test(Config) ->
    check_prop(prop_match()),
    check_prop(prop_match2()).

prop_match() ->
    ?FORALL({Left, Right}, {any(), any()},
    {op, 1, '=', Left, Right} == fn_ast:op_match(Left, Right)).

prop_match2() ->
    ?FORALL({Line, Left, Right}, {pos_integer(), any(), any()},
    {op, Line, '=', Left, Right} == fn_ast:op_match(Line, Left, Right)).
