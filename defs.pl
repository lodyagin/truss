:- module(defs,[
                ]).

:- use_module(matrix).

el(fd23, straight(L), [fwd(L)]) :-
   !, memberchk(L, 250, 500, 750,
                   1000, 1250, 1500, 1750,
                   2000, 2500, 3000, 3500, 4000).
el(fd23, c21, [fwd(500-w/2), turn(z, -90), fwd(500-w/2)]) :- !.
el(fd23, c24, [fwd(500-w/2), turn(y, -90), fwd(500-w/2)]) :- !.
el(fd23, c25, [fwd(500-w/2), turn(y, 90), fwd(500-w/2)]) :- !.
%el(fd23, c31, [rotate(180), fwd(500),
%         [[turn(h, 90), fwd(500)],
%          [turn(v, -90), rotate(-45)]]]).
%el(fd23, c42, [[fwd(500)],
%         [fwd(250 + w/2), turn(h, 90), fwd(500)],
%         [fwd(250 + w/2), turn(v, -90), rotate(-90), fwd(500)]]).

% conn_pars(Type, Width, Height, MainPipeDiameter).
conn_pars(fd23, 220, 195, 35).

conn_shape(fd23, s(p(0, Y1, ZB), p(0, Y2, ZB), p(0, 0, Z3))) :-
   conn_pars(fd23, W, H, D), !,
   Y2 is (W - D) / 2,
   Y1 is - Y2,
   Z3 is (H - D) / 2,
   ZB is - Z3.

transform(Matrix, p(X0, Y0, Z0), p(X, Y, Z)) :-
   !, matrix_multiply(Matrix, [[X0], [Y0], [Z0], [1]], [[X1], [Y1], [Z1], [Mult]]),
   X is X1 / Mult,
   Y is Y1 / Mult,
   Z is Z1 / Mult.
transform(Matrix, s(A0, B0, C0), s(A, B, C)) :-
   !, transform(Matrix, A0, A),
   transform(Matrix, B0, B),
   transform(Matrix, C0, C).

eval_expr(Type, w, Width) :- 
   !, conn_pars(Type, Width, _, _), !.
eval_expr(_, -Val, Val) :- !.
eval_expr(_, Val, Val) :-
   number(Val), !.
eval_expr(Type, Op2, Val) :-
   Op2 =.. [Op, ExprA, ExprB], !,
   eval_expr(Type, ExprA, ValA),
   eval_expr(Type, ExprB, ValB),
   Op21 =.. [Op, ValA, ValB],
   Val is Op21.

calc_matrix(Type, Ops, M) :-
   Diagonal = [[1, 0, 0, 0],
               [0, 1, 0, 0],
               [0, 0, 1, 0],
               [0, 0, 0, 1]],
   calc_matrix(Type, Ops, Diagonal, M).

calc_matrix(_, [], M, M) :- !.
calc_matrix(Type, [Op|Ops], M0, M) :-
   matrix(Type, Op, M1),
   matrix_multiply(M0, M1, M2),
   calc_matrix(Type, Ops, M2, M).

matrix(Type, fwd(D), M) :-
   !, eval_expr(Type, D, DVal),
   matrix_translate(v(-DVal, 0, 0), M).
matrix(_, turn(Axis, Deg), M) :-
   !, Rad is Deg / 180 * pi,
   matrix_rotate(Axis, Rad, M).

% A translate transofrmatino
matrix_translate(v(DX, DY, DZ),
                 [[1, 0, 0, DX],
                  [0, 1, 0, DY],
                  [0, 0, 1, DZ],
                  [0, 0, 0, 1]]).

matrix_rotate(y, Rad,
              [[Cos,  0,  Sin, 0],
               [0,    1,  0,   0],
               [MSin, 0,  Cos, 0],
               [0,    0,  0,   1]]) :-
   Cos is cos(Rad),
   Sin is sin(Rad),
   MSin is -Sin.
matrix_rotate(z, Rad,
              [[Cos,  MSin, 0, 0],
               [Sin,  Cos,  0, 0],
               [0,    0,    1, 0],
               [0,    0,    0, 1]]) :-
   Cos is cos(Rad),
   Sin is sin(Rad),
   MSin is -Sin.

