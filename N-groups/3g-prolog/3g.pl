ident(e).

inv(e,e).
inv(a,a).
inv(b,b).

inG(e). 
inG(a).
inG(b).

op3(a,e,b,b).
op3(b,e,a,a).

gop(A,B,C) :- inG(A), inG(B), inG(C).

invprop(A,B,C) :- gop(A,B,C), ident(A), inv(B,C).
invprop(A,B,C) :- gop(A,B,C), ident(B), inv(A,C).
invprop(A,B,C) :- gop(A,B,C), ident(C), inv(A,B).

invprop(A,B,C) :- gop(A,B,C), ident(A), inv(C,B).
invprop(A,B,C) :- gop(A,B,C), ident(B), inv(C,A).
invprop(A,B,C) :- gop(A,B,C), ident(C), inv(B,A).

identprop(gop(A,X,Y),A) :- ident(X), ident(Y).
identprop(gop(X,A,Y),A) :- ident(X), ident(Y).
identprop(gop(X,Y,A),A) :- ident(X), ident(Y).

assocprop(gop(A,B,C),gop(D,E,F),gop(G,H,I)) :- gop(gop(gop(A,B,C),D,G),gop(e,E,F),gop(e,H,I)).

% ident prop
op3(A,B,C,R) :- gop(A,B,C), ident(A), ident(B), R=C.
op3(A,B,C,R) :- gop(A,B,C), ident(A), ident(C), R=B.
op3(A,B,C,R) :- gop(A,B,C), ident(C), ident(B), R=A.

% inv prop
op3(A,B,C,R) :- gop(A,B,C), ident(A), inv(B,C), R=e.
op3(A,B,C,R) :- gop(A,B,C), ident(B), inv(A,C), R=e.
op3(A,B,C,R) :- gop(A,B,C), ident(C), inv(B,A), R=e.
op3(A,B,C,R) :- gop(A,B,C), ident(A), inv(C,B), R=e.
op3(A,B,C,R) :- gop(A,B,C), ident(B), inv(C,A), R=e.
op3(A,B,C,R) :- gop(A,B,C), ident(C), inv(A,B), R=e.

% assoc prop
% op3(op3(A,B,C,R1),op3(D,E,F,_),op3(G,H,I,_),R4) :- op3(op3(op3(A,B,C,R1),D,G,RT1),op3(e,E,F,RT2),op3(e,H,I,RT3),R4), op3(RT1,RT2,RT3,R4).

op3(op3(_,_,_,R1),op3(_,_,_,R2),op3(_,_,_,R3),R4) :- gop(R1,R2,R3), op3(R1,R2,R3,R4).
op3(op3(_,_,_,R1),op3(D,E,F,R2),op3(G,H,I,R3),R4) :- gop(R1,R2,R3), op3(op3(R1,D,G,RT1),op3(e,E,F,RT2),op3(e,H,I,RT3),RT4), op3(RT1,RT2,RT3,RT4), R4=RT4.
op3(op3(A,B,C,R1),op3(_,_,_,R2),op3(G,H,I,R3),R4) :- gop(R1,R2,R3), op3(op3(A,e,C,RT1),op3(B,R2,H,RT2),op3(G,e,I,RT3),RT4), op3(RT1,RT2,RT3,RT4), R4=RT4.
op3(op3(A,B,C,R1),op3(D,E,F,R2),op3(_,_,_,R3),R4) :- gop(R1,R2,R3), op3(op3(A,B,e,RT1),op3(D,E,e,RT2),op3(C,F,R3,RT3),RT4), op3(RT1,RT2,RT3,RT4), R4=RT4.
