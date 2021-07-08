
## add/3

Prolog:

    add(0, S, S).
    add(s(A), B, s(S)) :-
        add(A, B, S).

WAM:

    add/3:
        switch_on_term var:#1 const:{0:#1.1} struct:{s/1:#2.1}
    #1:
        try_me_else #2
    #1.1:   
        get_const 0, X0
        get_value X2, X1
        proceed
    #2:
        trust_me
    #2.1:
        get_struct s/1, X0
            unify_variable X0
        get_struct s/1, X2
            unify_variable X2
        execute add/3

Python:

    class M1(Machine):
        def add_3(self, a, b, s):
            def _1():
                self.try_me_else(_2)
                _1_1()

            def _2():
                self.trust_me()
                _2_1()

            def _1_1():
                self.unify(a, 0)
                self.unify(b, s)

            def _2_1():
                _a, _s = Var('A'), Var('S')
                self.unify(a, ('s', _a))
                self.unify(s, ('s', _s))
                self.add(_a, b, _s)
                
            if is_var(a):
                _1()
                return
            if is_const(a):
                if a == 0:
                    _1_1()
                    return
            if is_struct(a):
                if a.indicator == ('s', 1):
                    _2_1()
                    return
            raise TermNotIndexed('add/3', a)

## mul/3

Prolog:

    mul(0, _, 0).
    mul(s(A), B, P) :-
        mul(A, B, P1),
        add(P1, A, P).
WAM:

    mul/3:
        switch_on_term var:#1 const:{0:#1.1} struct:{s/1:#2.1}
    #1:
        try_me_else #2
    #1.1:
        get_const 0, X0
        get_const 0, X2
        proceed
    #2:
        trust_me
    #2.1:
        get_struct s/1, X0
            unify_variable Y0  % A in Y0
        get_variable Y1, X2    % P in Y1
        put_value Y0, X0
        put_variable Y2, X2i   % P1 in Y2
        call mul/3
        put_value Y2, X0
        put_value Y0, X1
        put_value Y1, X2
        execute add/3

Python:

    class M2(Machine):
        def mul_3(self, a, b, p):
            def _1():
                self.try_me_else(_2)
                _1_1()

            def _2():
                self.trust_me()
                _2_1()

            def _1_1():
                self.unify(a, 0)
                self.unify(p, 0)

            def _2_1():
                _a, _p1 = Var('A'), Var('P1')
                self.unify(a, ('s', _a))
                self.mul_3(_a, b, _p1)
                self.add(_p1, _a, p)
                    
            if is_var(a):
                _1()
                return
            if is_const(a):
                if a == 0:
                    _1_1()
                    return
            if is_struct(a):
                if a.indicator == ('s', 1):
                    _2_1()
                    return
            raise TermNotIndexed('mul/3', a)

## member/2

Prolog:

    member(E, [H|T]) :-
        member_(T, E, H).

    member_(_, E, E).
    member_([H|T], E, _) :-
        member_(T, E, H).

WAM:

    member/2:
        get_struct ./2, X1
            unify_variable X2
            unify_variable X3
        put_value X0, X1
        put_value X3, X0
        execute member_/3

    member_/3:
    #1:
        try_me_else #2
        get_value X1, X2
        proceed
    #2:
        trust_me
        get_struct ./2, X0
            unify_variable X2
            unify_variable X0
        execute member_/3

Python:

    class M3(Machine):
        def member_2(self, e, l):
            _h, _t = Var('H'), Var('T')
            self.unify(l, ('.', _h, _t))
            self.member__3(t, e, h)

        def member__3(self, t, e, h):
            def _1():
                self.try_me_else(_2)
                self.unify(e, h)

            def _2():
                self.trust_me()
                _h, _t = Var('H'), Var('T')
                self.unify(t, ('.', _h, _t))
                self.member__3(_t, e, _h)

            _1()
