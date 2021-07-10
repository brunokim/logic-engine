from collections import defaultdict, Counter

try:
    from pytest import mark
    parametrize = mark.parametrize
except ImportError:
    # On pdb, pytest is not available, so we stub parametrize.
    def parametrize(args, data):
        print("@parametrize stub")
        return lambda f: f


def is_var(term):
    return isinstance(term, str) and term and (term[0].isupper() or term[0] == '_')


def is_atom(term):
    return isinstance(term, str) and not is_var(term)


def is_comp(term):
    return isinstance(term, tuple) and term and is_atom(term[0])


def is_clause(term):
    return isinstance(term, list) and term and all(is_comp(t) for t in term)


def indicator(comp):
    return f"{comp[0]}/{len(comp)-1}"


def count_vars(term):
    c = Counter()

    def count(t):
        if is_var(t):
            c[t] += 1
        if is_comp(t) or isinstance(t, list):
            for arg in t:
                count(arg)

    count(term)
    return c


def count_comps(term):
    c = 0

    def count(t):
        nonlocal c
        if is_comp(t):
            c += 1
            for arg in t:
                count(arg)
    count(term)
    return c


def gen_vars(term):
    c = count_vars(term)
    return c.keys()


inlined = {
    "!/0",
    "=/2",
    "</2",
    ">/2",
    "=</2",
    ">=/2",
    "==/2",
    "\\==/2",
}


def is_inlined(term):
    if not is_comp(term):
        return false
    ind = indicator(term)
    return ind in inlined


def gen_chunks(clause):
    assert is_clause(clause), f'Not a clause: {clause}'
    head, *body = clause
    chunk = [head]
    for term in body:
        chunk.append(term)
        if is_inlined(term):
            continue
        yield chunk
        chunk = []
    if chunk:
        yield chunk


def clause_chunks(clause):
    assert is_clause(clause), f'Not a clause: {clause}'
    chunks = list(gen_chunks(clause))
    chunk_vars = [list(gen_vars(chunk)) for chunk in chunks]
    var_chunks = defaultdict(list)
    for i, xs in enumerate(chunk_vars):
        for x in xs:
            var_chunks[x].append(i)
    temps, perms = [], []
    for x, chunk_idxs in var_chunks.items():
        if len(chunk_idxs) == 1:
            temps.append(x)
        else:
            perms.append(x)
    return {'temps': temps, 'perms': perms, 'chunks': chunks}


def count_nested_comps(chunk):
    n = 0
    for term in chunk:
        for arg in term[1:]:
            n += count_comps(arg)
    return n


def chunk_sets(chunk, temps, is_head):
    last_args = chunk[-1][1:]

    # Maximum number of arguments, either output (last chunk) or input (head)
    max_args = len(last_args)
    if is_head and len(chunk) > 1:
        max_args = max(max_args, len(chunk[0])-1)

    # Maximum number of registers: one per argument, temp variable, and nested comp.
    max_regs = max_args + len(temps) + count_nested_comps(chunk)

    # Calc USE set
    def calc_use(term):
        use = defaultdict(list)
        for i, arg in enumerate(term[1:]):
            if arg in temps:
                use[arg].append(f'X{i}')
        return use

    use = calc_use(chunk[-1])
    if is_head and len(chunk) > 1:
        use.update(calc_use(chunk[0]))

    # Calc NOUSE set
    nouse = defaultdict(list)
    for x in temps:
        for i, arg in enumerate(last_args):
            if arg in temps and arg != x and i not in use[x]:
                nouse[x].append(f'X{i}')

    # Calc CONFLICT set
    conflict = defaultdict(list)
    vars_in_last = set(gen_vars(chunk[-1]))
    for x in temps:
        if x not in vars_in_last:
            continue
        for i, arg in enumerate(last_args):
            if arg != x:
                conflict[x].append(f'X{i}')

    return {
        'max_args': max_args,
        'max_regs': max_regs,
        'use': use,
        'nouse': nouse,
        'conflict': conflict,
    }


class ClauseCompiler:
    def __init__(self, clause, **kwargs):
        self.clause = clause
        self.kwargs = kwargs

        d = clause_chunks(clause)
        self.temps = d['temps']
        self.perms = d['perms']
        self.chunks = d['chunks']

        self.perm_addrs = None

    def compile(self):
        self.perm_addrs = {}
        for i, chunk in enumerate(self.chunks):
            chunk_compiler = ChunkCompiler(chunk, i == 0, self, **self.kwargs)
            for instr in chunk_compiler.compile():
                # Filter no-op instructions that don't move values around.
                if instr[0] in ('get_var', 'get_val', 'put_val') and instr[1] == instr[2]:
                    continue
                yield instr

    def perm_addr(self, x):
        if x not in self.perms:
            raise ValueError(f"{x} is not a permanent variable: {self.perms}")
        if x in self.perm_addrs:
            return self.perm_addrs[x], False
        index = len(self.perm_addrs)
        addr = f'Y{index}'
        self.perm_addrs[x] = addr
        return addr, True


class ChunkCompiler:
    def __init__(self, chunk, is_head, clause_compiler, alloc_strategy='naive'):
        self.chunk = chunk
        self.is_head = is_head
        self.parent = clause_compiler
        self.alloc_strategy = alloc_strategy

        d = chunk_sets(chunk, clause_compiler.temps, is_head)
        self.max_args = d['max_args']
        self.max_regs = d['max_regs']
        self.use = d['use']
        self.nouse = d['nouse']
        self.conflict = d['conflict']

        self.instructions = None
        self.delayed_comps = None

        self.free_regs = None
        self.top_reg = None
        self.temp_addrs = None
        self.reg_content = None

    def set_reg(self, reg, term):
        self.temp_addrs[term] = reg
        self.reg_content[reg] = term

    def compile(self):
        self.instructions = []
        self.free_regs = {f'X{i}' for i in range(self.max_regs)}
        self.top_reg = self.max_args

        self.temp_addrs = {}
        self.reg_content = {}

        chunk = self.chunk
        if self.is_head:
            head = chunk[0]
            chunk = chunk[1:]
            for i in range(len(head)-1):
                self.free_regs.remove(f'X{i}')
            self.compile_head(head)

        for goal in chunk[:-1]:
            instr = [goal[0]]
            for arg in goal[1:]:
                addr = self.term_addr(arg)
                instr.append(addr)
            self.instructions.append(tuple(instr))

        if chunk:
            last_goal = chunk[-1]
            for i, arg in enumerate(last_goal[1:]):
                self.put_term(arg, f'X{i}', top_level=True)
            self.instructions.append(('call', indicator(last_goal)))

        yield from self.instructions

    def compile_head(self, head):
        self.delayed_comps = []
        for i, arg in enumerate(head[1:]):
            self.get_term(arg, f'X{i}')
        while len(self.delayed_comps):
            delayed = self.delayed_comps.copy()
            self.delayed_comps = []
            for comp, addr in delayed:
                self.get_term(comp, addr)

    def get_term(self, term, reg):
        if is_var(term):
            if self.alloc_strategy == 'conflict_resolution':
                self.set_reg(reg, term)
            addr, is_new = self.var_addr(term)
            instr = 'get_var' if is_new else 'get_val'
            self.instructions.append((instr, addr, reg))
            if addr != reg:
                self.free_regs.add(reg)
        elif is_comp(term):
            self.instructions.append(('get_struct', indicator(term), reg))
            self.free_regs.add(reg)
            for arg in term[1:]:
                self.unify_arg(arg)
        else:
            self.instructions.append(('get_const', term, reg))
            self.free_regs.add(reg)

    def unify_arg(self, term):
        if is_var(term):
            addr, is_new = self.var_addr(term)
            instr = 'unify_var' if is_new else 'unify_val'
            self.instructions.append((instr, addr))
        elif is_comp(term):
            addr, _ = self.temp_addr(term)
            self.delayed_comps.append((term, addr))
            self.instructions.append(('unify_var', addr))
        else:
            self.instructions.append(('unify_const', term))

    def put_term(self, term, reg, *, top_level=False):
        # Move content out of register if in conflict.
        if self.alloc_strategy == 'conflict_resolution' and top_level:
            value = self.reg_content.get(reg)
            if value is not None and value != term and value in self.parent.temps:
                addr = self.conflict_resolution_alloc(value)
                self.set_reg(addr, value)
                self.instructions.append(('get_var', addr, reg))

        if is_var(term):
            addr, is_new = self.var_addr(term)
            instr = 'put_var' if is_new else 'put_val'
            self.instructions.append((instr, addr, reg))
            if addr != reg and addr[0] == 'X':
                self.free_regs.add(addr)
        elif is_comp(term):
            self.instructions.append(('put_struct', indicator(term), reg))
            delayed_vars = []
            for arg in term[1:]:
                if is_var(arg):
                    delayed_vars.append(arg)
                elif is_comp(arg):
                    addr = self.term_addr(arg)
                    self.instructions.append(('unify_val', addr))
                else:
                    self.unify_arg(arg)
            for x in delayed_vars:
                self.unify_arg(x)
        else:
            self.instructions.append(('put_const', term, reg))

    def term_addr(self, term):
        if is_var(term):
            addr, _ = self.var_addr(term)
            return addr
        if is_comp(term):
            addr, is_new = self.temp_addr(term)
            if is_new:
                self.put_term(term, addr)
            return addr
        return term

    def var_addr(self, x):
        if x in self.parent.perms:
            return self.parent.perm_addr(x)
        return self.temp_addr(x)

    def temp_addr(self, x):
        if x in self.temp_addrs:
            return self.temp_addrs[x], False
        if self.alloc_strategy == 'naive':
            addr = self.naive_alloc(x)
        if self.alloc_strategy == 'conflict_avoidance':
            addr = self.conflict_avoidance_alloc(x)
        if self.alloc_strategy == 'conflict_resolution':
            addr = self.conflict_resolution_alloc(x)
        self.set_reg(addr, x)
        return addr, True

    def naive_alloc(self, x):
        index = len(self.temp_addrs) + self.max_args
        return f'X{index}'

    def conflict_avoidance_alloc(self, x):
        use = set(self.use[x])
        nouse = set(self.nouse[x])
        conflict = set(self.conflict[x])
        free_regs = self.free_regs

        # Try to allocate a free register.
        free = free_regs & use
        if not free:
            free = free_regs - (nouse | conflict)
        if free:
            reg = min_reg(free)
            self.free_regs.remove(reg)
            return reg

        # Create a new register.
        reg = f'X{self.top_reg}'
        self.top_reg += 1
        return reg

    def conflict_resolution_alloc(self, x):
        use = set(self.use[x])
        nouse = set(self.nouse[x])
        free_regs = self.free_regs

        # Try to allocate a free register.
        free = free_regs & use
        if not free:
            free = free_regs - nouse
        if free:
            reg = min_reg(free)
            self.free_regs.remove(reg)
            return reg

        # Create a new register.
        reg = f'X{self.top_reg}'
        self.top_reg += 1
        return reg


def min_reg(regs):
    def by_index(reg):
        return int(reg[1:])  # Ignore 'X' in 'X123' and compare numerically
    return min(regs, key=by_index)


testdata = [
    ([('member', 'E', ('.', 'H', 'T')), ('member_', 'T', 'E', 'H')],
     {'conflict_avoidance': """
        get_var X3 X0
     get_struct ./2 X1
      unify_var X2
      unify_var X0
        put_val X3 X1
           call member_/3
     """,
      'conflict_resolution': """
      get_struct ./2 X1
       unify_var X2
       unify_var X3
         get_var X1 X0
         put_val X3 X0
            call member_/3
     """}),
    ([('mul', 'A', 'B', 'P'),
      ('=', ('s', 'B1'), 'B'),
      ('mul', 'A', 'B1', 'P1'),
      ('add', 'B1', 'P1', 'P')],
     {'conflict_avoidance': """
        get_var X3 X0
        get_var X4 X1
        get_var Y0 X2
     put_struct s/1 X0
      unify_var Y1
              = X0 X4
        put_val X3 X0
        put_val Y1 X1
        put_var Y2 X2
           call mul/3
        put_val Y1 X0
        put_val Y2 X1
        put_val Y0 X2
           call add/3
     """,
      'conflict_resolution': """
        get_var Y0 X2
     put_struct s/1 X2
      unify_var Y1
              = X2 X1
        get_var X3 X1
        put_val Y1 X1
        put_var Y2 X2
           call mul/3
        put_val Y1 X0
        put_val Y2 X1
        put_val Y0 X2
           call add/3
     """}),
    ([('is_even', ('s', ('s', 'X'))), ('is_even', 'X')],
     {'conflict_avoidance': """
     get_struct s/1 X0
      unify_var X0
     get_struct s/1 X0
      unify_var X0
           call is_even/1
     """,
      'conflict_resolution': """
     get_struct s/1 X0
      unify_var X0
     get_struct s/1 X0
      unify_var X0
           call is_even/1
     """}),
    ([('f', ('.', ('g', 'a'), ('.', ('h', 'b'), '[]')))],
     {'conflict_avoidance': """
      get_struct ./2 X0
       unify_var X0
       unify_var X1
      get_struct g/1 X0
     unify_const a
      get_struct ./2 X1
       unify_var X0
     unify_const []
      get_struct h/1 X0
     unify_const b
     """,
      'conflict_resolution': """
      get_struct ./2 X0
       unify_var X0
       unify_var X1
      get_struct g/1 X0
     unify_const a
      get_struct ./2 X1
       unify_var X0
     unify_const []
      get_struct h/1 X0
     unify_const b
     """}),
    ([('p', 'X', ('f', 'X'), 'Y', 'W'),
      ('=', 'X', ('.', 'a', 'Z')),
      ('>', 'W', 'Y'),
      ('q', 'Z', 'Y', 'X'),
      ],
     {'conflict_avoidance': """
         get_var X4 X0
      get_struct f/1 X1
       unify_val X4
         get_var X1 X2
         get_var X5 X3
      put_struct ./2 X0
     unify_const a
       unify_var X3
               = X4 X0
               > X5 X1
         put_val X3 X0
         put_val X4 X2
            call q/3
     """,
      'conflict_resolution': """
      get_struct f/1 X1
       unify_val X0
      put_struct ./2 X1
     unify_const a
       unify_var X4
               = X0 X1
               > X3 X2
         get_var X5 X0
         put_val X4 X0
         put_val X2 X1
         put_val X5 X2
            call q/3
     """}),
    ([('p', 'X', 'Y', 'Z', 'a'), ('q', 'Z', 'X', 'Y')],
     {'conflict_avoidance': """
       get_var X4 X0
       get_var X5 X1
       get_var X0 X2
     get_const a X3
       put_val X4 X1
       put_val X5 X2
          call q/3
     """,
      'conflict_resolution': """
     get_const a X3
       get_var X3 X0
       put_val X2 X0
       get_var X2 X1
       put_val X3 X1
          call q/3
     """}),
    ([('p', 'X', 'a', 'b'), ('q', 'c', 'd', ('f', 'X'))],
     {'conflict_avoidance': """
        get_var X3 X0
      get_const a X1
      get_const b X2
      put_const c X0
      put_const d X1
     put_struct f/1 X2
      unify_val X3
           call q/3
     """,
      'conflict_resolution': """
      get_const a X1
      get_const b X2
        get_var X1 X0
      put_const c X0
        get_var X2 X1
      put_const d X1
        get_var X3 X2
     put_struct f/1 X2
      unify_val X3
           call q/3
     """}),
]


@parametrize("clause,instrs", testdata)
@parametrize("alloc_strategy", ['conflict_avoidance', 'conflict_resolution'])
def test_compile_clause(clause, instrs, alloc_strategy):
    instrs = instrs[alloc_strategy]
    lines = [line.strip() for line in instrs.split("\n")]
    expected = [tuple(line.split(" ")) for line in lines if line]
    compiler = ClauseCompiler(clause, alloc_strategy=alloc_strategy)
    got = list(compiler.compile())
    assert got == expected


def main():
    term = ('f', 'X', 'a', 1, ('g', 'Y', 'X'))
    print(f"{term} vars: {list(gen_vars(term))}")
    for clause, _ in testdata:
        print(f'Clause: {clause}')
        d = clause_chunks(clause)
        temps, perms, chunks = d['temps'], d['perms'], d['chunks']
        print(f'  Permanent vars: {perms}')
        for i, chunk in enumerate(chunks):
            print(f'  Chunk #{i}: {chunk}')
            d = chunk_sets(chunk, temps, i == 0)
            print(f'    Max args: {d["max_args"]}, Max regs: {d["max_regs"]}')
            use, nouse, conflict = d['use'], d['nouse'], d['conflict']
            for x in temps:
                print(f'    USE({x}) = {use[x]}, NOUSE({x}) = {nouse[x]}, CONFLICT({x}) = {conflict[x]}')

        print('Instructions:')
        compiler = ClauseCompiler(clause, alloc_strategy='conflict_resolution')
        instrs = compiler.compile()
        for instr in instrs:
            print(f'  {instr}')

        print('Addresses')
        addrs = compiler.temp_addrs.copy()
        addrs.update(compiler.perm_addrs)
        for x, addr in addrs.items():
            print(f'  {x}: {addr}')


if __name__ == '__main__':
    main()
