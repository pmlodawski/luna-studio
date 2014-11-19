#!/usr/bin/env python3
import itertools


class TMono:
    def __init__(self, i):
        self.i = i

    def __repr__(self):
        return "τ_" + str(self.i)


class TConst(TMono):
    def __init__(self, name):
        super().__init__(-1)
        self.name = name

    def __repr__(self):
        return self.name


class TArr:
    def __init__(self, t1, t2):
        self.t1 = t1
        self.t2 = t2

    def __repr__(self):
        return "{} → {}".format(repr_ty(self.t1), repr(self.t2))


def repr_ty(ty):
    if isinstance(ty, TMono):
        return repr(ty)
    return "({})".format(repr(ty))


def _typegen():
    for i in itertools.count(0):
        yield TMono(i)

typegen = _typegen()


class CEq:
    def __init__(self, ty1, ty2):
        self.ty1 = ty1
        self.ty2 = ty2

    def __repr__(self):
        return "{} ≡ {}".format(repr_ty(self.ty1), repr_ty(self.ty2))


class CEInst:
    def __init__(self, ty, sch):
        self.ty = ty
        self.sch = sch

    def __repr__(self):
        return "{} ≼ {}".format(repr_ty(self.ty), repr_ty(self.sch))


class CIInst:
    def __init__(self, ty, ty_sch, m):
        self.ty = ty
        self.ty_sch = ty_sch
        self.m = m

    def __repr__(self):
        return "{} ≤[{}] {}".format(repr_ty(self.ty), ", ".join(repr_ty(t) for t in self.m), repr_ty(self.ty_sch))


class Let:
    def __init__(self, var, subt_value, subt_body):
        self.var = var
        self.var_type = next(typegen)
        self.subt_value = subt_value
        self.subt_body = subt_body
        self.m = None

    def calc_mono(self, m=None):
        if self.m is not None:
          return
        self.m = m or set()
        self.subt_value.calc_mono(self.m)
        self.subt_body.calc_mono(self.m)

    def infer(self) -> 'A, C ⊢ e : t':
        a1, c1, e1, t1 = self.subt_value.infer()
        a2, c2, e2, t2 = self.subt_body.infer()

        a = a2.copy()
        del a[self.var]
        a.update(a1)

        c = c1 + c2 + [CIInst(tp, t1, self.m)
                       for xp, tp in a2.items()
                       if xp == self.var]

        return a, c, self, t2

    def __repr__(self):
        return self.show(0)

    def show(self, indent):
        ind = ''.rjust(indent)
        sv = str(self.var)
        return "let {} = {}\n{ind} in {}".format(sv,
                                                 self.subt_value.show(indent + len(sv) + 4),
                                                 self.subt_body.show(indent + 4),
                                                 ind=ind)


class Abs:
    def __init__(self, var, subt_body):
        self.var = var
        self.var_type = next(typegen)
        self.subt_body = subt_body
        self.m = None

    def calc_mono(self, m=None):
        if self.m is not None:
          return
        self.m = m or set()
        md = self.m.copy()
        md.add(self.var_type)
        self.subt_body.calc_mono(md)

    def infer(self) -> 'A, C ⊢ e : t':
        a1, c1, e1, t1 = self.subt_body.infer()
        beta = self.var_type

        a = a1.copy()
        del a[self.var]
        c = c1 + [CEq(tp, beta)
                  for (xp, tp) in a1.items()
                  if xp == self.var]
        return a, c, self, TArr(beta, t1)

    def show(self, indent):
        ind = ''.rjust(indent)
        sv = str(self.var)
        return "λ{} → {}".format(sv, self.subt_body.show(indent + len(sv) + 4), ind=ind)
        # return "{}λ{} → {}".format(ind, str(self.var), self.subt_body.show(0))

    def __repr__(self):
        return self.show(0)


class App:
    def __init__(self, subt_fun, subt_arg):
        self.subt_fun = subt_fun
        self.subt_arg = subt_arg
        self.m = None
        self.beta = next(typegen)

    def calc_mono(self, m=None):
        if self.m is not None:
          return
        self.m = m or set()
        self.subt_arg.calc_mono(self.m)
        self.subt_fun.calc_mono(self.m)

    def infer(self) -> 'A, C ⊢ e : t':
        a1, c1, e1, t1 = self.subt_fun.infer()
        a2, c2, e2, t2 = self.subt_arg.infer()

        a12 = dict(a1.items() | a2.items())
        c12 = c1 + c2 + [CEq(t1, TArr(t2, self.beta))]

        return a12, c12, self, self.beta

    # noinspection PyUnusedLocal
    def show(self, indent):
        return "{} {}".format(self.subt_fun.show(0), self.subt_arg.show(0))

    def __repr__(self):
        return self.show(0)


class Var:
    def __init__(self, var, var_type=None):
        self.var = var
        self.var_type = var_type or next(typegen)
        self.m = None

    def calc_mono(self, m=None):
        if self.m is not None:
          return
        self.m = m or set()

    def infer(self) -> 'A, C ⊢ e : t':
        a = {self.var: self.var_type}
        c = []
        return a, c, self, self.var_type

    # noinspection PyUnusedLocal
    def show(self, indent):
        return "{}".format(str(self.var))

    def __repr__(self):
        return self.show(0)

# ----------------------------------------------------------------------------------------------------------------------

def example1_heeren():
    var3a = Var("x")
    var3b = Var("y")

    let2 = Let("y", var3a, var3b)
    var2a = Var("id")
    var2b = Var("id")

    abs1 = Abs("x", let2)
    app1 = App(var2a, var2b)

    let0 = Let("id", abs1, app1)
    return let0


def example2_heeren():
    var4a = Var("y")
    var4b = Var("True", TConst("𝔹"))
    app3 = App(var4a, var4b)
    var3 = Var("x")
    var2 = Var("m")
    let2 = Let("x", app3, var3)
    let1 = Let("y", var2, let2)
    abs0 = Abs("m", let1)
    return abs0


def main():
    ex1 = example1_heeren()
    ex1.calc_mono()
    print(str(ex1.infer()))


def calc(x):
    x.calc_mono()
    a, c, e, t = x.infer()
    print("A = {}\nC = {}\nE = {}\nT = {}".format(a, c, e.show(4), t))

if __name__ == '__main__':
    main()