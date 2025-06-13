import importlib
import json
import copy
import typing

class SchemeException(Exception):
    pass

# Environment representation
Env = typing.Dict[str, list]
Seq = typing.Sequence
def makeEnv() -> Env:
    return {}
def extendEnv(e: Env, free: Seq[str]) -> typing.Union[SchemeException, Env]:
    ne = {}
    for var in free:
        if var in e:
            ne[var] = e[var]
        else:
            return SchemeException(f"extendEnv: there is no variable called {var}")
    return ne
def setEnv(e: Env, k: str, v):
    if k in e:
        e[k][0] = v
    else:
        return SchemeException(f"setEnv: there is no variable called {k}")

    return None
def bindEnv(e: Env, ks: Seq[str], vs: Seq) -> typing.Union[SchemeException, Env]:
    if len(ks) != len(vs):
       return SchemeException(f"bindEnv: given {len(ks)} key(s) and {len(vs)} value(s)")
    for i in range(0, len(ks)):
        k = ks[i]
        v = vs[i]
        if k in e:
            return SchemeException(f"bindEnv: there has been a variable called {k}")
        else:
            e[k] = [v]
    return e
def refEnv(e: Env, k:str):
    if k in e:
        return e[k][0]
    else:
        return SchemeException(f"refEnv: there is no variable called {k}")

# Utilities
CC = typing.Callable[[typing.Any], typing.Any]
def apply_cc(cc: CC, v):
    return cc(v)

# Primitives
def get_attribute(cc, obj, name):
    return apply_cc(cc, getattr(obj, name))
def set_attribute(cc, obj, name, value):
    setattr(obj, name, value)
    return apply_cc(cc, None)
def vm_apply(cc, proc, args):
    return apply_cc(cc, proc(*args))
def apply(cc, proc, args):
    return proc(cc, *args)
def make_procedure(cc, proc):
    def func(cc, *args):
        return apply(cc, proc, [args])
    return apply_cc(cc, func)
def make_python_procedure(cc, proc, arity):
    def func(*args):
        if arity and (arity != len(args)):
            raise SchemeException(f"make-python-procedure <func>: arity mismatch(expected: {arity} argument(s); given: {args})")
        return runTrampoline(apply(lambda x:x, proc, args))
    return apply_cc(cc, func)
def dynamic_require(cc, name, pkg):
    return apply_cc(cc, importlib.import_module(name, pkg))
def ref(cc, obj, ind):
    return apply_cc(cc, obj[ind])
def set(cc, obj, ind, val):
    obj[ind] = val
    return apply_cc(cc, None)
def has(cc, container, v):
    return apply_cc(cc, v in container)
def append(cc, arr, obj):
    arr.append(obj)
    return apply_cc(cc, None)
def length(cc, arr):
    return apply_cc(cc, len(arr))
def _not(cc, b):
    return apply_cc(cc, not b)
def equal(cc, o1, o2):
    return apply_cc(cc, o1 == o2)
def eq(cc, o1, o2):
    return apply_cc(cc, o1 is o2)
def add(cc, v1, v2):
    return apply_cc(cc, v1 + v2)
def sub(cc, v1, v2):
    return apply_cc(cc, v1 - v2)
def neg(cc, v):
    return apply_cc(cc, -v)
def mul(cc, v1, v2):
    return apply_cc(cc, v1 * v2)
def div(cc, v1, v2):
    return apply_cc(cc, v1 / v2)
def quo(cc, v1, v2):
    return apply_cc(cc, v1 // v2)
def mod(cc, v1, v2):
    return apply_cc(cc, v1 % v2)
def isinstanceof(cc, obj, type):
    return apply_cc(cc, isinstance(obj, type))
class Stream(object):
    def __init__(self, car: typing.Callable[[], typing.Any], cdr: typing.Callable[[], typing.Any]):
        self.car = car
        self.cdr = cdr
none = None
object_type = object
prims: typing.Dict[str, typing.Union[typing.Callable, type, None]] = {
    "@": ref,
    "!": set,
    "?": has,
    "<!": append,
    "length": length,
    "not": _not,
    "equal?": equal,
    "eq?": eq,
    "+": add,
    "-": sub,
    "*": mul,
    "/": div,
    "quotient": quo,
    "modulo": mod,
    "negate": neg,
    "get-attribute": get_attribute,
    "set-attribute!": set_attribute,
    "apply": apply,
    "make-procedure": make_procedure,
    "make-python-procedure": make_python_procedure,
    "vm-apply": vm_apply,
    "dynamic-require": dynamic_require,
    "is-a?": isinstanceof,
    "stream-type": Stream,
    "object-type": object_type,
    "none": none,
}

# Trampoline
class LazyBox(object):
    def __init__(self, func: typing.Callable[[], typing.Any]):
        self.func = func
    def __call__(self):
        return self.func()
def runTrampoline(b):
    r = b
    while isinstance(r, LazyBox):
        r = r()
    if isinstance(r, SchemeException):
        raise r
    return r

# Evaluator
def evalBegin(seq, e: Env) -> LazyBox:
    last_result = none
    for expr in seq:
        last_result = runTrampoline(evalExpr(expr, e))
    return LazyBox(lambda:last_result)
def evalIf(cond, then, otherwise, e: Env) -> LazyBox:
    cond_v = runTrampoline(evalExpr(cond, e))
    if cond_v is False:
        return LazyBox(lambda:evalExpr(otherwise, e))
    else:
        return LazyBox(lambda:evalExpr(then, e))
def evalSet(var: str, value, e: Env) -> LazyBox:
    return LazyBox(lambda:setEnv(e, var, runTrampoline(evalExpr(value, e))))
def evalVar(var: str, e: Env) -> LazyBox:
    return LazyBox(lambda:refEnv(e, var))
# Return the object directly
##########################
def evalPrim(prim, e: Env):
    return prims[prim]
def evalDatum(v, e: Env):
    # Avoid mutating objects in the abstract syntax tree
    return copy.deepcopy(v)
def evalLambda(arg_names: Seq[str], body, free: Seq[str], e: Env):
    def func(*args):
        if len(args) != len(arg_names):
           return SchemeException(f"evalLambda <func>: arity mismatch(Expected {len(arg_names)} argument(s); Given {args})")
        ne1 = extendEnv(e, free)
        if isinstance(ne1, SchemeException):
           return ne1
        ne2 = bindEnv(ne1, arg_names, args)
        if isinstance(ne2, SchemeException):
           return ne2
        return LazyBox(lambda: evalExpr(body, ne2))
    return func
##########################
def evalApp(func, args, e: Env) -> LazyBox:
    func_v = runTrampoline(evalExpr(func, e))
    args_l = []
    for arg in args:
        args_l.append(runTrampoline(evalExpr(arg, e)))
    return LazyBox(lambda:func_v(*args_l))
def evalExpr(expr, e: Env) -> typing.Union[LazyBox, typing.Any]:
    if not "type" in expr:
        return SchemeException(f"evalExpr: expects an AST, given {expr}")
    etype = expr["type"]
    if etype == "begin":
        return evalBegin(expr["seq"], e)
    elif etype == "if":
        return evalIf(expr["cond"], expr["then"], expr["otherwise"], e)
    elif etype == "set!":
        return evalSet(expr["var"], expr["value"], e)
    elif etype == "var":
        return evalVar(expr["name"], e)
    elif etype == "prim":
        return evalPrim(expr["name"], e)
    elif etype == "datum":
        return evalDatum(expr["value"], e)
    elif etype == "lambda":
        return evalLambda(expr["args"], expr["body"], expr["free"], e)
    elif etype == "app":
        return evalApp(expr["func"], expr["args"], e)
    else:
        return SchemeException(f"evalExpr: expects an AST, given {expr}")

def run(code: str):
    return runTrampoline(evalExpr(json.loads(code), makeEnv()))
