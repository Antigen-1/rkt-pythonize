import importlib
import json
import copy

class SchemeException(Exception):
    pass

# Evironment representation
def makeEnv():
    return []
def extendEnv(e):
    return [{}] + e
def setEnv(e, k, v):
    exists = False
    for frame in e:
        if k in frame:
            exists = True
            frame[k] = v

    if not exists:
        return SchemeException(f"setEnv: there is no variable called {k}")

    return None
def bindEnv(e, k, v):
    first = e[0]
    if k in first:
        return SchemeException(f"bindEnv: there has been a variable called {k}")
    first[k] = v
    return None
def refEnv(e, k):
    for frame in e:
        if k in frame:
            return frame[k]

    return SchemeException(f"refEnv: there is no variable called {k}")

# Closure
class Closure(object):
    def __init__(this, func):
        this.func = func

# Utilities
def apply_cc(cc, v):
    if isinstance(cc, Closure):
        return cc.func(v)
    else:
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
    if isinstance(proc, Closure):
        return proc.func(cc, *args)
    else:
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
def isClosure(cc, obj):
    return apply_cc(cc, isinstance(obj, Closure))
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
    def __init__(this, car, cdr):
        this.car = car
        this.cdr = cdr
none = None
object_type = object
prims = {
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
    "closure?": isClosure,
    "is-a?": isinstanceof,
    "stream-type": Stream,
    "object-type": object_type,
    "none": none,
}

# Trampoline
class LazyBox(object):
    def __init__(this, func):
        this.func = func
def runTrampoline(b):
    r = b
    while isinstance(r, LazyBox):
        r = r.func()
        if isinstance(r, SchemeException):
            raise r
    return r

# Evaluator
def evalBegin(seq, e):
    last_result = none
    for expr in seq:
        last_result = runTrampoline(evalExpr(expr, e))
    return LazyBox(lambda:last_result)
def evalIf(cond, then, otherwise, e):
    cond_v = runTrampoline(evalExpr(cond, e))
    if cond_v is False:
        return LazyBox(lambda:evalExpr(otherwise, e))
    else:
        return LazyBox(lambda:evalExpr(then, e))
def evalSet(var, value, e):
    return LazyBox(lambda:setEnv(e, var, runTrampoline(evalExpr(value, e))))
def evalVar(var, e):
    return LazyBox(lambda:refEnv(e, var))
def evalPrim(prim, e):
    return LazyBox(lambda:prims[prim])
def evalDatum(v, e):
    # Avoid mutating objects in the abstract syntax tree
    return LazyBox(lambda:copy.deepcopy(v))
def evalLambda(arg_names, body, e):
    def func(*args):
        if len(arg_names) != len(args):
            return LazyBox(lambda:SchemeException(f"evalLambda <func>: arity mismatch(expected: {arg_names}; given: {args})"))
        ne = extendEnv(e)
        for i in range(len(arg_names)):
            bindEnv(ne, arg_names[i], args[i])
        return LazyBox(lambda:evalExpr(body, ne))
    return LazyBox(lambda:Closure(func))
def evalApp(func, args, e):
    func_v = runTrampoline(evalExpr(func, e))
    args_l = []
    for arg in args:
        args_l.append(runTrampoline(evalExpr(arg, e)))
    if isinstance(func_v, Closure):
        return LazyBox(lambda:func_v.func(*args_l))
    else:
        return LazyBox(lambda:func_v(*args_l))
def evalExpr(expr, e):
    if not "type" in expr:
        return LazyBox(lambda:SchemeException(f"evalExpr: expects an AST, given {expr}"))
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
        return evalLambda(expr["args"], expr["body"], e)
    elif etype == "app":
        return evalApp(expr["func"], expr["args"], e)
    else:
        return LazyBox(lambda:SchemeException(f"evalExpr: expects an AST, given {expr}"))

def run(code):
    return runTrampoline(evalExpr(json.loads(code), makeEnv()))
