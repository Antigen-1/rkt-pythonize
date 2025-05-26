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

# Primitives
# Continuations are all closures
def get_attribute(cc, obj, name):
    return cc.func(getattr(obj, name))
def set_attribute(cc, obj, name, value):
    setattr(obj, name, value)
    return cc.func(None)
def vm_apply(cc, proc, args):
    return cc.func(proc(*args))
def apply(cc, proc, args):
    if isinstance(proc, Closure):
        return proc.func(cc, *args)
    else:
        return proc(cc, *args)
def dynamic_require(cc, name, pkg):
    return cc.func(importlib.import_module(name, pkg))
def isClosure(cc, obj):
    return cc.func(isinstance(obj, Closure))
def ref(cc, obj, ind):
    return cc.func(obj[ind])
def set(cc, obj, ind, val):
    obj[ind] = val
    return cc.func(None)
def append(cc, arr, obj):
    arr.append(obj)
    return cc.func(None)
def equal(cc, o1, o2):
    return cc.func(o1 == o2)
def eq(cc, o1, o2):
    return cc.func(o1 is o2)
def add(cc, v1, v2):
    return cc.func(v1 + v2)
def sub(cc, v1, v2):
    return cc.func(v1 - v2)
def neg(cc, v):
    return cc.func(-v)
def mul(cc, v1, v2):
    return cc.func(v1 * v2)
def div(cc, v1, v2):
    return cc.func(v1 / v2)
def quo(cc, v1, v2):
    return cc.func(v1 // v2)
def mod(cc, v1, v2):
    return cc.func(v1 % v2)
def isinstanceof(cc, obj, type):
    return cc.func(isinstance(obj, type))
none = None
object_type = object
prims = {
    "@": ref,
    "!": set,
    "<!": append,
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
    "vm-apply": vm_apply,
    "dynamic-require": dynamic_require,
    "closure?": isClosure,
    "is-a?": isinstanceof,
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
    if cond_v == False:
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
            return LazyBox(lambda:SchemeException(f"<func>: arity mismatch(expected: {arg_names}; given: {args})"))
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
