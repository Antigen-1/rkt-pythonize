import importlib
import json

class SchemeException(Exception):
    pass

# Evironment representation
def makeEnv():
    return [{}]
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

# Primitives
def get_attribute(obj, name):
    return getattr(obj, name)
def set_attribute(obj, name, value):
    setattr(obj, name, value)
def apply(proc, args):
    return proc(*args)
def dynamic_require(name, pkg):
    return importlib.import_module(name, pkg)
none = None
prims = {
    "get-attribute": get_attribute,
    "set-attribute!": set_attribute,
    "apply": apply,
    "dynamic-require": dynamic_require,
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
    return LazyBox(lambda:v)
def evalLambda(arg_names, body, e):
    def func(*args):
        if len(arg_names) != len(args):
            return LazyBox(lambda:SchemeException(f"<func>: arity mismatch(expected: {arg_names}; given: {args})"))
        ne = extendEnv(e)
        for i in range(len(arg_names)):
            bindEnv(ne, arg_names[i], args[i])
        return LazyBox(lambda:evalExpr(body, ne))
    return LazyBox(lambda:func)
def evalApp(func, args, e):
    func_v = runTrampoline(evalExpr(func, e))
    args_l = []
    for arg in args:
        args_l.append(runTrampoline(evalExpr(arg, e)))
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
