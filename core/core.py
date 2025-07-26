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
def get_attribute(cc: CC, obj, name):
    return apply_cc(cc, getattr(obj, name))
def set_attribute(cc: CC, obj, name, value):
    setattr(obj, name, value)
    return apply_cc(cc, None)
def vm_apply(cc: CC, proc, args):
    return apply_cc(cc, proc(*args))
def apply(cc: CC, proc, args):
    return proc(cc, *args)
def make_procedure(cc: CC, proc):
    def func(cc, *args):
        return apply(cc, proc, [args])
    return apply_cc(cc, func)
def make_python_procedure(cc: CC, proc, arity):
    def func(*args):
        if arity and (arity != len(args)):
            raise SchemeException(f"make-python-procedure <func>: arity mismatch(expected: {arity} argument(s); given: {args})")
        return runTrampoline(apply(lambda x:x, proc, args))
    return apply_cc(cc, func)
def dynamic_require(cc: CC, name, pkg):
    return apply_cc(cc, importlib.import_module(name, pkg))
def ref(cc: CC, obj, ind):
    return apply_cc(cc, obj[ind])
def set(cc: CC, obj, ind, val):
    obj[ind] = val
    return apply_cc(cc, None)
def has(cc: CC, container, v):
    return apply_cc(cc, v in container)
def append(cc: CC, arr, obj):
    arr.append(obj)
    return apply_cc(cc, None)
def length(cc: CC, arr):
    return apply_cc(cc, len(arr))
def _not(cc: CC, b):
    return apply_cc(cc, not b)
def equal(cc: CC, o1, o2):
    return apply_cc(cc, o1 == o2)
def eq(cc: CC, o1, o2):
    return apply_cc(cc, o1 is o2)
def add(cc: CC, v1, v2):
    return apply_cc(cc, v1 + v2)
def sub(cc: CC, v1, v2):
    return apply_cc(cc, v1 - v2)
def neg(cc: CC, v):
    return apply_cc(cc, -v)
def mul(cc: CC, v1, v2):
    return apply_cc(cc, v1 * v2)
def div(cc: CC, v1, v2):
    return apply_cc(cc, v1 / v2)
def quo(cc: CC, v1, v2):
    return apply_cc(cc, v1 // v2)
def mod(cc: CC, v1, v2):
    return apply_cc(cc, v1 % v2)
def isinstanceof(cc: CC, obj, type):
    return apply_cc(cc, isinstance(obj, type))
class Stream(object):
    __slots__ = ('car', 'cdr')
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
    __slots__ = ('func',)
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

# AST
# TypedDicts for AST nodes
class CodeType(typing.TypedDict):
    type: typing.Literal["begin", "if", "set!", "var", "prim", "datum", "lambda", "app"]
class Lambda(CodeType, total=True):
    type: typing.Literal["lambda"]
    args: Seq[str]
    body: CodeType
    free: Seq[str]
class Begin(CodeType, total=True):
    type: typing.Literal["begin"]
    seq: Seq[CodeType]
class If(CodeType, total=True):
    type: typing.Literal["if"]
    cond: CodeType
    then: CodeType
    otherwise: CodeType
class Set(CodeType, total=True):
    type: typing.Literal["set!"]
    var: str
    value: CodeType
class Var(CodeType, total=True):
    type: typing.Literal["var"]
    name: str
class Prim(CodeType, total=True):
    type: typing.Literal["prim"]
    name: str
JSON_VALUE: typing.TypeAlias = typing.Union[str, int, float, bool, None, typing.List['JSON_VALUE'], typing.Dict[str, 'JSON_VALUE']]
class Datum(CodeType, total=True):
    type: typing.Literal["datum"]
    value: JSON_VALUE
class App(CodeType, total=True):
    type: typing.Literal["app"]
    func: CodeType
    args: Seq[CodeType]
# Type guards for AST
def is_lambda(c: CodeType) -> typing.TypeGuard[Lambda]:
    return c["type"] == "lambda"
def is_begin(c: CodeType) -> typing.TypeGuard[Begin]:
    return c["type"] == "begin"
def is_if(c: CodeType) -> typing.TypeGuard[If]:
    return c["type"] == "if"
def is_set(c: CodeType) -> typing.TypeGuard[Set]:
    return c["type"] == "set!"
def is_var(c: CodeType) -> typing.TypeGuard[Var]:
    return c["type"] == "var"
def is_prim(c: CodeType) -> typing.TypeGuard[Prim]:
    return c["type"] == "prim"
def is_datum(c: CodeType) -> typing.TypeGuard[Datum]:
    return c["type"] == "datum"
def is_app(c: CodeType) -> typing.TypeGuard[App]:
    return c["type"] == "app"

# Evaluator
def evalBegin(c: Begin, e: Env):
    seq = c["seq"]
    last_result = none
    for expr in seq:
        last_result = runTrampoline(evalExpr(expr, e))
    return last_result
def evalIf(c: If, e: Env):
    cond = c["cond"]
    then = c["then"]
    otherwise = c["otherwise"]
    cond_v = runTrampoline(evalExpr(cond, e))
    if cond_v is False:
        return evalExpr(otherwise, e)
    else:
        return evalExpr(then, e)
def evalSet(c: Set, e: Env):
    var = c["var"]
    value = c["value"]
    return setEnv(e, var, runTrampoline(evalExpr(value, e)))
def evalVar(c: Var, e: Env):
    var = c["name"]
    return refEnv(e, var)
def evalPrim(c: Prim, e: Env):
    prim = c["name"]
    return prims[prim]
def evalDatum(c: Datum, e: Env):
    v = c["value"]
    # Avoid mutating objects in the abstract syntax tree
    return copy.deepcopy(v)
def evalLambda(c: Lambda, e: Env):
    arg_names = c["args"]
    body = c["body"]
    free = c["free"]
    def func(*args) -> typing.Union[LazyBox, SchemeException]:
        if len(args) != len(arg_names):
           return SchemeException(f"evalLambda <func>: arity mismatch(Expected {len(arg_names)} argument(s); Given {args})")
        ne1 = extendEnv(e, free)
        if isinstance(ne1, SchemeException):
           return ne1
        ne2 = bindEnv(ne1, arg_names, args)
        if isinstance(ne2, SchemeException):
           return ne2
        # Tail-call optimization
        return LazyBox(lambda: evalExpr(body, ne2))
    return func
def evalApp(c: App, e: Env):
    func = c["func"]
    args = c["args"]
    func_v = runTrampoline(evalExpr(func, e))
    args_l = []
    for arg in args:
        args_l.append(runTrampoline(evalExpr(arg, e)))
    return func_v(*args_l)
def evalExpr(expr: CodeType, e: Env) -> typing.Union[LazyBox, typing.Any]:
    if is_begin(expr):
        return evalBegin(expr, e)
    elif is_if(expr):
        return evalIf(expr, e)
    elif is_set(expr):
        return evalSet(expr, e)
    elif is_var(expr):
        return evalVar(expr, e)
    elif is_prim(expr):
        return evalPrim(expr, e)
    elif is_datum(expr):
        return evalDatum(expr, e)
    elif is_lambda(expr):
        return evalLambda(expr, e)
    elif is_app(expr):
        return evalApp(expr, e)
    else:
        raise SchemeException(f"evalExpr: unknown expression type {expr['type']}")
    
def run(code: str):
    return runTrampoline(evalExpr(json.loads(code), makeEnv()))
