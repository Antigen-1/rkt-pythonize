import importlib
import json
import copy
import typing
import collections.abc
import functools

# Types
Seq = typing.Sequence
Env = list

class SchemeException(Exception):
    pass

# Environment representation
def makeEnv() -> Env:
    return []
def refEnv(e: Env, n: int):
    if n >= len(e):
        return SchemeException(f"refEnv: index {n} too large for environment {e}")
    return e[n]
def pushEnv(e: Env, v):
    return e.append(v)

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
#####################################
class List(collections.abc.Sequence):
    pass
class Null(List):
    def __len__(self):
        return 0
    def __getitem__(self, ind):
        assert ind >= 0
        return SchemeException(f"list-ref: index {ind} too large for list {self}")
    def __str__(self) -> str:
        return "null"
class Pair(List):
    __slots__ = ("car", "cdr")
    def __init__(self, car, cdr: List) -> None:
        self.car = car
        self.cdr = cdr
    def __iter__(self) -> typing.Iterator:
        list = self
        while isinstance(list, Pair):
            yield list.car
            list = list.cdr
    def __len__(self):
        length = 0
        list = self
        while isinstance(list, Pair):
            length += 1
            list = list.cdr
        return length
    def __getitem__(self, ind):
        assert ind >= 0
        offset = ind
        list = self
        while offset>0:
            if isinstance(list, Null):
                return SchemeException(f"list-ref: index {ind} too large for list {self}")
            offset -= 1
            list = list.cdr
        if isinstance(list, Pair):
                return list.car
        return SchemeException(f"list-ref: index {ind} too large for list {self}")
    def __str__(self) -> str:
        return f"(cons {self.car} {self.cdr})"
null = Null()
def cons(cc: CC, v1, v2: List):
    return apply_cc(cc, Pair(v1, v2))
def car(cc: CC, l: Pair):
    return apply_cc(cc, l.car)
def cdr(cc: CC, l: Pair):
    return apply_cc(cc, l.cdr)
#####################################
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
class Box(object):
    __slots__ = ("value",)
    def __init__(self, value) -> None:
        self.value = value
def set_box(cc: CC, b: Box, v):
    b.value = v
    return apply_cc(cc, None)
def box(cc: CC, v):
    return apply_cc(cc, Box(v))
def unbox(cc: CC, b: Box):
    return apply_cc(cc, b.value)
none = None
object_type = object
prims: typing.Dict[str, typing.Union[typing.Callable, type, None, Null]] = {
    "@": ref,
    "!": set,
    "?": has,
    "<!": append,
    "cons": cons,
    "car": car,
    "cdr": cdr,
    "null": null,
    "length": length,
    "set-box!": set_box,
    "box": box,
    "unbox": unbox,
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
    "linked-list-type": List,
    "box-type": Box,
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
class Argument(typing.TypedDict):
    type: typing.Literal["boxed", "unboxed"]
class Closure(CodeType, total=True):
    type: typing.Literal["closure"]
    args: Seq[Argument]
    free: Seq[int]
    code: CodeType
class If(CodeType, total=True):
    type: typing.Literal["if"]
    cond: CodeType
    then: CodeType
    otherwise: CodeType
class Ref(CodeType, total=True):
    type: typing.Literal["ref"]
    location: int
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
def is_closure(c: CodeType) -> typing.TypeGuard[Closure]:
    return c["type"] == "closure"
def is_if(c: CodeType) -> typing.TypeGuard[If]:
    return c["type"] == "if"
def is_ref(c: CodeType) -> typing.TypeGuard[Ref]:
    return c["type"] == "ref"
def is_prim(c: CodeType) -> typing.TypeGuard[Prim]:
    return c["type"] == "prim"
def is_datum(c: CodeType) -> typing.TypeGuard[Datum]:
    return c["type"] == "datum"
def is_app(c: CodeType) -> typing.TypeGuard[App]:
    return c["type"] == "app"

# Evaluator
def evalIf(c: If, e: Env):
    cond = c["cond"]
    then = c["then"]
    otherwise = c["otherwise"]
    cond_v = runTrampoline(evalExpr(cond, e))
    if cond_v is False:
        return evalExpr(otherwise, e)
    else:
        return evalExpr(then, e)
def evalRef(c: Ref, e: Env):
    loc = c["location"]
    return refEnv(e, loc)
def evalPrim(c: Prim, e: Env):
    prim = c["name"]
    return prims[prim]
def evalDatum(c: Datum, e: Env):
    v = c["value"]
    # Avoid mutating objects in the abstract syntax tree
    return copy.deepcopy(v)
def evalClosure(c: Closure, e: Env):
    arg_info = c["args"]
    free = c["free"]
    arg_types = [ai["type"] for ai in arg_info]
    body = c["code"]
    def func(*args) -> typing.Union[LazyBox, SchemeException]:
        if len(args) != len(arg_info):
           return SchemeException(f"evalClosure <func>: arity mismatch(Expected {len(arg_info)} argument(s); Given {args})")
        ne = makeEnv()
        for fi in free:
            fv = refEnv(e, fi)
            if isinstance(fv, SchemeException):
                return fv
            pushEnv(ne, fv)
        for at, av in zip(arg_types, args):
            if at == "boxed":
                pushEnv(ne, Box(av))
            else:
                pushEnv(ne, av)
        # Tail-call optimization
        return LazyBox(lambda: evalExpr(body, ne))
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
    if is_if(expr):
        return evalIf(expr, e)
    elif is_ref(expr):
        return evalRef(expr, e)
    elif is_prim(expr):
        return evalPrim(expr, e)
    elif is_datum(expr):
        return evalDatum(expr, e)
    elif is_closure(expr):
        return evalClosure(expr, e)
    elif is_app(expr):
        return evalApp(expr, e)
    else:
        raise SchemeException(f"evalExpr: unknown expression type {expr['type']}")
    
def run(code: str):
    return runTrampoline(evalExpr(json.loads(code), makeEnv()))
