from __future__ import annotations

import collections.abc
import copy
import gc
import importlib
import json
import typing

# Static type definitions
Seq = typing.Sequence
Env = list
CC = typing.Callable[[typing.Any], typing.Any]
CodeTypeNames = typing.Literal["if", "ref", "prim", "datum", "app", "closure"]
CodeTypes = typing.Union["Closure", "If", "App", "Ref", "Datum", "Prim"]
JSON_VALUE: typing.TypeAlias = typing.Union[
    str,
    int,
    float,
    bool,
    None,
    typing.List["JSON_VALUE"],
    typing.Dict[str, "JSON_VALUE"],
]
Analyzed = typing.Union["Error", typing.Callable[[Env], typing.Any]]

# Garbage collector is enabled by default.
gc.enable()

# Exceptions
class SchemeException(Exception):
    pass

# Monad
class Value(object):
    __slots__ = ("value",)
    def __init__(self, value) -> None:
        self.value = value
class Error(Value):
    pass

# Environment representation
def makeEnv(l : int) -> Env:
    return [None]*l
def refEnv(e: Env, n: int):
    try:
        return e[n]
    except IndexError:
        return Error(SchemeException(f"refEnv: index {n} too large for environment {e}"))
def setEnv(e: Env, n: int, v):
    try:
        e[n] = v
    except IndexError:
        return Error(SchemeException(f"setEnv: index {n} too large for environment {e}"))
    return None

# Utilities
def apply_cc(cc: CC, v):
    return cc(v)

# Primitives
def _print(cc: CC, v):
    return apply_cc(cc, print(v))
def get_attribute(cc: CC, obj, name):
    return apply_cc(cc, getattr(obj, name))
def set_attribute(cc: CC, obj, name, value):
    setattr(obj, name, value)
    return apply_cc(cc, None)
def _raise(cc: CC, v):
    return cc(Error(v))
# Procedures
####################################################
# Closures
class SchemeClosure(object):
    __slots__ = ("base", "arg_type", "arg_num", "free", "free_num", "code")
    def __init__(self, base: list, arg_info: Seq['Argument'], free: Seq[int], code: typing.Callable[[Env], typing.Any]) -> None:
        self.base = base
        self.arg_type = [ai["type"] for ai in arg_info]
        self.arg_num = len(arg_info)
        self.free = free
        self.free_num = len(free)
        self.code = code
    def __call__(self, *args):
        if len(args) != self.arg_num:
            return Error(SchemeException(f"SchemeClosure: arity mismatch(Expected {self.arg_num} argument(s); Given {args})"))
        base = self.base
        try:
            ne = [base[fi] for fi in self.free]
        except IndexError:
            return Error(SchemeException(f"SchemeClosure: free index too large for environment {base}"))
        boxed = Value
        ne.extend(boxed(av) if at == "boxed" else av for at, av in zip(self.arg_type, args))
        return LazyBox(self.code, ne)
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
            return Error(SchemeException(f"make-python-procedure <func>: arity mismatch(expected: {arity} argument(s); given: {args})"))
        return runTrampoline(apply(lambda x:x, proc, args))
    return apply_cc(cc, func)
####################################################
def dynamic_require(cc: CC, name, pkg):
    return apply_cc(cc, importlib.import_module(name, pkg))
#####################################
class List(collections.abc.Sequence):
    pass
class Null(List):
    def __len__(self):
        return 0
    def __getitem__(self, ind):
        if ind < 0:
            return Error(SchemeException(f"list-ref: index {ind} less than 0"))
        return Error(SchemeException(f"list-ref: index {ind} too large for list {self}"))
    def __str__(self) -> str:
        return "()"
class Pair(List):
    __slots__ = ("car", "cdr", "size")
    def __init__(self, car, cdr: 'LinkedList') -> None:
        self.car = car
        self.cdr = cdr
        self.size = 1 + (cdr.size if isinstance(cdr, Pair) else 0) # type: int
    def __iter__(self) -> typing.Iterator:
        list = self # type: 'LinkedList'
        while isinstance(list, Pair):
            yield list.car
            list = list.cdr
    def __len__(self):
        return self.size
    def __getitem__(self, ind):
        if ind < 0:
            return Error(SchemeException(f"list-ref: index {ind} less than 0"))
        offset = ind
        list = self # type: typing.Union[Pair, Null]
        while offset>0:
            if isinstance(list, Null):
                return Error(SchemeException(f"list-ref: index {ind} too large for list {self}"))
            offset -= 1
            list = list.cdr
        if isinstance(list, Pair):
                return list.car
        return Error(SchemeException(f"list-ref: index {ind} too large for list {self}"))
    def __str__(self) -> str:
        s = f"{self.car}"
        for v in self.cdr:
            s = s + " " + f"{v}"
        return f"({s})"
LinkedList = typing.Union[Pair, Null]
null = Null()
def cons(cc: CC, v1, v2: LinkedList):
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
def more(cc: CC, v1, v2):
    return apply_cc(cc, v1 > v2)
def less(cc: CC, v1, v2):
    return apply_cc(cc, v1 < v2)
def isinstanceof(cc: CC, obj, type):
    return apply_cc(cc, isinstance(obj, type))
class Stream(object):
    __slots__ = ('car', 'cdr')
    def __init__(self, car: typing.Callable[[], typing.Any], cdr: typing.Callable[[], typing.Any]):
        self.car = car
        self.cdr = cdr
def set_box(cc: CC, b: Value, v):
    b.value = v
    return apply_cc(cc, None)
def box(cc: CC, v):
    return apply_cc(cc, Value(v))
def unbox(cc: CC, b: Value):
    return apply_cc(cc, b.value)
def _error(cc: CC, msg: str):
    return cc(Error(SchemeException(msg)))
none = None # type: None
object_type = object
prims: typing.Dict[str, typing.Union[typing.Callable, type, None, Null]] = {
    "print": _print,
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
    ">": more,
    "<": less,
    "get-attribute": get_attribute,
    "set-attribute!": set_attribute,
    "raise": _raise,
    "error": _error,
    "closure-type": SchemeClosure,
    "apply": apply,
    "make-procedure": make_procedure,
    "make-python-procedure": make_python_procedure,
    "vm-apply": vm_apply,
    "dynamic-require": dynamic_require,
    "is-a?": isinstanceof,
    "stream-type": Stream,
    "object-type": object_type,
    "linked-list-type": List,
    "box-type": Value,
    "exn-type": SchemeException,
    "none": none,
}

# Trampoline
class LazyBox(object):
    __slots__ = ('code', 'env')
    def __init__(self, code: typing.Callable[[Env], typing.Any], env: Env):
        self.code = code
        self.env = env
    def __call__(self):
        return self.code(self.env)
def runTrampoline(b : typing.Union[Error, LazyBox, typing.Any]) -> typing.Union[Error, typing.Any]:
    r = b
    lazy_box_type = LazyBox
    while type(r) is lazy_box_type:
        r = r()
    return r

# AST
# TypedDicts for AST nodes
class Argument(typing.TypedDict):
    type: typing.Union[
        typing.Literal["boxed"],
        typing.Literal["unboxed"],
    ]
class Closure(typing.TypedDict, total=True):
    type: typing.Literal["closure"]
    args: Seq[Argument]
    free: Seq[int]
    code: CodeTypes
class If(typing.TypedDict, total=True):
    type: typing.Literal["if"]
    cond: CodeTypes
    then: CodeTypes
    otherwise: CodeTypes
class Ref(typing.TypedDict, total=True):
    type: typing.Literal["ref"]
    location: int
class Prim(typing.TypedDict, total=True):
    type: typing.Literal["prim"]
    name: str
class Datum(typing.TypedDict, total=True):
    type: typing.Literal["datum"]
    value: JSON_VALUE
class App(typing.TypedDict, total=True):
    type: typing.Literal["app"]
    func: CodeTypes
    args: Seq[CodeTypes]

# Analyzers
def _copy_datum(v: JSON_VALUE):
    if isinstance(v, (str, int, float, bool)) or v is None:
        return v
    if isinstance(v, list):
        return [_copy_datum(item) for item in v]
    if isinstance(v, dict):
        return {key: _copy_datum(value) for key, value in v.items()}
    return copy.deepcopy(v)

_analyzer_dispatch: typing.Dict[
    CodeTypeNames, 
    typing.Callable[[CodeTypes], Analyzed]
    ] = {}

def register_analyzer(node_type: CodeTypeNames):
    def decorator(fn: typing.Callable[[CodeTypes], Analyzed]):
        _analyzer_dispatch[node_type] = fn
        return fn
    return decorator

@register_analyzer("if")
def analyzeIf(c: If) -> Analyzed:
    cond = analyzeExpr(c["cond"])
    if isinstance(cond, Error):
        return cond
    then = analyzeExpr(c["then"])
    if isinstance(then, Error):
        return then
    otherwise = analyzeExpr(c["otherwise"])
    if isinstance(otherwise, Error):
        return otherwise
    def func(e: Env):
        cond_v = runTrampoline(cond(e))
        if isinstance(cond_v, Error):
            return cond_v
        if cond_v is False:
            return otherwise(e)
        return then(e)
    return func

@register_analyzer("ref")
def analyzeRef(c: Ref) -> Analyzed:
    loc = c["location"]
    def func(e: Env):
        try:
            return e[loc]
        except IndexError:
            return Error(SchemeException(f"refEnv: index {loc} too large for environment {e}"))
    return func

@register_analyzer("prim")
def analyzePrim(c: Prim) -> Analyzed:
    prim = c["name"]
    # Pre-lookup primitive to avoid runtime dictionary access
    try:
        prim_value = prims[prim]
    except KeyError:
        return lambda e: Error(SchemeException(f"Unknown primitive: {prim}"))
    return lambda e: prim_value

@register_analyzer("datum")
def analyzeDatum(c: Datum) -> Analyzed:
    v = c["value"]
    def func(e: Env):
        # Avoid mutating objects in the abstract syntax tree
        return _copy_datum(v)
    return func

@register_analyzer("closure")
def analyzeClosure(c: Closure) -> Analyzed:
    arg_info = c["args"]
    free = c["free"]
    body = analyzeExpr(c["code"])
    if isinstance(body, Error):
        return body
    def func(e: Env):
        return SchemeClosure(e, arg_info, free, body)
    return func

@register_analyzer("app")
def analyzeApp(c: App) -> Analyzed:
    func = analyzeExpr(c["func"])
    if isinstance(func, Error):
        return func
    arg_codes = c["args"]
    args = []
    for arg_code in arg_codes:
        arg = analyzeExpr(arg_code)
        if isinstance(arg, Error):
            return arg
        args.append(arg)
    def helper(e: Env):
        func_v = runTrampoline(func(e))
        if isinstance(func_v, Error):
            return func_v
        args_l = []
        for arg in args:
            v = runTrampoline(arg(e))
            if isinstance(v, Error):
                return v
            args_l.append(v)
        return func_v(*args_l)
    return helper

def analyzeExpr(expr: CodeTypes) -> Analyzed:
    fn = _analyzer_dispatch.get(expr["type"])
    if fn is None:
        return Error(SchemeException(f"evalExpr: unknown expression type {expr['type']}"))
    return fn(expr)
    
def evalExpr(code: CodeTypes, e: Env):
    try:
        # Disable GC during analysis phase as it's pure functional
        gc_was_enabled = gc.isenabled()
        if gc_was_enabled:
            gc.disable()
        a = analyzeExpr(code)
        if isinstance(a, Error):
            return a
        if gc_was_enabled:
            gc.enable()
        return runTrampoline(a(e))
    finally:
        # Collect garbage after evaluation to free memory
        gc.collect()

def run(code: str):
    result = evalExpr(json.loads(code), makeEnv(0))
    return result
