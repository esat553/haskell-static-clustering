import re
from collections import OrderedDict

CLUSTERS = OrderedDict([
    # Parse Errors
    ("GHCi Context in Submission", re.compile(r"(?m)(^|\n).*?(ghci>|Prelude>|parse error on input\s+‘(:\{|}:)’|:{|}:)", re.MULTILINE)),
    ("Invalid Top-Level Declaration", re.compile(r"Parse error: module header, import declaration\s+or\s+top-level declaration expected\.", re.IGNORECASE)),
    ("Parse Error due to Import Error", re.compile(r"(parse error on input)[\s\S]+?‘?import",re.IGNORECASE)),
    ("Parse Error in 'let' Binding", re.compile(
        r"(?:\(let.*in.*\)-syntax\s+in\s+pattern"
        r"|parse\s+error\s*\(possibly\s+incorrect\s+indentation[^\)]*\)"
        r"[\s\S]*?\n\s*\d+\s*\|\s+.*\blet\b[^\n]*=)",
        re.IGNORECASE | re.DOTALL
    )),
    ("Parse Error in Function Declaration", re.compile(r"parse error.*?\n\s*\|\s*(\d+)\s\|\s([a-z]\w*)\s*::", re.DOTALL | re.IGNORECASE)),
    ("Parse Error", re.compile(r"\bparse\s+error\b", re.IGNORECASE)),

    # Typed Hole
    ("Typed Hole", re.compile(r"found hole: _ ::", re.IGNORECASE)),

    # Type Errors
    ("Incorrect Function Arity", re.compile(
        r"\bapplied to too (few|many) arguments\b|" +
        r"\bhas \w+ arguments, but its type .*? has only \w+",
        re.IGNORECASE
    )),
    ("Inconsistent Return Type", re.compile(r"Couldn't match type[:\s]*.*with[:\s]*.*In a case alternative", re.DOTALL | re.IGNORECASE)),
    ("Implementation Violates Type Signature", re.compile(r"is a rigid type variable bound by", re.DOTALL | re.IGNORECASE)),
    ("Duplicate Type Signature", re.compile(r"duplicate type signatures?", re.IGNORECASE)),
    ("Infinite Type", re.compile(r"occurs check:.*infinite type", re.IGNORECASE | re.DOTALL)),
    ("Multiple Declarations", re.compile(r"multiple declarations", re.IGNORECASE)),
    ("Ambiguous Identifier", re.compile(r"ambiguous occurrence", re.IGNORECASE)),
    ("Numeric Type Conflict", re.compile(
        r"No instance for .*Num .*"
        r"|No instance for .*Fractional .*"
        r"|Couldn't match expected type\s+.(Double|Float|Rational|Int|Integer|Num\s+a\d*).\s+with actual type\s+.(Double|Float|Rational|Int|Integer|Num\s+a\d*).",
        re.IGNORECASE | re.DOTALL
    )),
    ("Type Mismatch", re.compile(r"couldn'?t match (expected type|type)", re.IGNORECASE)),
    ("Illegal Type Operator", re.compile(r"illegal operator .* in type .*", re.IGNORECASE)),

    # Not in scope / Scope
    ("Type Constructor or Class Not Defined", re.compile(r"Not in scope: type constructor or class ‘[A-Z][a-zA-Z0-9_']*’", re.DOTALL | re.IGNORECASE)),
    ("Data Constructor Not Defined", re.compile(r"Not in scope: data constructor ‘[A-Z][a-zA-Z0-9_']*’", re.DOTALL | re.IGNORECASE)),
    ("Function Not Defined", re.compile(r"Variable not in scope: ([a-zA-Z_][a-zA-Z0-9_']*)\s*::\s*[^:\n]+->.*", re.DOTALL | re.IGNORECASE)),
    ("Variable Not in Scope", re.compile(r"not in scope", re.IGNORECASE)),

    # Binding Errors
    ("Pattern Binding in Instance Declaration", re.compile(r"pattern bindings.*not allowed in instance declaration", re.IGNORECASE)),
    ("Missing Binding", re.compile(r"type signature.*lacks an accompanying binding", re.IGNORECASE | re.DOTALL)),

    # Constructors / Arity
    ("Wrong Constructor Arity", re.compile(r"the constructor .* should have \d+ argument[s]?, but has been given \d+", re.IGNORECASE | re.DOTALL)),
    ("Inconsistent Arity", re.compile(r"equations for .* have different numbers of arguments", re.IGNORECASE | re.DOTALL)),
    ("Constraint Expected, Got Type", re.compile(r"expected a constraint, but .*(has kind|is a type)",re.IGNORECASE | re.DOTALL)),
    ("Invalid Instance Signature", re.compile(r"illegal type signature in instance declaration", re.IGNORECASE)),
    ("Invalid Type Signature", re.compile(r"(invalid|illegal) type signature", re.IGNORECASE)),

    # Instance Errors
    ("Overlapping Instances", re.compile(r"overlapping instances for", re.IGNORECASE)),
    ("Missing Constraint in Function Signature", re.compile(r"No instance for [\(‘]\w+ [a-z][\)’] arising from a use of", re.IGNORECASE | re.DOTALL)),
    ("Missing Superclass Instance", re.compile(r"no\s+instance\s+for.*arising\s+from\s+the\s+superclasses", re.IGNORECASE | re.DOTALL)),
    ("Missing Instance in 'deriving'", re.compile(r"When deriving the instance for", re.IGNORECASE)),
    ("Missing Instance", re.compile(r"no instance for", re.IGNORECASE)),

    # Conflicts
    ("Conflict in 'data' Declaration", re.compile(r"Conflicting definitions for\s+['‘`]?.+?['’`]?\s+.*?\n\s*\|\s*\n\s*\d+\s*\|\s*data", re.IGNORECASE | re.DOTALL)),
    ("Multiple Definitions in Function Equation", re.compile(r"conflicting definitions for.*in an equation for",re.IGNORECASE | re.DOTALL)),
    ("Conflicting Bindings", re.compile(r"conflicting definitions for", re.IGNORECASE)),

    # Module & Miscellaneous
    ("Module Not Found", re.compile(r"could not find module", re.IGNORECASE)),
    ("Data Constructor Error", re.compile(r"(cannot parse data constructor in a data/newtype declaration|not a data constructor)", re.IGNORECASE)),
    ("Duplicate Instance Declaration", re.compile(r"duplicate instance declarations", re.IGNORECASE)),
    ("Method Not in Class", re.compile(r"is not a \(visible\) method of class", re.IGNORECASE)),
    ("Invalid Instance Form", re.compile(r"illegal instance declaration|Use FlexibleInstances", re.IGNORECASE)),
    ("Wrong Number of Type Arguments", re.compile(r"expecting one more argument to .*has kind", re.IGNORECASE | re.DOTALL)),
    ("Kind Mismatch", re.compile(r"expected kind .* but .* has kind", re.IGNORECASE | re.DOTALL)),
    ("Kind Mismatch (Constraint vs. Type)", re.compile(r"expected (a constraint|a type), but .* has kind", re.IGNORECASE | re.DOTALL)),
    ("Ambiguous Type", re.compile(r"ambiguous type variable", re.IGNORECASE)),
    ("Constraint Not Satisfiable", re.compile(r"could not deduce", re.IGNORECASE)),
    ("Flexible Contexts Required", re.compile(r"non type-variable argument in the constraint", re.IGNORECASE)),
    ("Missing GADTs Extension", re.compile(r"enable the GADTs extension", re.IGNORECASE)),
    ("Invalid Character", re.compile(r"syntax error", re.IGNORECASE)),
    ("Lexical Error", re.compile(r"(lexical error at character|Unicode character .+ looks like .+ but it is not)",re.IGNORECASE)),
    ("Malformed Type Header", re.compile(r"malformed head of type or class declaration", re.IGNORECASE)),
    ("Empty 'do' Block", re.compile(r"empty\s+'do'\s+block", re.IGNORECASE)),
    ("Last Statement in 'do' Block", re.compile(r"the last statement in a 'do' block must be an expression", re.IGNORECASE)),
    ("Invalid Binding Syntax", re.compile(r"illegal binding of built-in syntax", re.IGNORECASE)),
    ("Missing Parentheses in Range Expression", re.compile(r"a section must be enclosed in parentheses", re.IGNORECASE)),
    ("Invalid Enum Deriving", re.compile(r"can't make a derived instance of ['‘`]Enum", re.IGNORECASE)),
    ("Invalid Deriving", re.compile(r"illegal deriving item", re.IGNORECASE)),

    # Warnings and Rest
    ("Warning", re.compile(r"warning", re.IGNORECASE)),
    ("Other Error", re.compile(r".*", re.DOTALL))
])

def classify(stderr: str) -> str:
    for label, pattern in CLUSTERS.items():
        if pattern.search(stderr):
            return label
    return "Other Error"