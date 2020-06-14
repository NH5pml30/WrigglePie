// :NOTE: strict mode is switched off
// :NOTE 2: still be
/* Review */

"use strict";

// :NOTE: common mistakes: 7


function Expression() {
}
Expression.prototype.simplify = function() { return this; };

function doNothing() {}

function inheritedWithMatchingConstructor(parent, name, prototypeFiller = doNothing, fieldFiller = doNothing, args2parent = (...args) => args) {
    // now console.log(new ...) outputs <name> {...} and not Constructor {...}
    const Constructor = {
        [name]: function(...args) {
            parent.apply(this, args2parent(...args));
            fieldFiller.apply(this, args);
        }
    };
    const Res = Constructor[name];
    Res.prototype = Object.create(parent.prototype);
    Res.prototype.constructor = Res;
    Res.prototype.name = name;
    prototypeFiller(Res.prototype);
    return Res;
}

// :NOTE: copy-pasted code for Const and Variable (at least passage of `Expression`, `proto.prefix = proto.toString`, ...)
function createValue(name, evaluate, toString, diff, fieldFiller, simplify = Expression.prototype.simplify) {
    return inheritedWithMatchingConstructor(Expression, name,
        function(proto) {
            proto.evaluate = evaluate;
            proto.diff = diff;
            proto.simplify = simplify;
            proto.prefix = proto.postfix = proto.toString = toString;
        }, fieldFiller);
}

const Const = createValue("Const",
    function(...varVals) {
        return this.value;
    },
    function() {
        return this.value.toString();
    },
    name => Const.ZERO,
    function(cnst) {
        this.value = cnst;
    },
    function(level) {
        return level === 0 ? this.value : this;
    }
);
Const.E = new Const(Math.E);
Const.ZERO = new Const(0);
Const.ONE = new Const(1);
Const.TWO = new Const(2);

const varName2indexMap = {
    "x": 0,
    "y": 1,
    "z": 2
};
const Variable = createValue("Variable",
    function(...varVals) {
        return varVals[varName2indexMap[this.name]];
    },
    function() {
        return this.name;
    },
    function(name) {
        return name === this.name ? Const.ONE : Const.ZERO;
    },
    function(name) {
        this.name = name;
    }
);

function defaultOperationSimplify() {
    let flag = true;
    let mapped = this.args.map(x => {
        let c = x.simplify();
        flag &= c instanceof Const;
        return c;
    });
    return flag ?
        new Const(this.operation(...mapped.map(x => x.value))) :
        new this.constructor(...mapped);
}

const Operation = inheritedWithMatchingConstructor(Expression, "Operation");

const NAryOp = (name, operation, symbol, diff, simplify = Expression.prototype.simplify) =>
    inheritedWithMatchingConstructor(Operation, name,
        function(proto) {
            proto.operation = operation;
            proto.evaluate = function(...varVals) {
                return operation(
                    ...this.args.map(x => x.evaluate(...varVals))
                );
            };
            proto.argsToString = function(prefix, isPostfix, postfix, extPrefix, extPostfix) {
                return prefix + this.args.reduce(
                    (str, x, ind) =>
                        str + (ind === 0 ? '' : ' ') + (isPostfix ? x.postfix : x.prefix).bind(x)(extPrefix, extPostfix), ""
                ) + postfix;
            };
            proto.toString = function() {
                return this.postfix('', '');
            };
            proto.diff = diff;
            proto.simplify = function() {
                let c = defaultOperationSimplify.call(this);
                if (c instanceof Const) {
                    return c;
                }

                return simplify.call(
                    c,
                    this.args.map(x => x.simplify()),
                    x => x instanceof Const ? x.value : undefined
                );
            };
            proto.prefix = function(prefix = '(', postfix = ')') {
                return this.argsToString(prefix + symbol + ' ', false, postfix, prefix, postfix);
            };
            proto.postfix = function(prefix = '(', postfix = ')') {
                return this.argsToString(prefix, true, ' ' + symbol + postfix, prefix, postfix);
            };
        },
        function(...args) {
            this.args = args.slice(0);
        }
    );

const BinaryOp = NAryOp;
const UnaryOp = NAryOp;

// :NOTE: the second call `UnaryOp (...)--->()<---` is not required by JS. If it's removed code still be compiling but logic lost.
// User can forget to call this method
const Negate = UnaryOp("Negate",
    x => -x,
    'negate',
    function(name) {
        return new Negate(this.args[0].diff(name));
    }
);
const Square = UnaryOp("Square",
    x => x * x,
    'sqr',
    function(name) {
        return new Multiply(Const.TWO, this.args[0].diff(name));
    }
);

const Add = BinaryOp("Add",
    (x, y) => x + y,
    '+',
    function(name) {
        return new Add(this.args[0].diff(name), this.args[1].diff(name));
    },
    function(mapped, toNumber) {
        if (toNumber(mapped[0]) === 0) {
            return mapped[1];
        }
        if (toNumber(mapped[1]) === 0) {
            return mapped[0];
        }
        return this;
    }
);
const Subtract = BinaryOp("Subtract",
    (x, y) => x - y,
    '-',
    function(name) {
        return new Subtract(this.args[0].diff(name), this.args[1].diff(name));
    },
    function(mapped, toNumber) {
        if (toNumber(mapped[1]) === 0) {
            return mapped[0];
        }
        return this;
    }
);
const Multiply = BinaryOp("Multiply",
    (x, y) => x * y,
    '*',
    function(name) {
        return new Add(
            new Multiply(this.args[0].diff(name), this.args[1]),
            new Multiply(this.args[0], this.args[1].diff(name))
        );
    },
    function(mapped, toNumber) {
        if (toNumber(mapped[0]) === 0 || toNumber(mapped[1]) === 0) {
            return Const.ZERO;
        }
        if (toNumber(mapped[0]) === 1) {
            return mapped[1];
        }
        if (toNumber(mapped[1]) === 1) {
            return mapped[0];
        }
        return this;
    }
);
const Divide = BinaryOp("Divide",
    (x, y) => x / y,
    '/',
    function(name) {
        return new Divide(
            new Subtract(
                new Multiply(this.args[0].diff(name), this.args[1]),
                new Multiply(this.args[0], this.args[1].diff(name))
            ),
            new Multiply(this.args[1], this.args[1])
        );
    },
    function(mapped, toNumber) {
        if (toNumber(mapped[0]) === 0) {
            return Const.ZERO;
        }
        if (toNumber(mapped[1]) === 1) {
            return mapped[0];
        }
        return this;
    }
);
const Power = BinaryOp("Power",
    (x, y) => Math.pow(x, y),
    'pow',
    function(name) {
        return new Multiply(
            new Add(
                new Divide(
                    new Multiply(this.args[0].diff(name), this.args[1]),
                    this.args[0]
                ),
                new Multiply(new Log(Const.E, this.args[0]), this.args[1].diff(name))
            ),
            this
        );
    },
    function(mapped, toNumber) {
        if (toNumber(mapped[0]) === 0 || toNumber(mapped[0]) === 1) {
            return mapped[0];
        }
        if (toNumber(mapped[1]) === 1) {
            return mapped[0];
        }
        if (toNumber(mapped[1]) === 0) {
            return Const.ONE;
        }
        return this;
    }
);
const Log = BinaryOp("Logarithm",
    (x, y) => Math.log(Math.abs(y)) / Math.log(Math.abs(x)),
    'log',
    function(name) {
        return new Divide(
            new Subtract(
                new Divide(this.args[1].diff(name), this.args[1]),
                new Multiply(this, new Divide(this.args[0].diff(name), this.args[0]))
            ),
            new Log(Const.E, this.args[0])
        );
    },
    function(mapped, toNumber) {
        if (toNumber(mapped[1]) === 1 || toNumber(mapped[1]) === -1) {
            return Const.ZERO;
        }
        return this;
    }
);

const operationsByArity = [
    {
    },
    {
        "negate": Negate,
    },
    {
        "+": Add,
        "-": Subtract,
        "*": Multiply,
        "/": Divide,
        "log": Log,
        "pow": Power,
    },
];


/***
 * Homework #7
 ***/

const checkToken = (token, isStart, isPart) =>
    token.split("").reduce((result, current, index) =>
        result ? index === 0 ? isStart(current, token) : isPart(current, token) : false, true);

const charChecker = reg => reg.test.bind(reg);

const isDigit = charChecker(/\d/);
const isStartNum = (x, str) => isDigit(x) || x === '-' && str.length > 1;
const isPartNum = isDigit;
const isWhitespace = charChecker(/\s/);
const isNumber = str => str.length > 0 && Array.from(str).reduce((prev, ch) => prev === undefined ? isStartNum(ch, str) : isPartNum(ch), undefined);

const hasOperation = (token) => operationsByArity.reduce(
    (result, array) => result ? true : token in array,
    false
);
const getOperationArgs = (operation, stack, start, arity) =>
    arity === 0 ?
        new operation() :
        new operation(...stack.splice(start, arity));

const getOperation = (token, stack) => operationsByArity.reduce(
    (result, array, index) =>
        token in array ?
            getOperationArgs(array[token], stack, stack.length - index, index) :
            result,
    undefined
);

const parse = str =>
    str.trim().split(/\s+/).reduce((stack, current) => {
        checkToken(current, isStartNum, isPartNum) ?
            stack.push(new Const(+current)) :
            hasOperation(current) ?
                stack.push(getOperation(current, stack)) :
                stack.push(new Variable(current));
        return stack;
    }, []).pop();

/***
 * Homework #8
 ***/

const sumexp = (...args) => args.reduce((prev, x) => prev + Math.exp(x), 0);

const Sumexp = NAryOp("Sumexp",
    sumexp,
    'sumexp',
    function(name) {
        return this.args.map(expr => new Multiply(expr.diff(name), new Power(Const.E, expr))).reduce(
            (prev, expr) => new Add(prev, expr), Const.ZERO);
    }
);
const Softmax = NAryOp("Softmax",
    (...args) => Math.exp(args[0]) / sumexp(...args),
    'softmax',
    function(name) {
        return new Divide(new Power(Const.E, this.args[0]), new Sumexp(...this.args)).diff(name);
    }
);

const nAryOperations = {
    "sumexp": Sumexp,
    "softmax": Softmax
};

const hasOperationExt = (operation) => operation in nAryOperations || hasOperation(operation);
const hasOperationExtN = (operation, arity) => operation in nAryOperations ?
    true :
    arity < operationsByArity.length && operation in operationsByArity[arity];
const getOperationExt = (operation, stack) =>
    operation in nAryOperations ?
        getOperationArgs(nAryOperations[operation], stack, 0, stack.length) :
        getOperation(operation, stack);

const source = (function() {
    const ParserError = inheritedWithMatchingConstructor(
        Object, "ParserError", doNothing,
        function (at, message) {
            this.at = at;
            this.message = "Cannot parse expression at " + at + ": " + message;
        }
    );
    const SpecificParserError = (name, args2parent, fieldFiller) => inheritedWithMatchingConstructor(
        ParserError, name, doNothing(), fieldFiller, args2parent
    );
    const UnexpectedTokenError = SpecificParserError(
        "UnexpectedTokenError",
        (at, message, token = "") => [at, "unexpected token" + (token === "" ? "" : " '" + token + "'") + ": " + message],
        function (at, message, token) {
            this.token = token;
        }
    );
    const unexpectedTokenCreator = token => (at, message) => new UnexpectedTokenError(at, message, token || "<end of expression>");
    const UnrecognizedTokenError = SpecificParserError(
        "UnrecognizedTokenError",
        (at, message, token = "") => [at, "unrecognized token" + (token === "" ? "" : " '" + token + "'") + ": " + message],
        function (at, message, token) {
            this.token = token;
        }
    );
    const unrecognizedTokenCreator = (token = "") => (at, message) => new UnrecognizedTokenError(at, message, token || "<end of expression>");

    function Source(input) {
        this._str = input;
        this._at = 0;
        this.expect((at, message) => new ParserError(at, message), "Empty source string", input !== "");
        this.lastToken = null;
        const id = this._parseIdentifier.bind(this), co = this._parseConst.bind(this);
        this.valueParsers = [co, id];
        this.funcParsers = [id];
        this.allParsers = [...this.valueParsers, ...this.funcParsers].filter((it, ind, obj) => obj.indexOf(it) === ind);
    }
    Source.prototype.error = function(creator, message) {
        return creator(this._at, message);
    };
    Source.prototype.expect = function(creator, message, condition) {
        if (!condition) {
            throw this.error(creator, message);
        }
    };
    Source.prototype.expectToken = function(message, token, condition) {
        if (!condition(token)) {
            throw this.error(unexpectedTokenCreator(token), message);
        }
    };
    Source.prototype.expectEnd = function() {
        const token = this._at < this._str.length ? this._str.substring(this._at) : this.lastToken;
        if (token) {
            throw this.error(unexpectedTokenCreator(token), "Expected end of expression");
        }
    };
    Source.prototype.getChar = function(pos = this._at) {
        return this._str[pos];
    };
    Source.prototype.testChar = function(pred) {
        if (this._at < this._str.length && pred(this.getChar())) {
            this._at++;
            return true;
        }
        return false;
    };
    Source.prototype.expectChar = function(pred, message) {
        if (!this.testChar(pred)) {
            throw this.error(unexpectedTokenCreator(this.getChar()), message);
        }
    };
    Source.prototype.skipWs = function() {
        while (this._at < this._str.length && isWhitespace(this.getChar())) {
            this._at++;
        }
        return this;
    };
    Source.prototype._readWhile = function(isStart, isPart) {
        if (!isStart(this.getChar())) {
            return null;
        }
        const begin = this._at;
        let end = this._at + 1;
        while (end < this._str.length && isPart(this.getChar(end))) {
            end++;
        }
        return this._str.substring(begin, end);
    };
    Source.prototype._readRegex = function(regex) {
        regex.lastIndex = this._at;
        const res = this._str.match(regex);
        return res ? res[0] : null;
    };
    Source.prototype._commitToken = function(token) {
        if (token !== null) {
            this._at += token.length;
        }
        return token;
    };
    const constRegexp = /[+\-]?\d+(?:\.\d+)?(?=(?:\)|\(|\s|$))/y;
    Source.prototype._parseConst = function() {
        const token = this._commitToken(this._readRegex(constRegexp));
        if (!token) { return null; }
        return new Const(+token);
    };
    Source.prototype._parseIdentifier = function() {
        const token = this._commitToken(this._readWhile(
            x => !isWhitespace(x) && !isDigit(x) && x !== '(' && x !== ')',
            x => !isWhitespace(x) && x !== '(' && x !== ')'
        ));
        if (!token) { return null; }
        this.expectToken("no variable/function with this name", token, token => token in varName2indexMap || hasOperationExt(token));
        return token in varName2indexMap ? new Variable(token) : token;
    };
    Source.prototype.parseSomething = function(readToken, parsers, checker, message) {
        this.skipWs();
        if (readToken === undefined) {
            this.lastToken = readToken = parsers.reduce((prev, cur) => prev ? prev : cur(), null);
        }
        this.expectToken(message, readToken, checker);
        return readToken;
    };

    return {
        Source: Source,
        ParserError: ParserError,
        UnexpectedTokenError: UnexpectedTokenError,
        UnrecognizedTokenError: UnrecognizedTokenError,
        unexpectedTokenCreator: unexpectedTokenCreator,
        unrecognizedTokenCreator: unrecognizedTokenCreator
    };
})();

// :NOTE: too many code for parser. The limit is 50-60 lines of code (without blaank lines)
const parser = (function () {
    const isOperation = str => typeof str === 'string' && str !== '(' && str !== ')';
    const isValue = token => token instanceof Const || token instanceof Variable;

    const Parser = function(input) {
        this.source = new source.Source(input);
        this._tryReadValue();
    };
    Parser.prototype._nextToken = function(readToken) {
        return this.source.parseSomething(readToken, this.source.allParsers, () => true)
    };
    Parser.prototype._parseOperation = function(readToken) {
        return this.source.parseSomething(readToken, this.source.funcParsers, isOperation, "expected operation");
    };
    Parser.prototype._tryReadValue = function() {
        return this.source.parseSomething(undefined, this.source.valueParsers, () => true);
    };
    Parser.prototype._parseTokens = function(recur, level = 0) {
        level || this._tryReadValue();
        let current = recur();
        return current ? [current, ...this._parseTokens(recur, 1)] : [];
    };
    Parser.prototype._shiftToken = function() {
        const res = this.source.lastToken;
        this._nextToken();
        return res;
    };
    Parser.prototype._getOperationArgsGlued = function(operation, args) {
        this.source.expect(
            source.unrecognizedTokenCreator(operation), "cannot find operation " + operation + " taking " + args.length + " arguments",
            hasOperationExtN(operation, args.length)
        );
        return getOperationExt(operation, args);
    };
    Parser.prototype.parseExpr = function(isPostfix = false) {
        let operation, args;
        if (isValue(this.source.lastToken)) {
            return this._shiftToken();
        } else if (!this.source.lastToken && this.source.testChar(x => x === '(')) {
            isPostfix || (operation = this._parseOperation());
            args = this._parseTokens(this.parseExpr.bind(this, isPostfix));
            !isPostfix || (operation = this._parseOperation(this._shiftToken()));
            this.source.expectToken("expected closing parenthesis", this.source.lastToken, token => !token);
            this.source.skipWs().expectChar(ch => ch === ')', "expected closing parenthesis");
            this._nextToken();
            return this._getOperationArgsGlued(operation, args);
        }
        return null;
    };
    Parser.prototype.checkEnd = function() { this.source.expectEnd(); };

    function parse_fix(input, isPostfix) {
        const parser = new Parser(input);
        const res = parser.parseExpr(isPostfix);
        parser.checkEnd();
        return res;
    }

    return {
        parsePrefix: str => parse_fix(str.trim(), false),
        parsePostfix: str => parse_fix(str.trim(), true)
    };
})();

const parsePrefix = parser.parsePrefix;
const parsePostfix = parser.parsePostfix;
