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

const Const = inheritedWithMatchingConstructor(
    Expression, "Const",
    function(proto) {
        proto.evaluate = function(...varVals) {
            return this.value;
        };
        proto.toString = function() {
            return this.value.toString();
        };
        proto.diff = function(name) {
            return Const.ZERO;
        };
        proto.simplify = function(level) {
            return level === 0 ? this.value : this;
        };
        proto.prefix = proto.toString;
    },
    function(cnst) {
        this.value = cnst;
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
const Variable = inheritedWithMatchingConstructor(
    Expression, "Variable",
    function(proto) {
        proto.evaluate = function(...varVals) {
            return varVals[varName2indexMap[this.name]];
        };
        proto.toString = function() {
            return this.name;
        };
        proto.diff = function(name) {
            return name === this.name ? Const.ONE : Const.ZERO;
        };
        proto.prefix = proto.toString;
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

const NAryOp = (name) => (operation, symbol, diff, simplify = Expression.prototype.simplify) =>
    inheritedWithMatchingConstructor(Operation, name,
        function(proto) {
            proto.operation = operation;
            proto.evaluate = function(...varVals) {
                return operation(
                    ...this.args.map(x => x.evaluate(...varVals))
                );
            };
            proto.argsToString = function(stringifier) {
                return this.args.reduce(
                    (str, x, ind) =>
                        str + (ind === 0 ? '' : ' ') + stringifier(x), ""
                );
            };
            proto.toString = function() {
                return this.argsToString(x => x.toString()) + ' ' + symbol;
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
            proto.prefix = function() {
                return '(' + symbol + ' ' + this.argsToString(x => x.prefix()) + ')';
            }
        },
        function(...args) {
            this.args = args.slice(0);
        }
    );

const BinaryOp = name => NAryOp(name);
const UnaryOp = name => NAryOp(name);

const Negate = UnaryOp("Negate")(
    x => -x,
    'negate',
    function(name) {
            return new Negate(this.args[0].diff(name));
    }
);
const Square = UnaryOp("Square")(
    x => x * x,
    'sqr',
    function(name) {
        return new Multiply(Const.TWO, this.args[0].diff(name));
    }
);

const Add = BinaryOp("Add")(
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
const Subtract = BinaryOp("Subtract")(
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
const Multiply = BinaryOp("Multiply")(
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
const Divide = BinaryOp("Divide")(
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
const Power = BinaryOp("Power")(
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
const Log = BinaryOp("Logarithm")(
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

const Sumexp = NAryOp("Sumexp")(
    sumexp,
    'sumexp',
    function(name) {
        return this.args.map(expr => new Multiply(expr.diff(name), new Power(Const.E, expr))).reduce(
            (prev, expr) => new Add(prev, expr), Const.ZERO);
    }
);
const Softmax = NAryOp("Softmax")(
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

const parser = (function () {
    function _error(creator, message) {
        return creator(this.at, message);
    }
    function _expect(creator, message, condition) {
        if (!condition) {
            throw this.error(creator, message);
        }
    }
    function _expectLastToken(message, condition) {
        if (!condition(this.lastToken)) {
            throw this.error(unexpectedTokenCreator(this.lastToken), message);
        }
    }
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
    const unexpectedTokenCreator = token => (at, message) => new UnexpectedTokenError(at, message, token === null ? "<end of expression>" : token);
    const UnrecognizedTokenError = SpecificParserError(
        "UnrecognizedTokenError",
        (at, message, token = "") => [at, "unrecognized token" + (token === "" ? "" : " '" + token + "'") + ": " + message],
        function (at, message, token) {
            this.token = token;
        }
    );
    const unrecognizedTokenCreator = (token = "") => (at, message) => new UnrecognizedTokenError(at, message, token);

    function _skipWs() {
        while (this.at < this.str.length && isWhitespace(this.str[this.at])) {
            this.at++;
        }
    }
    function _read(isPart) {
        let last = this.str[this.at];
        const begin = this.at++;
        while (this.at < this.str.length && isPart(this.str[this.at], last)) {
            last = this.str[this.at++];
        }
        return this.str.substring(begin, this.at);
    }
    function _nextToken() {
        this.skipWs();
        if (this.at === this.str.length) {
            this.lastToken = null;
        } else {
            const token = this.read((x, prev) => !isWhitespace(x) && x !== '(' && x !== ')' && prev !== ')' && prev !== '(');
            if (isNumber(token)) {
                this.lastToken = new Const(+token);
            } else if (token in varName2indexMap) {
                this.lastToken = new Variable(token);
            } else {
                this.expect(unrecognizedTokenCreator(token), "no variable/function with this name",
                    hasOperationExt(token) || token === '(' || token === ')');
                this.lastToken = token;
            }
        }
        return this.lastToken;
    }
    function _getOperationArgsGlued(operation, args) {
        this.expect(
            unrecognizedTokenCreator(operation), "cannot find operation " + operation + " taking " + args.length + " arguments",
            hasOperationExtN(operation, args.length)
        );
        return getOperationExt(operation, args);
    }
    function _parseArgs(recurse, breakWhen) {
        const args = [];
        let current;
        while ((current = this.nextToken()) !== null && !breakWhen(current)) {
            if (current === '(') {
                args.push(recurse(breakWhen));
                this.expectLastToken("expected closing parenthesis", t => t === ')');
            } else {
                this.expectLastToken("expected number or variable", t => t instanceof Const || t instanceof Variable);
                args.push(current);
            }
        }
        return args;
    }
    const isOperation = str => typeof str === 'string' && str !== '(' && str !== ')';
    function _parseOperation(readToken) {
        const operation = readToken === undefined ? this.nextToken(true) : readToken;
        this.expectLastToken("expected operation", t => isOperation(t));
        return operation;
    }
    function _parse_fixTokens(isPostfix = false) {
        let operation, args;
        if (!isPostfix) {
            operation = this.parseOperation();
            args = this.parseArgs(this.parse_fixTokens.bind(this, isPostfix), x => x === ')');
        } else {
            args = this.parseArgs(this.parse_fixTokens.bind(this, isPostfix), x => isOperation(x) || x === ')');
            operation = this.parseOperation(this.lastToken);
            this.nextToken();
        }
        this.expectLastToken("expected closing parenthesis", t => t === ')');
        return this.getOperationArgsGlued(operation, args);
    }

    const parserCreator = function(input) {
        return {
            str: input,
            at: 0,
            skipWs: _skipWs,
            read: _read,
            nextToken: _nextToken,
            getOperationArgsGlued: _getOperationArgsGlued,
            parseOperation: _parseOperation,
            parseArgs: _parseArgs,
            parse_fixTokens: _parse_fixTokens,
            error: _error,
            expect: _expect,
            expectLastToken: _expectLastToken
        };
    };

    function parse_fix(input, isPostfix) {
        const parser = parserCreator(input);

        const first = parser.nextToken();
        let res;
        if (first === '(') {
            res = parser.parse_fixTokens(isPostfix);
        } else {
            parser.expectLastToken("expected '(', number or variable", t => t instanceof Const || t instanceof Variable);
            res = first;
        }
        parser.nextToken();
        parser.expectLastToken("expected end of expression", t => t === null);
        return res;
    }

    return {
        ParserError: ParserError,
        UnexpectedTokenError: UnexpectedTokenError,
        parsePrefix: str => parse_fix(str, false),
        parsePostfix: str => parse_fix(str, true)
    };
})();

const parsePrefix = parser.parsePrefix;
const parsePostfix = parser.parsePostfix;
