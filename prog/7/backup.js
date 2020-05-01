function Expression() {
}
Expression.prototype.evaluate = undefined;
Expression.prototype.toString = undefined;

Expression.prototype.simplify = function() { return this; };

function doNothing() {}

function inheritedWithMatchingConstructor(parent, name, prototypeFiller = doNothing, fieldFiller = doNothing) {
    let Constructor = function(...args) {
        parent.apply(this, args);
        fieldFiller.apply(this, args);
    };
    Constructor.prototype = Object.create(parent.prototype);
    Constructor.prototype.constructor = Constructor;
    Constructor.prototype.name = name;
    prototypeFiller(Constructor.prototype);
    return Constructor;
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
            return new Const(0);
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
            return new Const(name === this.name ? 1 : 0);
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

const NAryOp = (N, name) => (operation, symbol, diff, simplify = Expression.prototype.simplify) =>
    inheritedWithMatchingConstructor(Expression, name,
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
            this.args = args.slice(0, N);
        }
    );

const BinaryOp = name => NAryOp(2, name);
const UnaryOp = name => NAryOp(1, name);

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
        return new Multiply(new Const(2), this.args[0].diff(name));
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
            return new Const(0);
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
            return new Const(0);
        }
        if (toNumber(mapped[1]) === 1) {
            return mapped[0];
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
    },
];

const checkToken = (token, isStart, isPart) =>
    token.split("").reduce((result, current, index) =>
        result ? index === 0 ? isStart(current, token) : isPart(current, token) : false, true);

const charChecker = reg => reg.test.bind(reg);

const isDigit = charChecker(/\d/);

const isStartNum = (x, str) => isDigit(x) || x === '-' && str.length > 1;
const isPartNum = isDigit;
const isStartName = charChecker(/[a-zA-Z_]/);
const isPartName = (x, str) => isStartName(x) || isDigit(x);

const applyVarVals = (array, varVals) => array.map(curVal => curVal(...varVals));

const hasOperation = (token) => operationsByArity.reduce(
    (result, array) => result ? true : token in array,
    false
);
const getOperationArgs = (operation, stack, start, arity) =>
    arity === 0 ?
        new operation() :
        new operation(...stack.splice(start, start, start + arity));

const getOperation = (token, stack) => operationsByArity.reduce(
    (result, array, index) =>
        token in array ?
            getOperationArgs(array[token], stack, index - stack.length, stack.length) :
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

const parser = (function () {
    const ParserError = inheritedWithMatchingConstructor(
        Object, "ParserError", doNothing,
        function(at, message) {
            this.at = at;
            this.message = "Cannot parse expression at " + at + ": " + message;
        }
    );

    const SpecificParserError = (name, fieldFiller) => inheritedWithMatchingConstructor(
        ParserError, name, doNothing(), fieldFiller
    );

    const UnexpectedTokenError = SpecificParserError(
        "UnexpectedTokenError",
        function (at, message, token) {
            this.token = token;
        }
    );

    const getValueToken = token => {
        if (checkToken(token, isStartNum, isPartNum)) {
            return new Const(+token);
        }
        else if (checkToken(token, isStartName, isPartName) && token in varName2indexMap) {
            return new Variable(token);
        }
        throw new Error();
    };
    const getOperationArgsGlued = args => {
        if (args.length >= 4 && args[0] === '(' && args[args.length - 1] === ')' &&
            args.length - 3 < operationsByArity.length &&
            args[1] in operationsByArity[args.length - 3]) {
            return getOperationArgs(operationsByArity[args.length - 3][args[1]], args, 2, args.length - 3);
        }
        throw new Error();
    };
    const parsePrefixTokens = tokens =>
        getOperationArgsGlued(tokens.reduceRight((args, current, index, array) => {
            current = current.trim();
            if (args.length > 0 && args[args.length - 1] === ')') {
                return args;
            }
            if (args.length === 0 && current !== '(') {
                throw new Error();
            }
            if (args.length <= 1 || current === ')') {
                args.push(current);
            }
            else if (current === '(') {
                args.push(parsePrefixTokens(array));
            } else {
                args.push(getValueToken(current));
            }
            if (current !== ')') {
                array.pop();
            }
            return args;
        }, []));
    return {
        parsePrefix: str => {
            const tokens = str.trim().split(
                /\s+|(?<=[\(\)])|(?=[\(\)])/
            );
            if (tokens[0] === '(') {
                const res = parsePrefixTokens(tokens.reverse());
                if (tokens.length !== 1) {
                    throw new Error();
                }
                return res;
            } else {
                if (tokens.length !== 1) {
                    throw new Error();
                }
                return getValueToken(tokens[0]);
            }
        }
    };
})();

const parsePrefix = parser.parsePrefix;

let exprr = parsePrefix("(- 3 y )");
console.log(exprr.prefix());
