function Expression() {
}
Expression.prototype.evaluate = undefined;
Expression.prototype.toString = undefined;

Expression.prototype.simplify = function() { return this; };

function doNothing() {}

function inheritedWithMatchingConstructor(parent, prototypeFiller = doNothing, fieldFiller = doNothing) {
    let Constructor = function(...args) {
        parent.apply(this, args);
        fieldFiller.apply(this, args);
    };
    Constructor.prototype = Object.create(parent.prototype);
    Constructor.prototype.constructor = Constructor;
    prototypeFiller(Constructor.prototype);
    return Constructor;
}

const Const = inheritedWithMatchingConstructor(
    Expression,
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
    Expression,
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
        proto.prefix = proto.toString();
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

const NAryOp = N => (operation, symbol, diff, simplify = Expression.prototype.simplify) =>
    inheritedWithMatchingConstructor(Expression,
        function(proto) {
            proto.operation = operation;
            proto.evaluate = function(...varVals) {
                return operation(
                    ...this.args.map(x => x.evaluate(...varVals))
                );
            };
            proto.argsToString = function(stringifier) {
                return this.args.reduce((str, x) => str + ' ' + stringifier(x));
            };
            proto.toString = function() {
                return proto.argsToString.call(this, x => x.toString()) + ' ' + symbol;
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
                return '(' + symbol + ' ' + proto.argsToString.call(this, x => x.prefix()) + ')';
            }
        },
        function(...args) {
            this.args = args.slice(0, N);
        }
    );

const BinaryOp = NAryOp(2);
const UnaryOp = NAryOp(1);

const Negate = UnaryOp(
    x => -x,
    'negate',
    function(name) {
            return new Negate(this.args[0].diff(name));
    }
);
const Square = UnaryOp(
    x => x * x,
    'sqr',
    function(name) {
        return new Multiply(new Const(2), this.args[0].diff(name));
    }
);

const Add = BinaryOp(
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
const Subtract = BinaryOp(
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
const Multiply = BinaryOp(
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
const Divide = BinaryOp(
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

const getOperationArgsGlued = args =>
    getOperationArgs(operationsByArity[args.length - 3][args[1]], args, 2, args.length - 3);

const parsePrefixTokens = tokens =>
    getOperationArgsGlued(tokens.reduceRight((args, current, index, array) => {
        current = current.trim();
        if (args.length > 0 && args[args.length - 1] === ')') {
            return args;
        }
        args.length <= 1 ?
            args.push(current) :
            current === '(' ?
                args.push(parsePrefixTokens(array)) :
                current === ')' ?
                    args.push(')') :
                    checkToken(current, isStartNum, isPartNum) ?
                        args.push(new Const(+current)) :
                        args.push(new Variable(current));
        if (current !== ')') {
            array.pop();
        }
        return args;
    }, []));

const parsePrefix = str =>
    parsePrefixTokens(str.trim().split(/(?<=\()|(?=\()|(?<=\))|(?=\))|\s+/).reverse());

console.log(parsePrefix("(-(* -2 x)3)").prefix());