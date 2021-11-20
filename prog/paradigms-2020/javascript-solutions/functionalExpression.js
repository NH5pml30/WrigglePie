"use strict";

const cnst = const_var => () => const_var;
const pi = cnst(Math.PI);
const e = cnst(Math.E);

const variableHelper = index => (...varVals) => varVals[index];
const varName2IndexMap = {
    "x": 0,
    "y": 1,
    "z": 2
};
const varName2FuncMap = {};
Object.keys(varName2IndexMap).forEach((name, index) => varName2FuncMap[name] = variableHelper(index));
const variable = name => varName2FuncMap[name];

const nary = oper => (...args) => (...varVals) => oper.call(null, args.map(arg => arg(...varVals)), args, varVals);

// :NOTE: copy-paste code for operation declaration (at least call for `args[0]` and `args [1]`)
const add = nary(args => args[0] + args[1]);
const subtract = nary(args => args[0] - args[1]);
const multiply = nary(args => args[0] * args[1]);
const divide = nary(args => args[0] / args[1]);
const negate = nary(args => -args[0]);

const checkToken = (token, isStart, isPart) =>
    token.split("").reduce((result, current, index) =>
        result ? index === 0 ? isStart(current, token) : isPart(current, token) : false, true);

const charChecker = reg => reg.test.bind(reg);

const isDigit = charChecker(/\d/);

const isStartNum = (x, str) => isDigit(x) || x === '-' && str.length > 1;
const isPartNum = isDigit;

const applyVarVals = (array, varVals) => array.map(curVal => curVal(...varVals));

const sumN = length => nary(args => args.reduce((sum, x) => sum + x, 0));
const medN = length => nary(args => args.sort((x, y) => x - y)[Math.floor(length / 2)]);
const avgN = length => nary((args, fArgs, varVals) => sumN(length)(...fArgs)(...varVals) / length);

const med3 = medN(3);
const avg5 = avgN(5);

const operationsByArity = [
    {
        "pi": pi,
        "e": e,
    },
    {
        "negate": negate,
    },
    {
        "+": add,
        "-": subtract,
        "*": multiply,
        "/": divide,
    },
    {
        "med3": med3
    },
    {},
    {
        "avg5": avg5
    }
];

const hasOperation = (token) => operationsByArity.reduce(
    (result, array) => result ? true : token in array,
    false
);
const getOperation = (token, stack) => operationsByArity.reduce(
    (result, array, index) =>
        token in array ?
            index === 0 ?
                array[token] :
                array[token](...stack.splice(stack.length - index, index)) :
            result,
    undefined
);

const parse = str =>
    str.trim().split(/\s+/).reduce((stack, current) => {
        checkToken(current, isStartNum, isPartNum) ?
            stack.push(cnst(+current)) :
            hasOperation(current) ?
                stack.push(getOperation(current, stack)) :
                stack.push(variable(current));
        return stack;
    }, []).pop();
