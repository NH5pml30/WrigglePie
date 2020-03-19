"use strict";

const cnst = const_var => () => const_var;
const pi = cnst(Math.PI);
const e = cnst(Math.E);
const variable = name => (...varVals) => name === "x" ? varVals[0] : name === "y" ? varVals[1] : varVals[2];

const binary = oper => (left, right) => (...varVals) => oper(left(...varVals), right(...varVals));
const unary = oper => arg => (...varVals) => oper(arg(...varVals));

const add = binary((left, right) => left + right);
const subtract = binary((left, right) => left - right);
const multiply = binary((left, right) => left * right);
const divide = binary((left, right) => left / right);
const negate = unary(arg => -arg);

const checkToken = (token, isStart, isPart) =>
    token.split("").reduce((result, current, index) =>
        result ? index === 0 ? isStart(current, token) : isPart(current, token) : false, true);

const charChecker = reg => reg.test.bind(reg);

const isDigit = charChecker(/\d/);

const isStartNum = (x, str) => isDigit(x) || x === '-' && str.length > 1;
const isPartNum = isDigit;

const applyVarVals = (array, varVals) => array.map(curVal => curVal(...varVals));

const sumN = length => (...args) => (...varVals) => applyVarVals(args, varVals).reduce((sum, x) => sum + x, 0);
const medN = length => (...args) => (...varVals) => applyVarVals(args, varVals).sort((x, y) => x - y)[Math.floor(length / 2)];
const avgN = length => (...args) => (...varVals) => sumN(length)(...args)(...varVals) / length;

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

console.log(med3(variable('x'), variable('y'), variable('z'))(0, 1, 2));
