"use strict";

const cnst = const_var => map => const_var;
const pi = cnst(Math.PI);
const e = cnst(Math.E);
const variable = name => (...args) => name === "x" ? args[0] : name === "y" ? args[1] : args[2];

const binary = oper => (left, right) => (...args) => oper(left(...args), right(...args));
const unary = oper => arg => (...args) => oper(arg(...args));

const add = binary((left, right) => left + right);
const subtract = binary((left, right) => left - right);
const multiply = binary((left, right) => left * right);
const divide = binary((left, right) => left / right);
const negate = unary(arg => -arg);
const sin = unary(Math.sin);
const cos = unary(Math.cos);
const cube = unary(arg => arg * arg * arg);
const cuberoot = unary(Math.cbrt);

const checkToken = (token, isStart, isPart) =>
    token.split("").reduce((result, current, index) =>
        result ? index === 0 ? isStart(current, token) : isPart(current, token) : false, true);

const charChecker = reg => reg.test.bind(reg);

const isDigit = charChecker(/\d/);
const isAlpha = charChecker(/[a-zA-Z]/);
const isStartIdentifier = charChecker(/[a-zA-Z0-9]/);
const isPartIdentifier = charChecker(/\w/);

const isStartNum = (x, str) => isDigit(x) || x === '-' && str.length > 1;
const isPartNum = isDigit;

const reverseArgs = f => (right, left) => f(left, right);

const binaryOperationMap = {
    "+": add,
    "-": subtract,
    "*": multiply,
    "/": divide,
};

const unaryOperationMap = {
    "negate": negate,
    "sin": sin,
    "cos": cos,
    "cube": cube,
    "cuberoot": cuberoot,
};

const constMap = {
    "pi": pi,
    "e": e,
};

const parse = str =>
    str.trim().split(/\s+/).reduce((stack, current) => {
        checkToken(current, isStartNum, isPartNum) ?
            stack.push(cnst(+current)) :
            current in binaryOperationMap ?
                stack.push(reverseArgs(binaryOperationMap[current])(stack.pop(), stack.pop())) :
                current in unaryOperationMap ?
                    stack.push(unaryOperationMap[current](stack.pop())) :
                    current in constMap ?
                        stack.push(constMap[current]) :
                        stack.push(variable(current));
        return stack;
    }, []).pop();
