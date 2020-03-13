"use strict";

const cnst = const_var => x => Number(const_var);
const variable = name => x => x;
const add = (left, right) => x => left(x) + right(x);
const subtract = (left, right) => x => left(x) - right(x);
const multiply = (left, right) => x => left(x) * right (x);
const divide = (left, right) => x => left(x) / right(x);
const negate = arg => x => -arg;

let expr = subtract(
    multiply(
        cnst(2),
        variable("x")
    ),
    cnst(3)
);
console.log(expr(5));

let x = variable("x");
expr = add(subtract(multiply(x, x), multiply(x, cnst(2))), cnst(1));
for (let xi = 0; xi <= 10; xi++) {
    console.log(expr(xi));
}

function skip(str, at, isStart, isPart) {
    if (at === str.length || !isStart(str[at])) {
        return at;
    }
    for (; at < str.length && isPart(str[at]); at++) {
    }
    return at;
}

const isWhitespace = ch => ch === ' ' || ch === '\t' || ch === '\n' || ch === '\r';
const isNonWhitespace = ch => !isWhitespace(ch);
const isPartNum = ch => ch >= '0' && ch <= '9';
const isStartNum = ch => ch === '-' || isPartNum(ch);
const isStartName = ch => ch >= 'a' && ch <= 'b' || ch >= 'A' && ch <= 'B' || ch === '_';
const isPartName = ch => isStartName(ch) || isPartNum(ch);

const skipWs = (str, at) => skip(str, at, isWhitespace, isWhitespace);
const skipNonWs = (str, at) => skip(str, at, isNonWhitespace, isNonWhitespace);
const skipNum = (str, at) => skip(str, at, isStartNum, isPartNum);

function nextToken(str, at) {
    let
        begin = skipWs(str, at),
        end = skipNonWs(str, at);
    return [str.substring(begin, end), end];
}

function checkToken(token, isStart, isPart) {
    if (token.length === 0 || !isStart(token[0])) {
        return false;
    }
    for (let i = 1; i < token.length; i++) {
        if (!isPart(token[i])) {
            return false;
        }
    }
    return true;
}

function addBiOp(stack, op) {
    let right = stack.pop(), left = stack.pop();
    stack.push(op(left, right));
}

function parse(str) {
    let stack = [];
    let at = 0;
    let token = "";
    while (true) {
        [token, at] = nextToken(str, at);
        if (token.length === 0) {
            break;
        }
        switch (token) {
            case '+':
                addBiOp(stack, add);
                break;
            case '-':
                addBiOp(stack, subtract);
                break;
            case '*':
                addBiOp(stack, multiply);
                break;
            case '/':
                addBiOp(stack, divide);
                break;
            default:
                if (checkToken(token, isStartName, isPartName)) {
                    stack.push(variable(token));
                } else {
                    stack.push(cnst(token));
                }
                break;
        }
    }
    return stack.pop();
}

console.log(parse("x x 2 - * x * 1 +")(5));