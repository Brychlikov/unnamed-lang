const unit = {};
function print(x) {
    console.log(x);
    return unit
}
function add(x) {
    return function (y) {
        return x + y;
    }
}
function sub(x) {
    return function (y) {
        return x - y;
    }
}
function mult(x) {
    return function (y) {
        return x * y;
    }
}
function div(x) {
    return function (y) {
        return x / y;
    }
}
function eq(x) {
    return function (y) {
        return x === y;
    }
}
function neq(x) {
    return function (y) {
        return x !== y;
    }
}
function pair(x) {
    return function (y) {
        return [x, y];
    }
}
function jump(func, ...args) {
    return { yip: "yip", func, args };
}


function trampoline(func, ...args) {
    let res = func(...args);
    while(res.yip === "yip") {
        let {func, args} = res;
        res = func (...args);
    }
    return res;
}

