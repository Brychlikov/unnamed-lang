const unit = {};
function println(x) {
    if(typeof x == "string") {
        console.log(x);
    }
    else {
        console.log(JSON.stringify(x));
    }
    return unit
}

function print(x) {
    if(typeof x == "string") {
        process.stdout.write(x);
    }
    else {
        process.stdout.write(JSON.stringify(x));
    }
    return unit;
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
function deepEq(x, y) {
    if(typeof x == 'function' || typeof y == 'function'){
        return false;
    }
    if(typeof x == 'object' && typeof y == 'object') {
        let xProps = Object.getOwnPropertyNames(x);
        let yProps = Object.getOwnPropertyNames(y);
        if(xProps.length != yProps.length) {
            return false;
        }
        for(const p of xProps) {
            if(!deepEq(x[p], y[p])) {
                return false;
            }
        }
        return true;
    }
    return x == y;
}
function eq(x) {
    return function (y) {
        return deepEq(x, y);
    }
}
function neq(x) {
    return function (y) {
        return !deepEq(x, y);
    }
}
function pair(x) {
    return function (y) {
        return [x, y];
    }
}

function fst (p) {
    return p[0];
}

function snd (p) {
    return p[1];
}

function seq(x) {
    return function(y) {
        return y;
    }
}

let strConcat = add;

function charList(str) {
    let splt = str.split("");
    let s = splt.reverse();
    let res = {conTag: "Empty", children: []};
    for(const c of s) {
        res = {conTag: "Cons", children: [c, res]};
    }
    return res;
} 

function error(t) {
    throw new Error(t);
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


