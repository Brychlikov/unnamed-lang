function even (n) {
    if(n == 0) {
        return true;
    }
    else {
        return jump(odd, n-1);
    }
}

function odd (n) {
    if (n == 0) {
        return false;
    }
    if (n == 1) {
        return true;
    }
    else {
        return jump(even, n-1);
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
function cnt_r(acc, n) {
    if (n == 0) {
        return acc;
    } 
    else {
        return jump(cnt_r, acc + 1, n -1);
    }
}


console.log(trampoline(even, 1000001));
// console.log(even(1000000));