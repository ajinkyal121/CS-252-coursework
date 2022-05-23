let proxyHandler = {
    get (target, name) {
        if (name in target){
            console.log('Get value: ', name);
            return target[name];
        }
        console.log('Get value not present');
    },
    set (target, name, val) {
        if (name in target){
            console.log('Set value: ', name, ' to ', val);
            return target[name];
        }
        console.log('Set value not present')
    },
    deleteProperty (target, name) {
        if(name in target){
            delete target[name]
            console.log('Delete value: ', name);
            return target[name];
        }
        console.log('Delete value not present')
    },
    apply (target, thisArg, argList) {
        console.log('Apply: ', argList);
        return target(argList[0]);
    },
    has (target, prop) {
        console.log('Has value : ', prop);
        return prop in target;
    },
    construct (target, args) {
        console.log('construct: ', args)
        return new target(args);
    }
};

let obj = {
    a : 4,
    b: 5,
    c: 'test val'
};

let prox = new Proxy(obj, proxyHandler);
console.log(prox.c); // get
console.log(prox.c = 'Test value'); // set
console.log(prox.c); // get
console.log(delete prox.b); // delete
console.log(prox.b) // get
console.log("c" in prox); // has
console.log("r" in prox); // has

let Test = function(name){
  this.name = name;
}
Test = new Proxy(Test, proxyHandler);
let test = new Test("One");

let applyTest = function(name){
    return "Apply called with " + name;
}
console.log(applyTest(" without proxy"));
var prox2 = new Proxy(applyTest, proxyHandler);
console.log(prox2("with proxy"));