// TS file for solution 1

var name1: string = "Monty";

class Rabbit{
    name: string;
    constructor(name: string){
        this.name = name
    }
}
var r: Rabbit = new Rabbit("Python");

console.log(r.name);  // ERROR!!!
console.log(name1);    // Prints "Python"


// TS file for solution 2

function swap(arr: number[],i:number,j:number) {
    var tmp:number = arr[i]; arr[i] = arr[j]; arr[j] = tmp;
  }
  function sortAndGetLargest (arr : number[]):number {
    var tmp = arr[0]; // largest elem
    for (var i : number=0; i<arr.length; i++) {
      if (arr[i] > tmp) tmp = arr[i];
      for (var j : number=i+1; j<arr.length; j++)
        if (arr[i] < arr[j]) swap(arr,i,j);
    }
    return tmp;
  }
  var largest : number = sortAndGetLargest([99,2,43,8,0,21,12]);
  console.log(largest); 