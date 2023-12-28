DummyLang sample : 
```js
\ non-recursive func\
fun factorial(number) {
    var result = 1;
    while(number > 0) {
        result <- result * number;
        number <- number - 1;
    }
    return result;
}

\ recursive func \
fun fibonacci(number) {
    if((number = 0) | (number = 1)) {
        return number;
    } else {
        return (fibonacci(number - 1)) + (fibonacci(number - 2));
    }
}

var input = read();
return fibonacci(input);
```

DummyVM F# DSL :
```fs
let bytecode =
  Build {
      Push 23
      Push 03
      Mul
      Returm
  } 
```


DummyVM Raw Bytecode :
```
let sumUpTo n = 
    [
        01uy; 00uy; 00uy; 00uy; 63uy;
        00uy; 00uy; 00uy; 00uy; n; 
        14uy; 00uy; 00uy; 
        00uy; 00uy; 00uy; 00uy; 0uy; 
        14uy; 00uy; 04uy;
        15uy; 00uy; 00uy;
        00uy; 00uy; 00uy; 00uy; 00uy;
        26uy; 
        11uy; 00uy; 04uy;
        15uy; 00uy; 04uy;
        08uy;
        00uy; 00uy; 00uy; 00uy; 01uy;
        15uy; 00uy; 00uy;
        16uy; 00uy; 00uy;
        17uy; 00uy; 02uy;
        17uy; 00uy; 01uy;
        05uy;
        14uy; 00uy; 00uy;
        15uy; 00uy; 04uy;
        02uy;
        14uy; 00uy; 04uy;
        10uy; 255uy; 209uy;
    ] 
```
