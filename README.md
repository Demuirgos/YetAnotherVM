DummyLang sample : 
```
fun add(a,b) {
    a <- a + b;
    return a;
}
var condition = true;
var result = 0;
while(condition) {
    result <- add(result,1);
    if(result = 23) {
        condition <- false;
    }    
}
return result;
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
