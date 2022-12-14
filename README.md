# brainfuck-scala

A Brainfuck "compiler" written in Scala, along with a "CPU" to run the instructions of the compiled program.

The standard Brainfuck directives are accepted, and I added the # directive to allow the inclusion of comments in the code. Anything following a '#' is ignored, up to either end of line or end of file. 

## Build

Build by running ./build.sh

## Run

```java -jar ./target/scala-3.2.0/brainfuck-assembly-1.0.jar <Brainfuck source file name> <ram size> <list flag> <debug flag>```

## Example

Compile and run the Brainfuck program found in file "e.bf", using 100000 bytes of RAM, listing the program's 
instructions ("true") and disabling debug ("false"). This program computes e.

```
java -jar ./target/scala-3.2.0/brainfuck-assembly-1.0.jar e.bf 100000 true false

2.718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427427466391932003059921817413596629043572900334295260595630738132328627943490763233829880753195251019011573834187930702154089149934884167509244761460668082264800168477411853742345442437107539077744992069551702761838606261331384583000752044933826560297606737113200709328709127443747047230696977209310141692836819025515108657463772111252389784425056953696770785449969967946864454905987931636889230098793127736178215424999229576351482208269895193668033182528869398496465105820939239829488793320362509443117301238197068416140397019837679320683282376464804295311802328782509819 [...]
```

## License

[MIT](https://choosealicense.com/licenses/mit/)
