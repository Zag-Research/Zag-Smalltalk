In Pharo, `zoomFactorSettingsOn:` settings: 1 1.12 1.4 1.7 2 2.5 3 3.5 4 4.5 5 

What about Lazy-Smalltalk
- mutable objects need reification queue
https://futureofcoding.org/

Debugging with lldb or gdb:
```sh
zig build
# Assuming your exe name is `zag`
lldb zig-out/bin/zag
# Or to use gdb
gdb zig-out/bin/zag
```
See [this blog](https://ziggit.dev/t/debugging-zig-with-a-debugger/7160) which [references this one](https://ziggit.dev/t/zig-debugging-with-lldb/3931)

[Store AST as SQLite](https://git.sr.ht/~robheghan/glogg/tree/main/item/src/schema.js)

[Detailed Primer on Data Statistics](https://lakens.github.io/statistical_inferences/)

[Hash Function Prospector](https://github.com/skeeto/hash-prospector?tab=readme-ov-file)

[Big Idea Initiative](https://www.bigideainitiative.org/)

## Zig <--> Rust interface
[Calling rust from zig from rust from zig](https://www.youtube.com/watch?v=QQGRd_r12fw) watched first hour
[Wiring Together the Rust and Zig Build Systems](https://www.youtube.com/watch?v=zRM_vOxbAx8)

[Great pictures for slides](https://simonoya.com/drawings/)

## SICP Lectures
[Where it all starts](https://akovantsev.github.io/corpus/sicp#) click on the timeline

## Scaling from the ground up
[NAND to Tetris](https://www.nand2tetris.org/)
Ben Eater’s video series [gates to CPU](https://www.youtube.com/playlist?list=PLowKtXNTBypGqImE405J2565dvjafglHU) [microprocessor to BASIC](https://www.youtube.com/playlist?list=PLowKtXNTBypFbtuVMUVXNR0z1mu7dp7eH)

## Trinket
[Trinket](https://github.com/trinketapp/trinket-oss) is a browser-based IDE that supports Python, Java, and more.

## NotebookLM
[Describes how to use NptebookLM to learn](https://pub.towardsai.net/the-notebooklm-workflow-that-changed-how-i-learn-any-technology-373f430a17e5)

## Logic diagrams
[(A AND (NOT B)) OR C](https://www.101computing.net/logic-gates/?title=%28+A+AND+%28+NOT+B+%29+%29+OR+C)
[Circuit Verse](https://circuitverse.org/)
[Multimedia Logic:](https://sourceforge.net/projects/multimedialogic/)

I don't have a good app for drawing the gates but here are some word problems I use:
1) Create a logic circuit that will light a seat belt warning lamp if a car’s engine is on and the driver’s seat belt is not plugged in.  
2) The alarm in your car will sound if you touch the vehicle or you push the alarm button on the key fob.  
3) This circuit will have two inputs. Each input will be a single digit binary number. The output should indicate whether the two inputted numbers are equal.  
4) A hall light is controlled by two light switches, one at each end, the light can be switched on or off by either switch.  
5) Create a logic circuit that opens an apartment building’s garage door if a car is present on a driveway sensor when a post-mounted key switch is activated. Or if the correct remote control signal is received. Or whenever a pushbutton inside the parking garage is pressed.  
6) This circuit will input a three digit binary number. (Use one input for each digit e.g. A is the first digit, B the second, and C is the third). The output should indicate whether or not the three digit number is less than or equal to 2. (so only 000, 001, 010 and the light should come on)

## Using LLMs
[Karpathy's 3-layer method: Spec, verifier, environment - codifying your understanding](https://www.youtube.com/watch?v=7zZy1QTvokM)
