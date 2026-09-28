This is just a capture from the coRecursive slack space... I haven't edited it at all.


Annika  [Apr 27th at 8:49 AM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777294190791729)  

Does anyone have recommendations for resources to learn about the later parts of the compilation process, such as code generation and possibly type checking? Basically anything that comes after the AST has been generated.

35 replies

---

Alex Shroyer  [Apr 27th at 8:59 AM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777294761037129?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

[Essentials of Compilation](https://github.com/IUCompilerCourse/Essentials-of-Compilation) by my homeboys (I mean that literally, Jeremy's house is on one of my bike routes). (edited) 

GitHub

[GitHub - IUCompilerCourse/Essentials-of-Compilation: A book about compiling Racket and Python to x86-64 assembly](https://github.com/IUCompilerCourse/Essentials-of-Compilation)

A book about compiling Racket and Python to x86-64 assembly - IUCompilerCourse/Essentials-of-Compilation

[8:59](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777294796313549?thread_ts=1777294190.791729&cid=C06JQPGA8UU)

It's funny because I just saw someone has been [doing a livestream](https://www.youtube.com/playlist?list=PL_eVLc-qrivZyj1tGZg3DQeWLE2ThCTzp) working their way through this in Gleam.

YouTube

[Essentials of Compilation](https://www.youtube.com/playlist?list=PL_eVLc-qrivZyj1tGZg3DQeWLE2ThCTzp)

Share your videos with friends, family, and the world

[9:00](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777294835188389?thread_ts=1777294190.791729&cid=C06JQPGA8UU)

The book is almost exactly what you're looking for. The input language is a subset of Racket (a lisp) so they can skip right past lexing and parsing to get into the juicier parts.

[9:02](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777294937057019?thread_ts=1777294190.791729&cid=C06JQPGA8UU)

The course is organized so you start emitting x86 code almost immediately (for a very limited language) and then gradually add language features on top.

Jake  [Apr 27th at 9:28 AM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777296497353229?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

ooh! I’ll have to check it out.

Kevin Edey  [Apr 27th at 11:29 AM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777303791478319?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

Not exactly conventional types but Elixir is implementing set theoretic types.  
[https://www.youtube.com/watch?v=eBzoZWgZ334](https://www.youtube.com/watch?v=eBzoZWgZ334)I've heard some people in Python are working on this concept for a Python type system as well.

[](https://www.youtube.com/)[YouTube](https://www.youtube.com/)

[The Design Principles of the Elixir Type System - Guillaume Duboc | Code BEAM Europe 2023](https://www.youtube.com/watch?v=eBzoZWgZ334)

[](https://www.youtube.com/watch?v=eBzoZWgZ334)

Annika  [Apr 27th at 11:54 AM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777305261951319?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

Wow, there is even a course with videos!

Ionuț G. Stan  [Apr 27th at 12:42 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777308140784049?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

It's harder to find good resources about type systems. I think the only really good book is [Types & Programming Languages](https://www.amazon.co.uk/Types-Programming-Languages-MIT-Press/dp/0262162091/), but it feels very theoretical at times.I had more success with papers somehow. They may look daunting at first, but some of them are really self-contained — they might even advertise themselves as tutorials, e.g., [Bidirectional Typing Rules: A Tutorial](https://davidchristiansen.dk/tutorials/bidirectional.pdf).And if you're lucky, for some of them you may even find a nice talk about it on YouTube. For the paper above this is the talk: [https://www.youtube.com/watch?v=utyBNDj7s2w](https://www.youtube.com/watch?v=utyBNDj7s2w)

Ionuț G. Stan  [Apr 27th at 12:48 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777308519208829?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

And for a Haskell-like language there's this book from 1987: [The Implementation of Functional Programming Languages](https://simon.peytonjones.org/assets/pdfs/slpj-book-1987-searchable.pdf) (I've only read the chapter on pattern matching though).

Ionuț G. Stan  [Apr 27th at 12:53 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777308820685479?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

Maybe there's a niche for a more modern book on how to implement a statically-typed FP-oriented language. (edited) 

Jake  [Apr 27th at 1:31 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777311063284169?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

[https://sdiehl.github.io/typechecker-zoo/](https://sdiehl.github.io/typechecker-zoo/)

sdiehl.github.io

[Introduction - Typechecker Zoo](https://sdiehl.github.io/typechecker-zoo/)

Typecheckers built for fun

Annika  [Apr 27th at 1:31 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777311094589729?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

[@Ionuț G. Stan](https://corecursive.slack.com/team/U071TKJ3WR4) is that maybe because there is less consensus about how type systems should be implemented? I have started reading the book by SPJ before, but didn't get very far (even though I liked it)

Ionuț G. Stan  [Apr 27th at 1:36 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777311409788289?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

Maybe it's just easier to bikeshed about other aspects ![:slightly_smiling_face:](https://a.slack-edge.com/production-standard-emoji-assets/15.0/apple-medium/1f642@2x.png)Joking aside, I'm not sure about consensus. From what I've seen in my explorations, every feature from parser to code-generation can be done in multiple ways, each with its own trade-offs.Type systems get complicated really fast and they're also entirely optional, so it's an easy corner to cut I guess. (edited) 

Ionuț G. Stan  [Apr 27th at 1:41 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777311712173229?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

Damn... I totally missed this diss in the Typecheker Zoo series the previous time Jake shared it ![:astonished:](https://a.slack-edge.com/production-standard-emoji-assets/15.0/apple-medium/1f632@2x.png)

> Haskell, while historically important and technically very interesting, is an excellent guide for how _not_ to design a language.

Jake  [Apr 27th at 1:47 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777312070482999?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

oh snap, me too!

[1:49](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777312183324869?thread_ts=1777294190.791729&cid=C06JQPGA8UU)

oh man, that’s brutal. it’s true though and I’m a ![:haskell-logo:](https://emoji.slack-edge.com/TECHMCLH5/haskell-logo/bbb8f1c8dde6ae16.png) stan…

Annika  [Apr 27th at 2:12 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777313573008769?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

Ha, I will have to find that quote now. Curious about the reasoning behind that.

Jake  [Apr 27th at 2:18 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777313937418519?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

[https://sdiehl.github.io/typechecker-zoo/system-f-omega/language-design.html?the-haskell-hairball](https://sdiehl.github.io/typechecker-zoo/system-f-omega/language-design.html?the-haskell-hairball)

sdiehl.github.io

[Language Design - Typechecker Zoo](https://sdiehl.github.io/typechecker-zoo/system-f-omega/language-design.html?the-haskell-hairball)

Typecheckers built for fun

[2:19](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777313951154639?thread_ts=1777294190.791729&cid=C06JQPGA8UU)

beat you to it [@Ionuț G. Stan](https://corecursive.slack.com/team/U071TKJ3WR4) ![:grin:](https://a.slack-edge.com/production-standard-emoji-assets/15.0/apple-medium/1f601@2x.png)

Ionuț G. Stan  [Apr 27th at 2:20 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777314005897499?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

Indeed ![:smile:](https://a.slack-edge.com/production-standard-emoji-assets/15.0/apple-medium/1f604@2x.png) I was writing something apologizing for how I did something that I hate when I see it with others: quote some text without giving a proper source for it. (edited) 

Annika  [Apr 27th at 2:26 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777314401497399?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

Thanks both! Enjoying the read. While I don't fully agree, I can see how that would cause frustration if you are stuck with it in your day job.

Jake  [Apr 27th at 2:32 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777314765887519?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

Diehl is a Haskell legend and I respect his opinion, especially since I’ve experienced and agree with the critiques he mentioned. (edited) 

Cédric  [Apr 27th at 3:27 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777318028821629?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

[https://createlang.rs/intro.html](https://createlang.rs/intro.html)  covers everything including code generation (you can skip to that section for each generation of the language). It does a great job at demystifying LLVM and how trivial it is to generate code these days.

Jake  [Apr 27th at 3:35 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777318528455629?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

ooh this looks good ![:point_up:](https://a.slack-edge.com/production-standard-emoji-assets/15.0/apple-medium/261d-fe0f@2x.png)

Freemasen  [Apr 28th at 12:01 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777392070203059?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

I really enjoyed this course from Cornell: [https://www.cs.cornell.edu/courses/cs6120/2020fa/self-guided/](https://www.cs.cornell.edu/courses/cs6120/2020fa/self-guided/)

Freemasen  [Apr 28th at 12:09 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777392589555969?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

I recently asked an LLM based agent to write me a tutorial about building a type checker for the Lua programming language using the Salsa framework, it isn't perfect but the first few chapters have been a bit interesting[https://pages.freemasen.com/lola/lua-type-checker-tutorial/index.html](https://pages.freemasen.com/lola/lua-type-checker-tutorial/index.html)

pages.freemasen.com

[Introduction - Building a Gradual Type Checker for Lua with Salsa](https://pages.freemasen.com/lola/lua-type-checker-tutorial/index.html)

Learn Salsa, incremental computation, and gradual typing by building a Lua type checker

Cédric  [Apr 28th at 12:09 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777392591689299?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

C++, VIM, shell... now that's hardcore development ![:slightly_smiling_face:](https://a.slack-edge.com/production-standard-emoji-assets/15.0/apple-medium/1f642@2x.png)

Freemasen  [Apr 28th at 12:15 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777392951594229?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

The code for the Gradual Type Checker tutorial is available here: [https://git.freemasen.com/esme/lua-type-checker-tutorial](https://git.freemasen.com/esme/lua-type-checker-tutorial)

Forgejo

[lua-type-checker-tutorial](https://git.freemasen.com/esme/lua-type-checker-tutorial)

Esme review fork of lua-type-checker-tutorial

[https://git.freemasen.com/esme/lua-type-checker-tutorial](https://git.freemasen.com/esme/lua-type-checker-tutorial "lua-type-checker-tutorial")

[](https://git.freemasen.com/esme/lua-type-checker-tutorial)

Nikhil Thomas  [Apr 28th at 7:25 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777418700937619?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

Nora Sandler’s new-ish book covers this well too, you'll write x86 codegen after an IR pass

Freemasen  [Apr 29th at 2:57 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777489043458599?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

I assume this one? [https://nostarch.com/writing-c-compiler](https://nostarch.com/writing-c-compiler)

nostarch.com

[Writing a C Compiler](https://nostarch.com/writing-c-compiler)

A fun, hands-on guide to writing your own compiler for a real-world programming language.

Jun 25th, 2021

Jake  [Apr 29th at 4:11 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777493470283839?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

I need to bookmark all of these ![:star-struck:](https://a.slack-edge.com/production-standard-emoji-assets/15.0/apple-medium/1f929@2x.png)

Alex Shroyer  [Apr 29th at 4:15 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777493740128519?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

for real

Annika  [Apr 29th at 4:16 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777493804804559?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

Good point, doing it now

Nikhil Thomas  [Apr 29th at 4:18 PM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777493909474549?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

Yep that's the book! I like it a lot

Ionuț G. Stan  [Apr 30th at 2:06 AM](https://corecursive.slack.com/archives/C06JQPGA8UU/p1777529165894869?thread_ts=1777294190.791729&cid=C06JQPGA8UU)  

It's available as a series of blog posts too: [https://norasandler.com/archive/](https://norasandler.com/archive/) I presume the content in the book might be a little more polished, though.

norasandler.com

[Archive](https://norasandler.com/archive/)

it's a blog