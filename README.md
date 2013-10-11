NetVinylGLFW
====================
This is a small experiment application used to explore functional reactive game programming in Haskell using the Netwire, Vinyl, Vinyl-GL, 
Lens, Linear and GLFW-b packages. I is also did it to have something for show and tell at my functional programming users group.

**Disclaimer**: If you accidentally stumble upon this page do take note that I am still a novice Haskaller, and that I am capturing 
here my limited understanding of things, but hopefully you might find something valuable here like I have when I stumbled upon so 
many other resources. On that note here is a list of some resources (that I can remember) that helped me.
* [Postmodern Haskell and OpenGL: Introducing vinyl-gl](http://www.arcadianvisions.com/blog/?p=388)
* [Vinyl: Modern Records for Haskell](http://www.jonmsterling.com/posts/2013-04-06-vinyl-modern-records-for-haskell.html)
* [Haskell/Understanding arrows](http://en.wikibooks.org/wiki/Haskell/Understanding_arrows)
* [Haskell/Arrow tutorial](http://en.wikibooks.org/wiki/Haskell/Arrow_tutorial)
* [Learning Modern 3D Graphics Programming](http://www.arcsynthesis.org/gltut/)
* [OpenGL in Haskell: GLFW-b Boilerplate](http://www.tapdancinggoats.com/opengl-in-haskell-glfw-b-boilerplate.htm)
* [Modern OpenGL with Haskell](http://www.arcadianvisions.com/blog/?p=224)
* [Asteroids & Netwire](http://ocharles.org.uk/blog/posts/2013-08-18-asteroids-in-netwire.html)
* [Getting Started with Netwire and SDL](http://ocharles.org.uk/blog/posts/2013-08-01-getting-started-with-netwire-and-sdl.html)

Why
==================
*Why Haskell?* I wanted to learn functional programming because its different than my background (OOP, imperitive, C++). At first I picked
Haskell by accident but stuck to it because it was, still is, a bit scary and I like the air of "computer science formalism" it has about it.
Oh and I am getting the feeling if I ever even halfway master it I will become a ninja with safety goggles.

*Why game programming?* Well why not? It is I guess one of the more fun ways to explore a new domain and I sort of do something similar for
my day job. So for good or bad I know what kinds of concepts I would like to map to the functional space.

*Why OpenGL?* Haskell has support for it, and it follows more closely the Haskell community's tenet of portability than, lets say the other
choice.

*Why Vinyl and Vinyl-GL?* The road to Vinyl leads through Vinyl-GL, and to it from the fact that one does not use the fix function pipeline
any more. Even though it seems like that is more used and supported in the community. It comes down to using shaders and marshaling
a lot of data to OpenGL and this is a pain and error prone in any language or platform except if you build sensible abstractions. I was 
about to try and write my own (the blind leading the blind?) when I came across a guys blog where he had an example using Vinyl-GL and it 
was exactly what I wanted.

*Why GLFW-b?* It just installs, which for a non pure Haskell library on my OS can sometimes be a pain. But more than that it seems well
laid out minimalist and allows you to get a window with graphics up and running with minimal fuss.

*Why FRP and why Netwire and why arrows?* FRP is cool, at least it used to be until it lost some of its lustre but it appears to be
regaining some of its shine. Actually I was reading up on arrows and in particular a tutorial that was building up to an early version of
the Netwire package when they introduced the automaton arrow and locally evolving state. This sold me on the idea of learning Netwire and
arrows and FRP.

*Why Lens and Linear?* We just had a talk about lenses at our users group and Linear uses lenses and if you are doing graphics, even 2D,
you really need linear algebra package with nice support for low dimensionality vectors.

The theory of crumbs
=========================

A little FRP
-------------------------
As far as I have gathered functional reactive programming originated in the Haskell community in the late 1990s and was pioneered by people
like [Conal Elliot and Paul Hudak][]. More recently Conal Elliot brought out a paper on [push-pull functional reactive programming][] 
giving formal semantics for it as well as addressing implementation issues.

Functional reactive programming, and [reactive programming]() in general, aims to simplify the creation of reactive systems
like user interfaces, animations in real-time systems and games. Generally on would say what things are and how they related and the 
system makes sure everything updates correctly when something changes. In FRP you are provided with behaviours and events. Conceptually 
behaviours are continuous functions over time and events are discrete functions over time or streams of time value pairs. Additionally
you would be have a switching construct which allows you to change behaviours due to events. You would build your system by composing 
behaviours, events and switches.

```haskell
    Behaviour a :: T -> a
    Event a :: [(T,a)]
    Switch a :: Behaviour a -> Event (Behaviour a) -> Behaviour a
```

[reactive programming]: http://en.wikipedia.org/wiki/Reactive_programming
[Conal Elliot and Paul Hudak]: http://www.haskell.org/haskellwiki/Research_papers/Functional_reactive_programming
[push-pull functional reactive programming]: http://www.haskell.org/haskellwiki/Research_papers/Functional_reactive_programming

A smidgen of Arrows
-----------------------
[Arrows]() are an abstract view computation defined by [John Hughes][] which are similar to Monads but more general. *They are I guess*
*(but I really don't know what I am talking about) sort of categories of mappings between sets.* But the important thing is functions
are arrows and other arrows are like functions with super secret jet packs. An arrow is usually represented as **a b c** meaning 
arrow **a** which takes a value **b** and produces a value **c** and this all happening in the context defined by the arrow **a**

```
   -- b -> c == (->) b c == a b c where a == (->)
   --            ______                     ______ 
   --           |     |                     |     |
   --  input -> |  f  | -> output  ==  b -> |  a  | -> c
   --           |_____|                     |_____|
   --
   -- (a b c . a d b) == (a b c <<< a d b) == (a d b >>> a b c) == a d c
   --
   --     ___________________________
   --     |  ______   adc    ______ |
   --     |  |     |         |     ||
   -- d ->|  | adb | -> b -> | abc ||-> c
   --     |  |_____|         |_____||
   --     |_________________________|
   --    
```

Just like functions arrows can be composed and evaluated and there are arrow combinators which can be used to connect together networks
of these arrows. Haskell also provides you with arrow notation which is similar to do notation allowing you to define complicated
networks succinctly. You evaluate the arrow **(->) b c** basically by providing the argument **b** but other arrows will have special *run*
functions. 
[John Hughes]: http://www.haskell.org/arrows/
[Arrows]: http://en.wikibooks.org/wiki/Haskell/Understanding_arrows 















