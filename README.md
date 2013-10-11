NetVinylGLFW (Work in progress)
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
* [Haskell OpenGL animation done right: using closures and channels instead of IORef’s](http://dmytrish.wordpress.com/2013/01/12/haskell-opengl-animation-done-right/)

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

An example of a non normal function arrow could be the automaton arrow that is based on functions that produce an output and a new
evolved arrow which can be used in the next evaluation to produce a different result. Local state can be captured inside the automaton
arrow which may be updated and returned in the new arrow to have an effect.

```haskell
    newtype Auto b c = Auto (b -> (c, Auto b c))
    runAuto (Auto a) b = let (c, newAuto) = a b in (c, newAuto)
```
[John Hughes]: http://www.haskell.org/arrows/
[Arrows]: http://en.wikibooks.org/wiki/Haskell/Understanding_arrows 

A dash of Netwire
-----------------
[Netwire][] is an arrow library that can be used for FRP and other forms of locally stateful programming. The version I used was version 4
and that is the one currently on Hackage. It seems [Netwire 5][] is still in development. 

The core primitive in [Netwire][] is the wire which is an automaton arrow that evolves with time; at least thats how I see it. The semantics
of the [Netwire][] arrows are, I would say, more expressive than the standard arrow semantics in that the arrow can either produce a value or 
inhibit with a monoid. When a wire inhibits, itself and its dependant network will not produce a value but rather the result will be the
inhibition monoid. There are also primitives for merging sets of wires selecting the result based on which ones inhibit and which ones don't.

The wire type in Netwire is **Wire e m a b** with **Wire e m** defining the type of arrow with **e** being the type of the inhibition monoid
and **m** being the type of the Monad the wire inhabits. For your application or a subsets of wires your application you would usually 
fix **e** and **m** to specific types.

```haskell
    data Wire e m a b 
    type Event e m a = Wire e m a a
```

Behaviours are represented in Netwire as wires from **a** to **b** and events as wires from **a** to **a**. So the wires map obviously to
FRP behaviours but the mapping of input/output constrained wires to discrete FRP events involves a slight bit of subtlety. The continuous
signal is modulated into a discrete signal by the fact that a wire can either inhibit or produce (switch on or off). But wires are flexible
and any wire can inhibit, so why the type constraint on input/output values. It tells you that events should modulate the network they are
embedded in based on the signals that flow through them. To get the FRP sense of events as a streams of discrete time values you 
would modulate a value producing wire through an event wire. You could also have a value producing wire switching on and off but this is
not usually what you want really want to be doing. 

[Netwire 5][] actually recasts the event of of [Netwire 4][] as intervals and adds an explicit event concept. So [Netwire 5][] conceptually
has wires, intervals and events where [Netwire 4][] conceptually has wires and events.

[Netwire]: http://hackage.haskell.org/package/netwire-4.0.7/docs/Control-Wire.html
[Netwire 4]: http://hackage.haskell.org/package/netwire-4.0.7/docs/Control-Wire.html
[Netwire 5]: http://hub.darcs.net/ertes/netwire

And garnish with Vinyl
----------------------
[Vinyl][], what like in a record you play? Well yes. The vinyl library in short provides you constructs that you can use in place of the
default supported Haskell records. It basically allows you to construct custom product types as type lists using all kinds of extensions
for type level programming. 

You can define a **Field** as a combination of a type level string and another type. This is a distinct type. You can then define type
level lists of these fields to represent a record or **PlainRec**/**Rec** type. 

```Haskell
    Field :: "name" ::: String --- a distinct field type with symbol name "name"
    type LifeForm = ["name" ::: String, "age" ::: Int, "sleeping" ::: Bool] -- an example of a record using vinyl
```

[Vinyl-gl][] makes use of the fact that you tagged types with strings literals and can define arbitrary combinations of these to allow you
to easily define vertex buffers and shader inputs and then takes care of uploading those values to OpenGL and checking that the semantics on
OpenGL's side and your side matches up.

[Vinyl]: http://www.jonmsterling.com/posts/2013-04-06-vinyl-modern-records-for-haskell.html

The experiment
=========================

The game or not the game
-------------------------
That is the question and the answer is that I spent too much time playing with scaffolding rather than defining any creative or sensible
notion of a game. So currently you have a bordered area with a red and a green box bouncing around in it and a paddle at the bottom which
you can move around and that can shoot little bullets. Everything renders to screen collides with one another and the paddle responds to
keyboard input. It is very basic but at least something to experiment with.

The structure of things
-------------------------
I wanted to keep the game loop pure and not just because the application is in Haskell but so that it would be possible to evolve it into
a parallel game loop sometime in the future. This had an impact on the Monad chosen for the wires. I did not go for the Identity Monad, which
would certainly have made the wires pure, but rather the effect free Reader Monad. This allowed me to pass the environment to my wires 
without but still not introducing dependency on effects. The environment holds all the available resources and also the results of the GLFW
callbacks. How do I draw stuff then, well the main wire produces a stream of lists of IO actions.

```Haskell
    type WireM' = WireM ReadAppM        -- The type of wires in the app
    type EventM' a = EventM ReadAppM a  -- The type of events in the app
    mainW :: WireM' a [IO()]            -- The main wire takes nothing but produces IO actions
```

I wanted to make it easy to register and update GLFW callbacks as well as the other resources which would be available and I wanted to be
flexible with which resource are available. I decided to use vinyl records to represent all the data in my Reader Monad and use type classes
to iterate over them and initialize them.

```haskell
    -- Our application record consisting of a set of call back data, a function to update
    -- the call back data, a set IO actions that may be used to draw things to screen and 
    -- the initial width and height of the screen.
    type App = PlainRec [Callbacks, CallbacksUpdater, Renderables, InitWidthHeight]

    -- The Renderables field contains all the available renderables (things that can be drawn)
    -- the elements of the record are instantiated through CreateRenderable class
    type Renderables = "Renderables" ::: PlainRec '[DrawUnitBox]
    renderables :: Renderables
    renderables = Field

    -- Class defines function used to create a renderable in Renderables
    class CreateRenderable a where createRenderable :: Shaders -> IO (PlainRec '[a])

    -- Class used to iterate over the renderables in Renderables
    class CreateRenderables a where createRenderables :: Shaders -> IO a

    -- CreateRenderables for empty rec is empty
    instance CreateRenderables (PlainRec '[]) where createRenderables _ = return $ RNil

    -- CreateRenderables for rec is CreateRenderable for the head prefixed to CrreateRenderables for the tail
    instance (CreateRenderable f, CreateRenderables (PlainRec rs)) => CreateRenderables (PlainRec (f ': rs)) where
            createRenderables ss = (V.<+>) <$> createRenderable ss <*> createRenderables ss
`````

The flow of the application is then as follows:
1. Construct environment loading resources registering callbacks etc.
2. Step the main wire running it inside the reader monad.
3. Perform all the actions returned by the main wire (draw stuff).
4. Update the environment due to GLFW callbacks.
5. Repeat from step 2 until the main wire inhibits.


Handling callbacks
--------------------------
Dealing with callbacks is painful in Haskell even when having to hook into a nice minimalist library like GLFW-b. So I wanted to make it
easy to register callbacks with GLFW and to store the results in my Reader Monad so that they would be available for my wires to generate
wire events. On one of the blogs I read I got introduced to the idea of queueing up the callback results using STM and then processing them
again later. I evolved that idea into this.

```
    -- Set a GLFW call back using a lens to store the accumulated values in some type
    setCallBackGLFW :: forall c s m. (MonadIO m, CurryGLFW c)   
                    => (Maybe c -> IO())                        -- The specific call back registration function
                    -> Lens' s [TplGLFW c]                      -- The lens allowing us to store the result
                    -> m (s -> m s)                             -- The resultant updater function transfering callback results

    -- Example of call back registration
    -- The type which stores the results of the window size call back for this instant
    type CbWindowSize = "WindowSizeCallback" ::: [(GLFW.Window, Int, Int)]
    cbWindowSize :: CbWindowSize
    cbWindowSize = Field 
    instance RegisterCallback CbWindowSize where 
            registerCallback _ win = setCallBackGLFW (GLFW.setWindowSizeCallback win) (rLens callbacks . rLens cbWindowSize)

    -- Given a lens produce a wire which provides the environment value at the current instant
    readW :: (Lens' App b) -> WireM' a b
```
So you define a Vinyl type for your callback and an instance for **RegisterCallback** which will register it and make the results
available in your application Reader Monad which you can access using the **readW**.






