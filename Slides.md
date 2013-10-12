% Haskell Reactive Game Programming with Netwire, Vinyl and OpenGL
% Handré Stolp
% 14 October 2013

Introduction
========================================

Why
-------------------------------
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

```haskell
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

```haskell
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
without introducing dependency on effects. The environment holds all the available resources and also the results of the GLFW
callbacks. How do I draw stuff then, well the main wire produces a stream of lists of IO actions.

```haskell
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
1. Construct the environment loading resources and registering callbacks etc.
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

```haskell
-- Set a GLFW call back using a lens to store the accumulated values in some type
setCallBackGLFW :: forall c s m. (MonadIO m, CurryGLFW c)   
                => (Maybe c -> IO())                        -- The specific call back registration function
                -> Lens' s [TplGLFW c]                      -- The lens allowing us to store the result
                -> m (s -> m s)                             -- The resultant updater function transfering callback results
setCallBackGLFW f l = do
    -- Create the uncurried call back and the update function which are glued together
    (cb, upd) <- glueCbGLFW l
    -- set the call back
    liftIO $ f (Just (curryGLFW cb))
    return upd

-- Take a lens form some type to a list of tuples and return two functions
-- the one function adds values to the list as an IO action
-- the other function empties the list and modifies the type using the lens 
glueCbGLFW :: MonadIO m => Lens' s [t] -> m (t -> IO (), s -> m s)
glueCbGLFW l = do
        -- an STM TVar used communicate between the two functions
        var <- liftIO $ newTVarIO ([] :: [t])
        let -- The call back appends new tuple values to the list
            cb t = atomically . flip modifyTVar' (t:) $ var
            -- The updater set the value referenced by the lens and clears the list in the TVar
            upd s = (liftIO $ atomically . flip swapTVar [] $ var) >>= (\ts -> return $ set l ts s)
        return (cb, upd)

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

Handling resources
--------------------------------
The only resources that I had were shaders and vertex buffers which I combined into a single resource as a renderable which is
a function that takes values to be passed to the shader and renders it to screen. In a serious system you would actually want to batch
your dispatching to OpenGL based on the shaders and vertex data but that will have to be the result of future experiments.

I probably unnecessarily overcomplicated the way I loaded the shaders and made the renderables. The set of available shaders I represented
by a vinyl type list where I used a type class to iterate over them and load a shader per "shader type", but I just had one shader. The
list of shaders got passed into the function to construct the list of renderables which would allow me to combine different vertex buffers
with different vertex shaders to give me renderables, but I only had one renderable. 

```haskell
-- Take a shader and some geometry and return an action which takes a record of shader values and renders it
makeRenderable :: (ViableVertex (PlainRec ves), UniformFields (PlainRec svs))
               => GLU.ShaderProgram                 -- The shader program
               -> ([PlainRec ves], [GLU.Word32])    -- The geometry consisting of vertexes and indexes
               -> IO (PlainRec svs -> IO())         -- The function taking shader values and rendering 

-- loading a shader
type Simple2D = "Simple2D" ::: GLU.ShaderProgram
simple2D :: Simple2D
simple2D = Field
instance LoadShader Simple2D where 
    loadShader = (simple2D =:) 
              <$> GLU.simpleShaderProgramWith  ("Simple2D.vert") ("Simple2D.frag") (\_-> printGlErrors)

-- Shader value for
type MWorldViewProj2D = "mWorldViewProj2D" ::: M44 CFloat -- Associated with mWorldViewProj2D uniform shader value
mWorldViewProj2D :: MWorldViewProj2D
mWorldViewProj2D = Field

-- create the renderable using the shader and a utility function to make a box geometry
type DrawUnitBox = "DrawUnitBox" ::: (PlainRec [MWorldViewProj2D, VColour] -> IO ())
drawUnitBox :: DrawUnitBox
drawUnitBox = Field
instance CreateRenderable DrawUnitBox where
        createRenderable ss = (drawUnitBox =:) <$> (makeRenderable (ss ^. rLens simple2D) $ makeBox (V2 1 1))

-- vertex data
type VPosition2D = "vPosition2D" ::: V2 CFloat      -- Associated with vPosition2D vertex element in the shader
vPosition2D :: VPosition2D
vPosition2D = Field
-- function creates vertex data for a box 
makeBox  :: V2 CFloat                                    -- The extents 
         -> ([PlainRec '[VPosition2D]], [GLU.Word32]) -- The geometry's vertexes and indexes


-- Take a shader and some geometry and return an action which takes a record of shader values and renders it
makeRenderable :: (ViableVertex (PlainRec ves), UniformFields (PlainRec svs))
               => GLU.ShaderProgram                 -- The shader program
               -> ([PlainRec ves], [GLU.Word32])    -- The geometry consisting of vertexes and indexes
               -> IO (PlainRec svs -> IO())         -- The function taking shader values and rendering 
```

The vertex data is represented as a list of vinyl records of storable types where the name tags of the fields are the same as the names
of the vertex inputs in the shader and the types are convertible to the shader types. Vinyl-gl takes care of working out how to upload 
the data to OpenGL and will give you an error if anything does no match up. The same is true for the shader values that will be uploaded
to the shader, they are represented as a vinyl record.

Dealing with a dynamic set of wires
------------------------------------
Netwire allows you to switch wires due to events but this only allows you swap out a sub network. Granted the subnetwork could be as complex
as you like and depend on the switching node's input, but I thought I needed more. This is probably due to a lack of experience with Netwire.

I had top level wires that represented the objects in my world and as input to them I wanted to pass in all the other objects so that they
could react to each other. I also wanted new objects to be able to join the world or leave it and for the network to "rewire" itself. 

I couldn't be bothered with proper names so my top level type was the sum type **Thing** (a thing can be one of many things). The top level
wire **WireM' [Thing] ( IO(), (Thing, [ThingWire])** took a list of other things and produced an action, a new thing for itself
and a list of possible new things. So if a top level wire produced more things they get added to the set of all things and any wire that
inhibits is removed.

```haskell
-- The top level sum type
data Thing = BouncingBox Box | Border Box | Paddle Box | Bullet Box deriving Show

-- Wrap the top level wires wich take the list of other thins and produces an action a thing and a list of possible new things.
newtype ThingWire = ThingWire {  unThingWire :: WireM' [Thing] ( IO(), (Thing, [ThingWire]) )  } 

stepThingWires :: WireM' [ThingWire] [IO()]
stepThingWires = mkStateM ([],[]) stepWs
    where
        stepWs :: Time
               -> (  [ThingWire], ( [(Thing, ThingWire)], [ThingWire] )  )
               -> ReadAppM (  Either LastException [IO()], ( [(Thing, ThingWire)], [ThingWire] )  )

        stepWs dt (ews, (tws, iws)) = do
                atwsO <- connect [] tws
                atwsNE <- map fromJust . filter isJust <$> sequence (map (stepW tws) ews)
                atwsNI <- map fromJust . filter isJust <$> sequence (map (stepW tws) iws)
                case atwsO ++ atwsNE ++ atwsNI of
                    -- nothing so inhibit
                    [] -> return (Left mempty, ([],[]))
                    -- something so return actions
                    rs -> return ( Right . map (^._1) $ rs
                    -- and state for next update
                                 , (map ((^._2) &&& (^._3) >>^ uncurry (,)) &&& concatMap (^._4) >>^ uncurry (,)) rs
                                 )
            where
                connect :: [(Thing, ThingWire)] -> [(Thing, ThingWire)] -> ReadAppM [(IO(), Thing, ThingWire, [ThingWire])]
                connect _ [] = return []
                connect ls (c@(_,w):rs) = do
                        s <- (stepW (ls ++ rs) w) 
                        r <- (connect (c:ls) rs)
                        case s of
                            Nothing -> return  r
                            Just a -> return (a : r)

                stepW :: [(Thing, ThingWire)] -> ThingWire -> ReadAppM (Maybe (IO(), Thing, ThingWire, [ThingWire]))
                stepW tws' w  = do 
                    let ts = map fst tws'
                    (r, w') <- stepWire (unThingWire w) dt ts
                    case r of
                        Left _ -> return $ Nothing
                        Right (a, (t, nws)) -> return $ Just (a, t, ThingWire w', nws)
```

Events
--------------------------
To generate events I created wires which queried the environment and then either inhibited or acted as the identity wire passing its input
through to its output. This was not the first way I tried to approach it. First I had the event take any value and then either inhibit or
produce the value. This does not compose because you cannot embed them in other networks. I think the key is to realize that events in 
[Netwire 4][] are the same as intervals in [Netwire 5][] and that they should modulate the interval of production of the signals flowing
through them.

```haskell
-- Create a wire event the produces only when the specified key has been pressed
keyDownE :: GLFW.Key -> EventM' a
keyDownE k =  passOver $ require pressed . readW (rLens callbacks . rLens cbKey)
    where
        pressed = not . null . filter (\t -> (t^._2 == k && t^._4 == GLFW.KeyState'Pressed))

-- take some wire turning it into an identiy wire but retaining the argument wires ihibition properties
passOver :: WireM' a b -> EventM' a
passOver w = fst <$> (id &&& w)
```

The other thing that got me a bit was generating an event which depends on a previous event and for that I used the **switch** construct.
So when the key is pressed it produces an event wire which produces as long as the key is not up and initially we start with an inhibiting
wire.

```haskell
keyHeld :: GLFW.Key -> EventM' a
keyHeld k = switch (pure ( untilKeyUpE k) . keyDownE k) (inhibit $ mempty)
```

Sometimes you would want to switch your behaviour based on a choice of events and for this you use **<|>** since wires are Alternative 
Applicative Functors.

```haskell
vel =  (keyHeld GLFW.Key'Left) . pure (V2 (-1.5) 0.0)   -- if key left produce negative x velocity
    <|> (keyHeld GLFW.Key'Right) . pure (V2  1.5 0.0)   -- else if key right produce positive x velocity
    <|> pure (V2 0 0)                                   -- else produce zero velocity
```

Behaviours
-------------------------
Your behaviours are networks of wires modifying the signals flowing through them. You create more complex networks and behaviours by
composing simpler ones. There are many ways you can compose the wires. You can compose them using the arrow composition operators, you
can chain them using function composition, you can compose using Applicative and Alternative and you can even combine them using arithmetic
operators. There is also special arrow syntax which is especially handy when wanting to use local feedback or recursion.

```haskell
integralV :: (Additive f, Fractional a)
          => f a                        -- The constant of integration (starting point)
          -> WireM' (f a) (f a)         -- Wire that produces the integral of its input signal
integralV c = proc v -> do
    rec
        v' <- delay zero . arr (uncurry  (^+^)) . first (arr (uncurry (^*)) . (id &&& realToFrac <$> dtime))  -< (v,v')
    returnA -< c ^+^ v'
```

The **rec** key word is required when there is a recursive dependency between the wires. Do note that in general you will need to use a 
**delay** some where in your dependency chain (i.e. use the result one sample back). This can also be used to capture local state as in
the example above (**v'**). You could also make use of **mkState** and **mkStateM** to create a custom wire where you explicitly define
and handle the state, I was doing this in the beginning but feel implicitly capturing it is better. In general I found it is better to 
keep your wires short and to the point. Here is an example of my most complex wire.

```haskell
paddleW :: WireM' [Thing] (IO (), (Thing, [ThingWire]))
paddleW = proc ts -> do
        rec
            v   <- vel -< ()                                        -- we get our velocity from key held events
            cs  <- collisionsFiltered borderOnly -< (t,ts)          -- we collide only with border things
            -- we integrate our velocity to get position and reflect velocity when colliding
            p   <- integralV (V2 0 (-0.8)) . reflectVel -< (v, cs)  
            d   <- drawBoxW extent (V4 0 0 1 1) -< p                -- we draw a box at paddle position
            t   <- Paddle ^<< flip Box extent ^<< id  -< p          -- we let the rest of the things know where we are
            -- we generate new bullet wire by pressing spacebar using our position and our x-velocity
            bs  <- fireBullet -< (p,v)                              
        returnA -< (d, (t,bs))                                      -- (OurDrawAction, (UpdatedUs, NewBullets))
    where
        extent = V2 0.25 0.1

        vel :: WireM' a Velocity
        vel =  (keyHeld GLFW.Key'Left) . pure (V2 (-1.5) 0.0)
           <|> (keyHeld GLFW.Key'Right) . pure (V2  1.5 0.0)
           <|> pure (V2 0 0)

        fireBullet :: WireM' (Position, Velocity) [ThingWire]
        fireBullet =  (keyDownE GLFW.Key'Space) . arr ((:[]) . ThingWire . uncurry bulletW) . arr (_2._y .~ 1) . arr (_1._y .~ -0.75)
                  <|> pure []
        
        borderOnly (Border _) = True
        borderOnly _  = False

