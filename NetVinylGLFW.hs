{-# LANGUAGE DataKinds #-}          -- required by vinyl
{-# LANGUAGE TypeOperators #-}      -- required by vinyl
{-# LANGUAGE RankNTypes #-}         -- required for explicit for all
{-# LANGUAGE Arrows #-}             -- required for arrow syntax
{-# LANGUAGE FlexibleInstances #-}  -- needed by CurryGLFW
{-# LANGUAGE FlexibleContexts #-}   -- needed by vinyl record instance definition Default
{-# LANGUAGE TypeFamilies #-}       -- needed by TplGLFW
{-# LANGUAGE ConstraintKinds #-}    -- needed by type definitions in constraints
{-# LANGUAGE ScopedTypeVariables #-}-- needed by type definitions in constraints
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


import Control.Monad
import Control.Monad.Reader
import Control.Lens hiding (at,cons)
--import qualified Control.Lens as L
import Control.Applicative
import qualified Control.Wire as W
import Control.Wire hiding (perform,when,(<+>),cons)
import qualified Graphics.UI.GLFW as GLFW
import Data.Monoid
import Data.Maybe
import qualified Data.Vinyl as V
import Data.Vinyl hiding (perform, (<+>), at,cons)
import Data.Vinyl.Reflect
import Data.Default
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU
import Graphics.VinylGL
import Foreign.C.Types
import Foreign.Storable
import Linear hiding (trace)
import Safe hiding (at)
import System.Random ()
import Prelude hiding ((.), id)     -- more general version imported from Control.Category
--import Debug.Trace
import GHC.Conc
-- custom code
import Data.Vinyl.Instances.Default 
import Graphics.UI.GLFW.Vinyl.Callbacks



--------------------------------------------------------------------------------
type InitWidthHeight = "InitWidthHeight" ::: (Int, Int)
initWidthHeight :: InitWidthHeight
initWidthHeight = Field

-- Our application record consisting of a set of call back data, a function to update
-- the call back data, a set IO actions that may be used to draw things to screen and 
-- the initial width and height of the screen.
type App = PlainRec [Callbacks, CallbacksUpdater, Renderables, InitWidthHeight]
initApp :: GLFW.Window -> IO App
initApp win = let 
        cmb = liftM2 (V.<+>) 
        in return (callbacks =: unwrapDefault def) 
        `cmb` ((callbacksUpdater =:) . AppUpdater <$> registerCallbacks (undefined :: FieldT Callbacks) win)
        `cmb` ((renderables =:) <$> (loadShaders >>= createRenderables))
        `cmb` return (initWidthHeight =: (600, 600))


-- The application environment monad
type ReadAppM = Reader App 
--------------------------------------------------------------------------------
                                

--------------------------------------------------------------------------------
type family FieldT a
type instance FieldT (sy ::: t) = t
type instance FieldT (sy ::: [t]) = [t] 

printGlErrors ::  IO ()
printGlErrors = GL.get GL.errors >>= mapM_ print
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Wrapper for call back update function to get around circular reference
newtype AppUpdater = AppUpdater {getAppUpdater :: App -> IO App}
instance Default AppUpdater where def = AppUpdater (\a -> return a)

-- Field containing the function to update the call backs
type CallbacksUpdater = "CallbackUpdater" ::: AppUpdater
callbacksUpdater :: CallbacksUpdater
callbacksUpdater = Field
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Typedef to collect constraints for types which may be viable vertexes
type ViableVertex t = (HasFieldNames t, HasFieldSizes t, HasFieldDims t, HasFieldGLTypes t, Storable t) 

-- Typedef to collect constraints for types which may be viable shader values
type UniformFields a = (HasFieldNames a, HasFieldGLTypes a, SetUniformFields a)

-- Take a shader and some geometry and return an action which takes a record of shader values and renders it
makeRenderable :: (ViableVertex (PlainRec ves), UniformFields (PlainRec svs))
               => GLU.ShaderProgram                 -- The shader program
               -> ([PlainRec ves], [GLU.Word32])    -- The geometry consisting of vertexes and indexes
               -> IO (PlainRec svs -> IO())         -- The function taking shader values and rendering 
makeRenderable prg (vbuff, ibuff) = do
    let numIs = length ibuff
    when (numIs `mod` 3 /= 0) (fail "makeRenderable - number of indexes should be multiple of 3")
    vao <- GLU.makeVAO $ do vbuff' <- (bufferVertices vbuff)
                            printGlErrors
                            enableVertices' prg vbuff' 
                            printGlErrors
                            bindVertices vbuff' 
                            printGlErrors
                            ibuff' <- GLU.bufferIndices ibuff 
                            GL.bindBuffer GL.ElementArrayBuffer GL.$= Just ibuff'       
                            printGlErrors
    let draw svs = do   GL.currentProgram GL.$= Just (GLU.program prg)
                        setAllUniforms prg svs
                        GLU.withVAO vao (GLU.drawIndexedTris (fromIntegral $ numIs `div` 3))
                        printGlErrors
                        GL.currentProgram GL.$= Nothing
    return draw 
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Record holding the list of available shaders
type Shaders = PlainRec '[Simple2D]

-- Class defines function used to create a renderable in Renderables
class LoadShader a where loadShader :: IO (PlainRec '[a])

-- Class used to iterate over the renderables in Renderables
class LoadShaders a where loadShaders :: IO a

-- LoadShaders for empty rec is empty
instance LoadShaders (PlainRec '[]) where loadShaders = return $ RNil

-- LoadShaders for rec is LoadShader for the head prefixed to CrreateRenderables for the tail
instance (LoadShader f, LoadShaders (PlainRec rs)) => LoadShaders (PlainRec (f ': rs)) where
        loadShaders = (V.<+>) <$> loadShader <*> loadShaders 
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
type Simple2D = "Simple2D" ::: GLU.ShaderProgram
simple2D :: Simple2D
simple2D = Field
instance LoadShader Simple2D where 
    loadShader = (simple2D =:) 
              <$> GLU.simpleShaderProgramWith  ("Simple2D.vert") ("Simple2D.frag") (\_-> printGlErrors)

type MWorldViewProj2D = "mWorldViewProj2D" ::: M44 CFloat
mWorldViewProj2D :: MWorldViewProj2D
mWorldViewProj2D = Field

type VColour = "vColour" ::: V4 CFloat
vColour :: VColour 
vColour= Field
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- vertex data
type VPosition2D = "vPosition2D" ::: V2 CFloat
vPosition2D :: VPosition2D
vPosition2D = Field


makeBox  :: V2 CFloat                                    -- The extents 
         -> ([PlainRec '[VPosition2D]], [GLU.Word32]) -- The geometry's vertexes and indexes
makeBox (V2 extX extY) = (ps, is)
    where
        ps :: [PlainRec '[VPosition2D]]
        ps = map (vPosition2D =:) $
            mconcat [V2 <$> [x] <*> [y] | y <- [-extY, extY],   x <- [-extX, extX]]
        {-  
        - 1-----3 
        - |     | 
        - |     | 
        - 0-----2 
        -   CCW  
        -}
        is = [0,2,1,2,3,1]
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
type DrawUnitBox = "DrawUnitBox" ::: (PlainRec [MWorldViewProj2D, VColour] -> IO ())
drawUnitBox :: DrawUnitBox
drawUnitBox = Field
instance CreateRenderable DrawUnitBox where
        createRenderable ss = (drawUnitBox =:) <$> (makeRenderable (ss ^. rLens simple2D) $ makeBox (V2 1 1))


--------------------------------------------------------------------------------
mkSrt2D :: V2 CFloat   -- Scale
        -> CFloat      -- Orientation
        -> V2 CFloat   -- Position
        -> M44 CFloat
mkSrt2D s r t = eye4 
                    & _x._x .~ (s^._x) * cos r 
                    & _x._y .~ (s^._x) * sin r
                    & _y._x .~ (s^._y) * sin r * (-1)
                    & _y._y .~ (s^._y) * cos r
                    & _z._z .~ 1
                    & _w._x .~ (t^._x)
                    & _w._y .~ (t^._y)

-- mkScale2D :: V2 CFloat -> M44 CFloat
-- mkScale2D s = mkSrt2D s 0 (V2 0 0)

mkRotate2D :: CFloat -> M44 CFloat
mkRotate2D r = mkSrt2D (V2 1 1) r (V2 0 0)

mkTranslate2D :: V2 CFloat -> M44 CFloat
mkTranslate2D  = mkSrt2D (V2 1 1) 0




--------------------------------------------------------------------------------
-- The synonym for all the callbacks used in our app
type Callbacks = "Callbacks" ::: PlainRec [CbWindowSize, CbWindowClose, CbKey]
callbacks :: Callbacks
callbacks = Field

-- Class defines function used to create a renderable in Renderables
class RegisterCallback a where registerCallback :: a -> GLFW.Window ->  IO (App -> IO App)

-- Class used to iterate over the renderables in Renderables
class RegisterCallbacks a where registerCallbacks :: a -> GLFW.Window  -> IO (App -> IO App)

-- RegisterCallbacks for empty rec is empty
instance RegisterCallbacks (PlainRec '[]) where registerCallbacks _ _ = return $ (\a -> return a) 

-- RegisterCallbacks for rec is RegisterCallback for the head prefixed to CrreateRenderables for the tail
instance (RegisterCallback f, RegisterCallbacks (PlainRec rs)) => RegisterCallbacks (PlainRec (f ': rs)) where
        registerCallbacks _ w = liftM2 (>=>)
                                (registerCallback (undefined :: f) w)
                                (registerCallbacks (undefined :: PlainRec rs) w)
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
type WireM' = WireM ReadAppM
type EventM' a = EventM ReadAppM a

-- Given a lens produce a wire which provides the environment value at the current instant
readW :: (Lens' App b) -> WireM' a b
readW l = W.perform . pure (view l)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
data Thing = BouncingBox Box 
           | Border Box 
           | Paddle Box
           | Bullet Box
        deriving Show

collideThings :: Thing -> Thing -> [ContactPoint]
collideThings a b = collide (bnds a) (bnds b)
    where
    bnds (Border c) = c
    bnds (BouncingBox c) = c
    bnds (Paddle c) = c
    bnds (Bullet c) = c

type ContactPointPos = V2 CFloat    
type ContactPointNorm = V2 CFloat   
type ContactPoint = (ContactPointPos, ContactPointNorm)
class Collidable a b where
    collide  :: a -> b -> [ContactPoint] -- contact position and normal with normal from b to a

data Box = Box Centre Extent deriving Show
type Centre = V2 CFloat
type Extent = V2 CFloat
instance Collidable Box Box where
    collide (Box c1 e1) (Box c2 e2) =
        let corners c e = [c + V2 (sx * e^._x) (sy * e^._y) | sx <- [-1, 1, 1,-1,-1] 
                                                            | sy <- [ 1, 1,-1,-1, 1]] 
            cs1 = corners c1 e1
            cs2 = corners c2 e2
            n s2'' e2'' = normalize ((e2'' ^+^ s2'') ^* 0.5 ^-^ c2)
            intersect (s1', e1') (s2', e2') = (, n s2' e2') <$> intersectSegSeg s1' e1' s2' e2'
            is' = (intersect <$> (zip cs1 (drop 1 cs1)) <*> (zip cs2 (drop 1 cs2)))
            is = filter isJust is'
        in map fromJust is

intersectSegSeg :: V2 CFloat -- Start1
                -> V2 CFloat -- End1
                -> V2 CFloat -- Start2
                -> V2 CFloat -- End2
                -> Maybe (V2 CFloat) -- intersection point
intersectSegSeg s1 e1 s2 e2 = do
    let v1 = e1 ^-^ s1
        v2 = e2 ^-^ s2
        u  = s1 ^-^ s2
        den = v1^._x * v2^._y - v1^._y * v2^._x
    guard . not . nearZero $ den                        -- check not parallel
    let r = (u^._y * v2^._x - u^._x * v2^._y) / den
        s = (u^._y * v1^._x - u^._x * v1^._y) / den
    if  (r >= 0 && r <= 1 && s >= 0 && s <= 1) -- check not intesecting
        then return  (s1 + v1 ^* r)            -- intersection point
        else empty
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
newtype ThingWire = ThingWire {  unThingWire :: WireM' [Thing] ( IO(), (Thing, [ThingWire]) )  } 
instance Show ThingWire where
    show _ = "ThingWire"

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
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- The type which stores the results of the window size call back for this instant
type CbWindowSize = "WindowSizeCallback" ::: [(GLFW.Window, Int, Int)]
cbWindowSize :: CbWindowSize
cbWindowSize = Field 
instance RegisterCallback CbWindowSize where 
        registerCallback _ win = setCallBackGLFW (GLFW.setWindowSizeCallback win) (rLens callbacks . rLens cbWindowSize)

-- Wire produces the window size (Width, Height) at the instant that cbWindowSize fires
resizeWindowW :: WireM' a (Int, Int)
resizeWindowW = readW (rLens callbacks . rLens cbWindowSize) >>> require (not . null) >>> arr (\((_,w,h):_) -> (w, h))

resizeViewport :: WireM' a (IO ())
resizeViewport = switch initW (inhibit mempty) 
    where
        initW = once >>> (readW $ rLens initWidthHeight) >>^ (\i -> hold i resizeWindowW >>^ setViewPort)
        setViewPort (w,h) = GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral  w) (fromIntegral  h))

-- Wire supplies the persepective matrix to be used in shaders based on the last received window size
-- given an initial window size
projectionMatrix2D' :: WireM' a (M44 CFloat)
projectionMatrix2D' = switch initW (inhibit mempty)
    where
        initW = once >>> (readW $ rLens initWidthHeight) >>^ (\i -> hold i resizeWindowW >>^ mkM)
        mkM (w,h) = eye4
                & _x . _x .~ xsq * cor
                & _y . _y .~ ysq * cor
                & _z . _z .~ 1
                & _w . _w .~ 1
                where
                    h' = realToFrac h
                    w' = realToFrac w
                    diagPixSz = sqrt $ w' * w' + h' * h'
                    xsq = diagPixSz / w'
                    ysq = diagPixSz / h'
                    cor = 1 / max xsq ysq
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
type Position = V2 CFloat
type Velocity = V2 CFloat
type Collisions = [ContactPoint]

-- Create integrating wire based for any functor a member of Additive with an element type which 
-- is an instance of Fractional (doubles, floats etc)
integralV :: (Additive f, Fractional a)
          => f a                        -- The constant of integration (starting point)
          -> WireM' (f a) (f a)         -- Wire that produces the integral of its input signal
integralV c = proc v -> do
    rec
        v' <- delay zero . arr (uncurry  (^+^)) . first (arr (uncurry (^*)) . (id &&& realToFrac <$> dtime))  -< (v,v')
    returnA -< c ^+^ v'

-- Given a velocity and a list of collisions reflect the velocity based on the collision normals
reflectVel :: WireM' (Velocity, Collisions) Velocity
reflectVel = arr $ \(v, cs) ->
        let (vr, v') = foldl reflect (V2 0 0,v) . map (^._2) $ cs
            reflect (r, o) n = let r' =  max (negated n `dot` o) 0 *^ n in (r + r', o + r')
        in   v' + vr                   -- new velocity

collisionsFiltered :: (Thing -> Bool) ->  WireM' (Thing, [Thing]) Collisions
collisionsFiltered f = delay [] . arr (collideThings *** filter f >>> uncurry concatMap)

collisionsAll :: WireM' (Thing, [Thing]) Collisions
collisionsAll = collisionsFiltered . const $ True

-- Wire takes a position and produces an action that draws a box on screen
drawBoxW :: Extent     -- size of box
         -> V4 CFloat  -- colour
         -- Rendered box at specified centre
         ->  WireM' Position (IO ())
drawBoxW e col = (readW $ rLens renderables . rLens drawUnitBox) &&& shaderVals  >>^ uncurry ($)
        where
            shaderVals =  arr (mkSrt2D e 0) &&& projectionMatrix2D' >>^ uncurry (!*!) >>^ (mWorldViewProj2D=:)
                          &&& pure (vColour =: col) >>^ uncurry (V.<+>)

-- A box that will bounce around the screen colliding against other boxes
bouncingBoxW' :: Extent     -- size of box
              -> V4 CFloat  -- colour
              -> Position   -- initial position 
              -> Velocity   -- initial velocity
              -- Rendered box that updates position due to velocity and velocity due to collisions
              ->  WireM' [ContactPoint] (Thing, IO ())
bouncingBoxW' e col ip iv = proc cs -> do
    rec
        v'  <- delay iv . reflectVel -< (v', cs) 
        p   <- integralV ip -< v'
        t   <- BouncingBox ^<< flip Box e ^<< id -< p
        d   <- drawBoxW e col -< p
    returnA -< (t, d)

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
paddleW :: WireM' [Thing] (IO (), (Thing, [ThingWire]))
paddleW = proc ts -> do
        rec
            v   <- vel -< ()
            cs  <- collisionsFiltered borderOnly -< (t,ts)
            p   <- integralV (V2 0 (-0.8)) . reflectVel -< (v, cs)
            d   <- drawBoxW extent (V4 0 0 1 1) -< p
            t   <- Paddle ^<< flip Box extent ^<< id  -< p
            bs  <- fireBullet -< (p,v)
        returnA -< (d, (t,bs))
    where
        extent = V2 0.25 0.1

        vel :: WireM' a Velocity
        vel =  (keyHeld GLFW.Key'Left) . pure (V2 (-1.5) 0.0)
           <|> (keyHeld GLFW.Key'Right) . pure (V2  1.5 0.0)
           <|> pure (V2 0 0)

        fireBullet :: WireM' (Position, Velocity) [ThingWire]
        fireBullet =  (keyE GLFW.Key'Space) . arr ((:[]) . ThingWire . uncurry bulletW) . arr (_2._y .~ 1) . arr (_1._y .~ -0.75)
                  <|> pure []
        
        borderOnly (Border _) = True
        borderOnly _  = False

bulletW :: Position -> Velocity -> WireM' [Thing] (IO(), (Thing, [ThingWire]))
bulletW ip iv = proc ts -> do
    rec
       p    <- integralV ip . pure iv &&& id delay [] . require null . collisionsFiltered notPaddle -< (t, ts)
       t    <- Bullet ^<< flip Box e ^<< id -< fst p
       d    <- drawBoxW e (V4 0 1 1 1)      -< fst p
    returnA -< (d, (t, []))
    where
        e = V2 0.01 0.01
        notPaddle (Paddle _) = False
        notPaddle _ = True

        

bouncinRedBoxTW :: WireM' [Thing] ( IO(), (Thing, [ThingWire]) )
bouncinRedBoxTW = proc ts -> do
        rec
            (t, a) <- bouncingBoxW' (V2 0.05 0.05) (V4 1 0 0 1) (V2 (ips!!0) (ips!!1)) (V2 (ivs!!0) (ivs!!1)) -< cols
            cols <- collisionsAll -< (t, ts)
        returnA -< (a, (t, []))
        where
            ivs = randomRs (-1,1) (mkStdGen 1)
            ips = randomRs (-0.5, 0.5)(mkStdGen 9)

bouncinGreenBoxTW :: WireM' [Thing] ( IO(), (Thing, [ThingWire]) )
bouncinGreenBoxTW = proc ts -> do
        rec
            (t, a) <- bouncingBoxW' (V2 0.05 0.05) (V4 0 1 0 1) (V2 (ips!!0) (ips!!1)) (V2 (ivs!!0) (ivs!!1)) -< cols
            cols <- delay [] . (arr (uncurry concatMap)) -< (collideThings t, ts)
        returnA -< (a, (t, []))
        where
            ivs = randomRs (-1,1) (mkStdGen 11)
            ips = randomRs (-0.5, 0.5)(mkStdGen 19)

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
makeBorderWire :: V2 CFloat             -- pos
            -> CFloat                   -- orientation
            -> WireM' a (Thing, IO())
makeBorderWire t r = thing &&& draw 
        where
            ext' = V2 0.1 1
            mW = mkSrt2D ext' r t
            c = V2 (mW^._w._x)(mW^._w._y)
            ext = let mE =  abs <$> mkTranslate2D ext' !*! mkRotate2D r in V2 (mE^._w._x) (mE^._w._y)
            thing = pure $ Border (Box c ext)
            draw = (readW $ rLens renderables . rLens drawUnitBox) &&& shaderVals  >>^ (\(f, a) -> f a)
            shaderVals =  pure mW &&& projectionMatrix2D' >>^ uncurry (!*!) >>^ (mWorldViewProj2D=:)
                          &&& pure (vColour =: V4 0 0 1 1) >>^ uncurry (V.<+>)


borderLefW' :: WireM' a (Thing, IO ())
borderLefW' = makeBorderWire (V2 (-0.9) 0) 0

borderRightW' :: WireM' a (Thing, IO ())
borderRightW' = makeBorderWire (V2 0.9 0) (0)

borderTopW' :: WireM' a (Thing, IO ())
borderTopW' = makeBorderWire (V2 0 (-0.9)) (pi/2)

borderBottomW' :: WireM' a (Thing, IO ())
borderBottomW' = makeBorderWire (V2 0 0.9) (pi/2)
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
thingWires :: WireM' a [IO()]
thingWires = stepThingWires . (once . pure iws --> pure [])
    where iws = map ThingWire   [ bouncinRedBoxTW
                                , bouncinGreenBoxTW
                                , borderLefW' >>^ fixOut
                                , borderRightW' >>^ fixOut
                                , borderTopW' >>^ fixOut
                                , borderBottomW' >>^ fixOut
                                , paddleW
                                ]
          fixOut = (^._2) &&& ((^._1) &&& arr (const []) >>^ uncurry (,)) >>^ uncurry (,)
--------------------------------------------------------------------------------



-- Type for storing the results of the key call back for this instant
type CbKey = "KeyCallback" ::: [(GLFW.Window, GLFW.Key, Int, GLFW.KeyState, GLFW.ModifierKeys)]
cbKey :: CbKey
cbKey = Field
instance RegisterCallback CbKey where 
    registerCallback _ win = setCallBackGLFW (GLFW.setKeyCallback win) (rLens callbacks . rLens cbKey)

-- Create a wire event the produces only when the specified key has been pressed
keyE :: GLFW.Key -> EventM' a
keyE k =  passOver $ require pressed . readW (rLens callbacks . rLens cbKey)
    where
        pressed = not . null . filter (\t -> (t^._2 == k && t^._4 == GLFW.KeyState'Pressed))

passOver :: WireM' a b -> EventM' a
passOver w = fst <$> (id &&& w)


keyHeld :: GLFW.Key -> EventM' a
keyHeld k = switch keyDown (inhibit $ mempty)
    where
        readKeys = readW (rLens callbacks . rLens cbKey)
        keyDown :: WireM' a (EventM' a)
        keyDown = (pure keyUp) . require (not . null . filter (\t -> t^._2 == k && t^._4 == GLFW.KeyState'Pressed)) . readKeys
        keyUp :: EventM' a
        keyUp =  passOver $ W.until (not . null . filter (\t -> t^._2 == k && t^._4 == GLFW.KeyState'Released)) . readKeys

-- Type for sotring the results of the window close callback for this instant
type CbWindowClose = "WindowCloseCallback" ::: [GLFW.Window]
cbWindowClose :: CbWindowClose
cbWindowClose = Field 
instance RegisterCallback CbWindowClose where 
    registerCallback _ win = setCallBackGLFW (GLFW.setWindowCloseCallback win) (rLens callbacks . rLens cbWindowClose)

-- Wire event that produces only at the instant that cbWindoClose has fired
windowCloseE :: EventM' a
windowCloseE = fst <$> (id &&& require (not . null) . readW (rLens callbacks . rLens cbWindowClose))

-- Wire event to indicate that the application should close, it produces only when 'esc' is pressed 
-- or windowCloseE produces
exitE :: EventM' a
exitE =  keyE GLFW.Key'Escape <|> windowCloseE

appendActionW :: WireM' ([IO ()], IO ()) [IO ()]
appendActionW = arr (\(xs, x) -> x:xs)

mainW :: WireM' a [IO()]
mainW =  notE exitE . effects
    where
        effects = thingWires &&& resizeViewport >>> appendActionW

main :: IO ()
main = do
    -- create our window and run our code in it
    withWindow 600 600 "NetVinylGl" $ \win -> do
        a <- initApp win
        t <- fromJustNote "failed getting GLFW time" <$> GLFW.getTime       -- get initial time
        GL.clearColor GL.$= GL.Color4 0 0 0 0
        GL.depthFunc GL.$= Just GL.Less
        mainLoop win a mainW t                                              -- call our main game loop
    where
        -- loop continually evolving the main wire until it inhibits
        mainLoop win a w t = do
            t' <- fromJustNote "failed getting GLFW time" <$> GLFW.getTime  -- new time for delta time
            let dt =  t' - t
            if dt >= 1.0 / 60.0 
                then do
                    GLFW.swapBuffers win
                    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
                    GLFW.pollEvents
                    a' <- a ^. rLens callbacksUpdater . to getAppUpdater $ a        -- update our call backs
                    let (r, w') = runReader (stepWire w dt a') a'               -- run our 'pure' wire
                    {-let (r, w') = runReader (stepWire w (1.0/5000.0) a') a'               -- run our 'pure' wire-}
                    case r of
                        -- we inhibited so print last exception and terminate
                        Left e -> fromMaybe (return ()) $ print `fmap` getLast e    
                        -- we did not ihibit so sequence the resultant monadic actions then recurse
                        Right ms -> sequence ms >> mainLoop win a' w' t'
                else yield >> mainLoop win a w t
                

        -- basic call back used to receive errors and print them during construction and cleanup
        simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

        -- in the monad construct the window and run the given function cleaning up when done
        withWindow width height title f = do
                GLFW.setErrorCallback $ Just simpleErrorCallback
                r <- GLFW.init
                when r $ do
                    m <- GLFW.createWindow width height title Nothing Nothing
                    case m of
                        (Just win) -> do
                            GLFW.makeContextCurrent m
                            _ <- f win
                            GLFW.setErrorCallback $ Just simpleErrorCallback
                            GLFW.destroyWindow win
                        Nothing -> putStrLn "Failed to create the GLFW window"
                    GLFW.terminate
