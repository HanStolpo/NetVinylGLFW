{-# LANGUAGE RankNTypes #-}         -- required for explicit for all
{-# LANGUAGE FlexibleInstances #-}  -- needed by CurryGLFW
{-# LANGUAGE TypeFamilies #-}       -- needed by TplGLFW

module Graphics.UI.GLFW.Vinyl.Callbacks  ( setCallBackGLFW
                                         ) where 
import Control.Lens
import Control.Concurrent.STM
import Control.Monad.IO.Class

-- Type family use to determine the equivalant tuple type for a GLFW call back
-- requires TypeFamilies
type family TplGLFW f
type instance TplGLFW (a -> IO ()) = a
type instance TplGLFW (a -> b -> IO ()) = (a, b)
type instance TplGLFW (a -> b -> c -> IO ()) = (a, b, c)
type instance TplGLFW (a -> b -> c -> d -> IO ()) = (a, b, c, d)
type instance TplGLFW (a -> b -> c -> d -> e -> IO ()) = (a, b, c, d, e)
type instance TplGLFW (a -> b -> c -> d -> e -> f -> IO ()) = (a, b, c, d, e, f)
type instance TplGLFW (a -> b -> c -> d -> e -> f -> g -> IO ()) = (a, b, c, d, e, f, g)

-- Type class used convert an uncurried callback function to a curried call back function suitable for GLFW
-- requires FlexibleInstances
class CurryGLFW f where curryGLFW :: (TplGLFW f -> IO ()) -> f
instance CurryGLFW (a -> IO ()) where curryGLFW ft = \a -> ft a
instance CurryGLFW (a -> b -> IO ()) where curryGLFW ft = \a b -> ft (a,b)
instance CurryGLFW (a -> b -> c -> IO ()) where curryGLFW ft = \a b c -> ft (a,b,c)
instance CurryGLFW (a -> b -> c -> d -> IO ()) where curryGLFW ft = \a b c d -> ft (a,b,c,d)
instance CurryGLFW (a -> b -> c -> d -> e -> IO ()) where curryGLFW ft = \a b c d e -> ft (a,b,c,d,e)
instance CurryGLFW (a -> b -> c -> d -> e -> f -> IO ()) where curryGLFW ft = \a b c d e f -> ft (a,b,c,d,e,f)
instance CurryGLFW (a -> b -> c -> d -> e -> f -> g -> IO ()) where curryGLFW ft = \a b c d e f g -> ft (a,b,c,d,e,f,g)

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

-- Set a GLFW call back using a lens to store the accumulated values in some type
setCallBackGLFW :: forall c s m. (MonadIO m, CurryGLFW c)   
                => (Maybe c -> IO())                        -- The specific call back registration function
                -> Lens' s [TplGLFW c]                      -- The lens allowing us to store the result
                -> m (s -> m s)                             -- The resultant updater function transfering callback results
setCallBackGLFW f l = do
    -- Create the uncurried call back an the update function which are glued together
    (cb, upd) <- glueCbGLFW l
    -- set the call back
    liftIO $ f (Just (curryGLFW cb))
    return upd
