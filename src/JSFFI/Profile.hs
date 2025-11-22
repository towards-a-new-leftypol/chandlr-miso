{-# LANGUAGE CPP #-}

module JSFFI.Profile
( sectionStart
, sectionEnd
, displayTotals
, bracket
) where

#if defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim (JSString(..), toJSString)
import Control.Monad.IO.Class (MonadIO, liftIO)

foreign import javascript unsafe "window.sectionStart($1)"
    sectionStart :: JSString -> IO ()

foreign import javascript unsafe "window.sectionEnd($1)"
    sectionEnd :: JSString -> IO ()

foreign import javascript unsafe "window.displayTotals($1)"
    displayTotals :: IO ()

bracket :: (MonadIO m) => String -> m a -> m a
bracket tag action = do
    liftIO $ sectionStart (toJSString tag)
    result <- action
    liftIO $ sectionEnd (toJSString tag)
    return result
#else
sectionStart :: a -> IO ()
sectionStart _ = return ()

sectionEnd :: a -> IO ()
sectionEnd = sectionStart

displayTotals :: IO ()
displayTotals = return ()

bracket :: b -> m a -> m a
bracket _ action = action
#endif
