{-# LANGUAGE PackageImports #-}

module Android (
	runAndroid,
	importAndroid,
	clone,
	method,
	primStr
) where

import Control.Prototype
import RunWindow
import "monads-tf" Control.Monad.Reader
import "monads-tf" Control.Monad.State
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Control.Monad.Tools
import Data.Char

type X11IO = StateT X11Env IO 

data X11Env = X11Env {
	display		:: Display,
	window		:: Window,
	delWin		:: Atom,
	exposeAction	:: Prot X11IO ()
 }

runAndroid :: Prot X11IO a -> IO ()
runAndroid act = withWindow $ \dpy win dWin -> do
	let initX11Env = X11Env dpy win dWin $ return ()
	flip evalStateT initX11Env $ fmap fst $ flip runProt initProtEnv $ do
		act
		exAct <- lift $ gets exposeAction
		x11Env <- lift get
		doWhile_ $ liftIO $ allocaXEvent $ \e -> flip evalStateT x11Env $
			fmap fst $ flip runProt initProtEnv $ do
			liftIO $ nextEvent dpy e
			ev <- liftIO $ getEvent e
			case ev of
				ClientMessageEvent { } ->
					return $ getClientMessageAtom ev /= dWin
				ExposeEvent { } -> exAct >> return True
				KeyEvent { } -> do
					let kc = ev_keycode ev
					ks <- liftIO $ keycodeToKeysym dpy kc 0
					let ch = chr $ fromEnum ks
					return $ ch /= 'q'
				ButtonEvent { } -> do
					liftIO $ print ev
					return True
				_ -> return True

importAndroid :: Prot X11IO ( Object, Member, Member )
importAndroid = package "android" $ do
	textWidget	<- clone object
	setText		<- makeMember "setText"
	setContentView	<- makeMember "setContentView"
	setMethod textWidget setText setTextFun
	setMethod textWidget setContentView setContentViewFun
	return ( textWidget, setText, setContentView )

setTextFun :: Method X11IO
setTextFun obj [ str ] = package "android" $ do
	text <- makeMember "text"
	printMember text
	setMember obj text str
	return [ ]

setContentViewFun :: Method X11IO
setContentViewFun obj [ ] = package "android" $ do
	X11Env dpy win dWin _ <- lift get
	text <- makeMember "text"
	txt <- member obj text
	lift $ put $ X11Env dpy win dWin $ liftIO $
		drawStringUtf8 dpy win 12 12 30 $ fromPrimStr txt
	return [ ]
