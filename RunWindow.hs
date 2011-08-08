module RunWindow (
	runWindow,
	withWindow,
	drawStringUtf8,
	getClientMessageAtom
) where

import Graphics.X11 (
  Display, Window, Atom, Position, createGC,
  defaultScreenOfDisplay, defaultVisual, defaultColormap,
  openDisplay, defaultScreen,
  rootWindow, createSimpleWindow, mapWindow,
  whitePixel, blackPixel,
  selectInput, allocaXEvent, nextEvent,
  exposureMask, keyPressMask, buttonPressMask, buttonReleaseMask,
  setWMProtocols, internAtom                   )
import Graphics.X11.Xlib.Extras (
  getEvent, Event(ClientMessageEvent), ev_data )
import Graphics.X11.Xft
import Data.Bits                ( (.|.)        )
import Control.Monad.Tools      ( doUntil_, doWhile_     )
import Data.Convertible         ( convert      )

runWindow :: ( Display -> Window -> Atom -> Event -> IO Bool ) -> IO ()
runWindow act = do
  dpy     <- openDisplay ""
  delWin  <- internAtom dpy "WM_DELETE_WINDOW" True
  let scr   = defaultScreen dpy
      black = blackPixel dpy scr
      white = whitePixel dpy scr
  rootWin <- rootWindow dpy scr
  win     <- createSimpleWindow dpy rootWin 0 0 100 100 1 black white
  setWMProtocols dpy win [ delWin ]
  selectInput dpy win $ exposureMask .|. keyPressMask .|. buttonPressMask .|. buttonReleaseMask
  mapWindow dpy win
  doWhile_ $
    allocaXEvent $ \e -> do
      nextEvent dpy e
      ev <- getEvent e
      act dpy win delWin ev
  where
  isClientMessageEvent (ClientMessageEvent {}) = True
  isClientMessageEvent _                       = False
--  getClientMessageAtom = convert . head . ev_data

withWindow :: ( Display -> Window -> Atom -> IO () ) -> IO ()
withWindow act = do
  dpy     <- openDisplay ""
  delWin  <- internAtom dpy "WM_DELETE_WINDOW" True
  let scr   = defaultScreen dpy
      black = blackPixel dpy scr
      white = whitePixel dpy scr
  rootWin <- rootWindow dpy scr
  win     <- createSimpleWindow dpy rootWin 0 0 100 100 1 black white
  setWMProtocols dpy win [ delWin ]
  selectInput dpy win $ exposureMask .|. keyPressMask .|. buttonPressMask .|. buttonReleaseMask
  mapWindow dpy win
  act dpy win delWin
  where
  isClientMessageEvent (ClientMessageEvent {}) = True
  isClientMessageEvent _                       = False

drawStringUtf8Base ::
	Bool -> Integer -> Display -> Window -> String -> String -> Position -> Position ->
	String -> IO ()
drawStringUtf8Base erase size dpy win font color x y str = do
	gc <- createGC dpy win
	let	scr		= defaultScreen dpy
		scrN		= defaultScreenOfDisplay dpy
		visual		= defaultVisual dpy scr
		colormap	= defaultColormap dpy scr
	xftDraw <- xftDrawCreate dpy win visual colormap
	xftFont <- xftFontOpen dpy scrN font
	if erase
		then withXftColorName dpy visual colormap color $ \clr ->
			xftDrawRect xftDraw clr x ( y - fromInteger size + 2 )
				( size * toInteger ( length str ) ) size
		else withXftColorName dpy visual colormap color $ \clr ->
			xftDrawString xftDraw clr xftFont x y str

fontName :: String
fontName = "Kochi Gothic"

drawStringUtf8 ::
	Display -> Window -> Integer -> Position -> Position -> String -> IO ()
drawStringUtf8 dpy win size =
	drawStringUtf8Base False size dpy win fontStr "black"
	where
	fontStr = fontName ++ "-" ++ show size ++ ":style=Regular"

getClientMessageAtom :: Event -> Atom
getClientMessageAtom = convert . head . ev_data
