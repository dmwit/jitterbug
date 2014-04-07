import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.IORef
import Data.List
import Graphics.UI.Gtk
import Text.Printf

data GameState
	= Initializing [TimeStamp]
	| Playing TimeStamp TimeStamp
	deriving (Eq, Ord, Show, Read)

main = do
	initGUI

	r  <- newIORef (Initializing [])
	da <- drawingAreaNew
	on  da keyPressEvent $ keyPressed r
	on  da exposeEvent   $ exposed    r
	set da [widgetCanFocus := True]

	w <- windowNew
	on  w objectDestroy mainQuit
	set w [containerChild := da]
	widgetShowAll w

	forkIO $ forever (postGUIAsync (widgetQueueDraw da) >> threadDelay 30000)
	mainGUI

mean ts = sum ts `div` genericLength ts
diff ts = zipWith (-) ts (tail ts)

errorMargin start freq t = 2 * signedMargin / freq' where
	rawMargin = (t - start) `mod` freq
	freq' = fromIntegral freq
	signedMargin = fromIntegral rawMargin - if rawMargin > freq `div` 2 then freq' else 0

keyPressed r = tryEvent $ do
	"space" <- eventKeyName
	t <- eventTime
	liftIO $ do
		s <- readIORef r
		case s of
			Initializing ts | length ts >= 5 -> writeIORef r (Playing (last ts) ((mean . diff) (t:ts)))
			                | otherwise      -> writeIORef r (Initializing (t:ts))
			Playing start freq -> printf "%5d\n" (round (errorMargin start freq t * 1000) :: Int)

exposed r = tryEvent $ do
	return ()
