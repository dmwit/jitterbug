{-# LANGUAGE LambdaCase #-}
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.IORef
import Data.Fixed
import Data.List
import Data.Time.Clock
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Cairo
import Text.Printf

data GameState
	= Initializing [UTCTime]
	| Playing UTCTime NominalDiffTime
	deriving (Eq, Ord)

main = do
	initGUI

	r  <- newIORef (Initializing [])
	da <- drawingAreaNew
	on  da keyPressEvent $ keyPressed r
	on  da draw          $ exposed    r da
	set da [widgetCanFocus := True]

	w <- windowNew
	on  w objectDestroy mainQuit
	set w [containerChild := da]
	widgetShowAll w

	forkIO $ forever (postGUIAsync (widgetQueueDraw da) >> threadDelay 5000)
	mainGUI

mean ts = sum ts / genericLength ts
diff ts = zipWith diffUTCTime ts (tail ts)

errorMargin start freq t = 2 * signedMargin / freq where
	rawMargin = diffUTCTime t start `mod'` freq
	signedMargin = rawMargin - if rawMargin > freq / 2 then freq else 0

keyPressed r = tryEvent $ do
	"space" <- eventKeyName
	liftIO $ do
		t <- getCurrentTime
		s <- readIORef r
		case s of
			Initializing ts | length ts >= 5 -> writeIORef r (Playing (last ts) ((mean . diff) (t:ts)))
			                | otherwise      -> writeIORef r (Initializing (t:ts))
			Playing start freq -> printf "%5d\n" (round (errorMargin start freq t * 1000) :: Int)

exposed r da = join . liftIO $ readIORef r >>= \case
	Playing start freq -> do
		t <- getCurrentTime
		w <- fromIntegral <$> widgetGetAllocatedWidth  da
		h <- fromIntegral <$> widgetGetAllocatedHeight da
		let m = realToFrac (errorMargin start freq t)
		    x = (atan (m*10)*2/pi*w + w)/2
		return $ do
			setSourceRGB 1 0 0
			moveTo (w/2) 0
			lineTo (w/2) h
			stroke
			setSourceRGB 0 0 0
			moveTo x 0
			lineTo x h
			stroke
	_ -> return (return ())
