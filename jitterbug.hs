{-# LANGUAGE LambdaCase, NoMonomorphismRestriction #-}
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
	--             keypresses
	= Initializing [UTCTime]
	--        start   frequency       past error margins
	| Playing UTCTime NominalDiffTime [NominalDiffTime]
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
	forkIO $ forever (postGUIAsync (modifyIORef r garbageCollect) >> threadDelay 100000000)
	mainGUI

mean ts = sum ts / genericLength ts
diff ts = zipWith diffUTCTime ts (tail ts)

errorMargin start freq t = signedMargin / freq where
	rawMargin = diffUTCTime t start `mod'` freq
	signedMargin = rawMargin - if rawMargin > freq / 2 then freq else 0

keyPressed r = tryEvent $ do
	t <- liftIO getCurrentTime
	"space" <- eventKeyName
	liftIO $ do
		s <- readIORef r
		case s of
			Initializing ts | length ts >= 5 -> writeIORef r (Playing (last ts) ((mean . diff) (t:ts)) [])
			                | otherwise      -> writeIORef r (Initializing (t:ts))
			Playing start freq ms -> do
				let m = errorMargin start freq t
				writeIORef r (Playing start freq (m:ms))
				printf "%5d\n" (round (errorMargin start freq t * 2000) :: Int)

exposed r da = join . liftIO $ readIORef r >>= \case
	Playing start freq ms -> do
		t <- getCurrentTime
		w <- fromIntegral <$> widgetGetAllocatedWidth  da
		h <- fromIntegral <$> widgetGetAllocatedHeight da
		let m = errorMargin start freq t
		return $ do
			scale w h
			setLineWidth 0.01

			-- target mark
			setSourceRGB 1 0 0
			lineForMargin 0 1 0
			stroke

			-- metronome marks
			setSourceRGB 0 0 0
			mapM_ (lineForMargin 0 1) [m-10, m-9 .. m+10]
			moveTo 0 0 >> lineTo 1 0
			moveTo 0 1 >> lineTo 1 1
			stroke

			-- past keypress marks
			setSourceRGB 0 0.5 0
			forM_ (zip [1..numHistoryPoints] ms) $ \(i,m) -> do
				lineForMargin (1-i/numHistoryPoints) (1-(i-1)/numHistoryPoints) m
			stroke
	_ -> return (return ())

lineForMargin x1 x2 m = let y = (atan (realToFrac m*10)*2/pi + 1)/2 in moveTo x1 y >> lineTo x2 y

numHistoryPoints = 10

garbageCollect (Playing start freq ms) = Playing start freq (take numHistoryPoints ms)
garbageCollect s = s
