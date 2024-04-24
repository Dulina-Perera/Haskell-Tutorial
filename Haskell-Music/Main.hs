import qualified Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Foldable
import Data.Text.Array (run)
import System.Process
import Text.Printf
import Language.Haskell.TH.PprLib (semi)

type Hz = Float
type Pulse = Float
type Samples = Float
type Seconds = Float

outFilePath :: FilePath
outFilePath = "wave.bin"

audioRate :: Samples
audioRate = 48000

pitchStd :: Hz
pitchStd = 440.0

volume :: Float
volume = 0.5

freq :: Seconds -> Hz -> [Pulse]
freq duration frequency = 
    map (((* volume) . sin) . (* step)) [0..audioRate*duration]
        where step = (frequency * 2 * pi) / audioRate

semiTone :: Int -> Hz
semiTone n = pitchStd * ((2 ** (1.0 / 12.0)) ** fromIntegral n)

wave :: [Pulse]
wave = concat [
        freq duration (semiTone 0),
        freq duration (semiTone 2),
        freq duration (semiTone 3),
        freq duration (semiTone 5),
        freq duration (semiTone 7),
        freq duration (semiTone 8),
        freq duration (semiTone 10),
        freq duration (semiTone 12)
    ] where duration = 0.5

save :: FilePath -> [Float] -> IO ()
save path xs = BL.writeFile path $ BB.toLazyByteString $ foldMap BB.floatLE xs

play :: IO ProcessHandle
play = do
    save outFilePath wave

    runCommand $ printf "ffplay -ar %.3f -autoexit -f f32le -showmode 1 -t 8.0 %s" audioRate outFilePath

main :: IO ()
main = do
    processHandle <- play
    _ <- waitForProcess processHandle

    putStrLn "Audio playback completed."
