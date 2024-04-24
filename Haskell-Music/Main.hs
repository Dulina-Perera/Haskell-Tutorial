import qualified Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Foldable
import Data.Text.Array (run)
import System.Process
import Text.Printf

type Hz = Float
type Pulse = Float
type Samples = Float
type Seconds = Float

outFilePath :: FilePath
outFilePath = "wave.bin"

audioRate :: Samples
audioRate = 48000

volume :: Float
volume = 0.5

freq :: Seconds -> Hz -> [Pulse]
freq duration frequency = 
    map (((* volume) . sin) . (* step)) [0..audioRate*duration]
        where step = (frequency * 2 * pi) / audioRate

wave :: [Pulse]
wave = concat [freq 1.0 130.82, freq 1.0 146.83, freq 1.0 164.81, freq 1.0 174.81, freq 1.0 196, freq 1.0 220, freq 1.0 246.94]

save :: FilePath -> [Float] -> IO ()
save path xs = BL.writeFile path $ BB.toLazyByteString $ foldMap BB.floatLE xs

play :: IO ProcessHandle
play = do
    save outFilePath wave

    runCommand $ printf "ffplay -ar %.3f -autoexit -f f32le -showmode 1 -t 7.0 %s" audioRate outFilePath

main :: IO ()
main = do
    processHandle <- play
    _ <- waitForProcess processHandle

    putStrLn "Audio playback completed."
