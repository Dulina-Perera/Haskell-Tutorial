import qualified Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Foldable
import Data.List
import Data.Text.Array (run)
import System.Process
import Text.Printf
import Language.Haskell.TH.PprLib (semi)
import Text.XHtml (rev)

type Beats = Float
type Hz = Float
type Pulse = Float
type Samples = Float
type Seconds = Float

outFilePath :: FilePath
outFilePath = "wave.bin"

audioRate :: Samples
audioRate = 48000

bpm :: Beats
bpm = 120.0

pitchStd :: Hz
pitchStd = 440.0

volume :: Float
volume = 0.5

freq :: Beats -> Hz -> [Pulse]
freq beats frequency =
    map (* volume) $ zipWith3 (\x y z -> x * y * z) attack output release
        where
            step = (frequency * 2 * pi) / audioRate
            attack = map (min 1.0) [0, 0.001 ..] :: [Pulse]
            release = reverse $ take (length output) attack
            output = map (sin . (* step)) [0 .. audioRate * beats * (60.0 / bpm)] :: [Pulse]

semiTone :: Int -> Hz
semiTone n = pitchStd * ((2 ** (1.0 / 12.0)) ** fromIntegral n)

note :: Beats -> Int -> [Pulse]
note beats n = freq beats $ semiTone n

wave :: [Pulse]
wave = concat [
        note 0.25 0,
        note 0.25 0,
        note 0.25 0,
        note 0.25 0,
        note 0.5 0,
        note 0.25 0,
        note 0.25 0,
        note 0.25 0,
        note 0.25 0,
        note 0.25 0,
        note 0.25 0,
        note 0.5 0,
        note 0.25 5,
        note 0.25 5,
        note 0.25 5,
        note 0.25 5,
        note 0.25 5,
        note 0.25 5,
        note 0.5 5,
        note 0.25 3,
        note 0.25 3,
        note 0.25 3,
        note 0.25 3,
        note 0.25 3,
        note 0.25 3,
        note 0.5 3,
        note 0.5 (-2),
        note 0.25 0,
        note 0.25 0,
        note 0.25 0,
        note 0.25 0,
        note 0.5 0
    ]

save :: FilePath -> [Float] -> IO ()
save path xs = BL.writeFile path $ BB.toLazyByteString $ foldMap BB.floatLE xs

play :: IO ProcessHandle
play = do
    save outFilePath wave

    runCommand $ printf "ffplay -ar %.3f -autoexit -f f32le -showmode 1 %s" audioRate outFilePath

main :: IO ()
main = do
    processHandle <- play
    _ <- waitForProcess processHandle

    putStrLn "Audio playback completed."
