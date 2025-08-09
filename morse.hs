-- sammy v3

import qualified Prelude as H
import Prelude hiding (length)
import Data.Char
import Data.Functor
import Data.List hiding (length)
import System.Environment
import System.Process

showNumber :: Float -> String
showNumber n
  | (f, 'e' : m) <- break ('e' ==) c = showNumber' (read m :: Int) (point f)
  | otherwise = c
  where
    c = show n
    point f
      | (l, '.' : r) <- break ('.' ==) f = (l, r)
      | otherwise = (f, [])
showNumber' 0 (l, r) = l <> "." <> r
showNumber' m ([], r)
  | m < 0 = showNumber' (m + 1) ("0", '0' : r)
showNumber' m ("-0", r)
  | m < 0 = showNumber' (m + 1) ("-0", '0' : r)
showNumber' m (l, [])
  | 0 < m = showNumber' (m - 1) (l <> "0", [])
showNumber' m (l, r)
  | m < 0 = showNumber' (m + 1) (init l, last l : r)
  | 0 < m = showNumber' (m - 1) (l <> [head r], tail r)

type Speed = Float
speed :: Speed
speed = 23

type Length = Float
length :: Length
length = 1 / frequency * speed

type Frequency = Float
frequency :: Frequency
frequency = 42e1

newtype Element = Element (Length, Pulse) deriving Show
data Pulse = Pulse | Silence deriving Show
dot, dash,
  elementSpace, characterSpace, wordSpace
  :: Element
dot = Element (length * 1, Pulse)
dash = Element (length * 3, Pulse)
elementSpace = Element (length * 1, Silence)
characterSpace = Element (length * 3, Silence)
wordSpace = Element (length * 7, Silence)
es = elementSpace
cs = characterSpace
ws = wordSpace

type Command = String
key :: Element -> FilePath -> Command
keys :: Command
key (Element (l, Silence)) f =
  unwords
    ["sox -q -r 44100",
     "-n",
     "\x22" <> f <> ".flac" <> "\x22",
     "trim", show 0, showNumber l
    ]
key (Element (l, Pulse)) f =
  unwords
    ["ffmpeg -loglevel 8 -y",
     "-f", "lavfi",
     "-i", "sine=frequency=" <> showNumber frequency <> ":duration=" <> showNumber l,
     "-c:0", "flac", "-compression_level", "12",
     "\x22" <> f <> ".flac" <> "\x22"
    ]
keys =
  intercalate " && "
    [key dot "dot", key dash "dash",
     key es "es", key cs "cs", key ws "ws"
    ]

type Key = String
hello :: [Key]
hello =
  ["dot", "es", "dot", "es", "dot", "es", "dot", "cs",
   "dot", "cs",
   "dot", "es", "dash", "es", "dot", "es", "dot", "cs",
   "dot", "es", "dash", "es", "dot", "es", "dot", "cs",
   "dash", "es", "dash", "es", "dash", "es"
  ]

character :: Char -> [Key]
character c =
  case c of
    'a' -> ["dot", "dash"]
    'b' -> ["dash", "dot", "dot", "dot"]
    'c' -> ["dash", "dot", "dash", "dot"]
    'd' -> ["dash", "dot", "dot"]
    'e' -> ["dot"]
    'f' -> ["dot", "dot", "dash", "dot"]
    'g' -> ["dash", "dash", "dot"]
    'h' -> ["dot", "dot", "dot", "dot"]
    'i' -> ["dot", "dot"]
    'j' -> ["dot", "dash", "dash", "dash"]
    'k' -> ["dash", "dot", "dash"]
    'l' -> ["dot", "dash", "dot", "dot"]
    'm' -> ["dash", "dash"]
    'n' -> ["dash", "dot"]
    'o' -> ["dash", "dash", "dash"]
    'p' -> ["dot", "dash", "dash", "dot"]
    'q' -> ["dash", "dash", "dot", "dash"]
    'r' -> ["dot", "dash", "dot"]
    's' -> ["dot", "dot", "dot"]
    't' -> ["dash"]
    'u' -> ["dot", "dot", "dash"]
    'v' -> ["dot", "dot", "dot", "dash"]
    'w' -> ["dot", "dash", "dash"]
    'x' -> ["dash", "dot", "dot", "dash"]
    'y' -> ["dash", "dot", "dash", "dash"]
    'z' -> ["dash", "dash", "dot", "dot"]
    '0' -> ["dash", "dash", "dash", "dash", "dash"]
    '1' -> ["dot", "dash", "dash", "dash", "dash"]
    '2' -> ["dot", "dot", "dash", "dash", "dash"]
    '3' -> ["dot", "dot", "dot", "dash", "dash"]
    '4' -> ["dot", "dot", "dot", "dot", "dash"]
    '5' -> ["dot", "dot", "dot", "dot", "dot"]
    '6' -> ["dash", "dot", "dot", "dot", "dot"]
    '7' -> ["dash", "dash", "dot", "dot", "dot"]
    '8' -> ["dash", "dash", "dash", "dot", "dot"]
    '9' -> ["dash", "dash", "dash", "dash", "dot"]
    _ -> error ("Unknown letter in alphanumeric Morse code: " ++ show c)

international :: Char -> [Key]
international c =
  case c of
    'a' -> ["dot", "dash"]
    'b' -> ["dash", "dot", "dot", "dot"]
    'c' -> ["dash", "dot", "dash", "dot"]
    'd' -> ["dash", "dot", "dot"]
    'e' -> ["dot"]
    'f' -> ["dot", "dot", "dash", "dot"]
    'g' -> ["dash", "dash", "dot"]
    'h' -> ["dot", "dot", "dot", "dot"]
    'i' -> ["dot", "dot"]
    'j' -> ["dot", "dash", "dash", "dash"]
    'k' -> ["dash", "dot", "dash"]
    'l' -> ["dot", "dash", "dot", "dot"]
    'm' -> ["dash", "dash"]
    'n' -> ["dash", "dot"]
    'o' -> ["dash", "dash", "dash"]
    'p' -> ["dot", "dash", "dash", "dot"]
    'q' -> ["dash", "dash", "dot", "dash"]
    'r' -> ["dot", "dash", "dot"]
    's' -> ["dot", "dot", "dot"]
    't' -> ["dash"]
    'u' -> ["dot", "dot", "dash"]
    'v' -> ["dot", "dot", "dot", "dash"]
    'w' -> ["dot", "dash", "dash"]
    'x' -> ["dash", "dot", "dot", "dash"]
    'y' -> ["dash", "dot", "dash", "dash"]
    'z' -> ["dash", "dash", "dot", "dot"]
    '\xe9' -> ["dot", "dot", "dash", "dot", "dot"]
    '0' -> ["dash", "dash", "dash", "dash", "dash"]
    '1' -> ["dot", "dash", "dash", "dash", "dash"]
    '2' -> ["dot", "dot", "dash", "dash", "dash"]
    '3' -> ["dot", "dot", "dot", "dash", "dash"]
    '4' -> ["dot", "dot", "dot", "dot", "dash"]
    '5' -> ["dot", "dot", "dot", "dot", "dot"]
    '6' -> ["dash", "dot", "dot", "dot", "dot"]
    '7' -> ["dash", "dash", "dot", "dot", "dot"]
    '8' -> ["dash", "dash", "dash", "dot", "dot"]
    '9' -> ["dash", "dash", "dash", "dash", "dot"]
    '.' -> ["dot", "dash", "dot", "dash", "dot", "dash"]
    ',' -> ["dash", "dash", "dot", "dot", "dash", "dash"]
    '?' -> ["dot", "dot", "dash", "dash", "dot", "dot"]
    '\x27' -> ["dot", "dash", "dash", "dash", "dash", "dot"]
    '/' -> ["dash", "dot", "dot", "dash", "dot"]
    '(' -> ["dash", "dot", "dash", "dash", "dot"]
    ')' -> ["dash", "dot", "dash", "dash", "dot", "dash"]
    ':' -> ["dash", "dash", "dash", "dot", "dot", "dot"]
    '=' -> ["dash", "dot", "dot", "dot", "dash"] 
    '+' -> ["dot", "dash", "dot", "dash", "dot"]
    '-' -> ["dash", "dot", "dot", "dot", "dot", "dash"]
    '"' -> ["dot", "dash", "dot", "dot", "dash", "dot"]
    '@' -> ["dot", "dash", "dash", "dot", "dash", "dot"]
    _ -> error ("Unknown letter in international Morse code: " ++ show c)

base95 :: Char -> [Key]
base95 c =
  case c of
    ' ' -> code "white"
    '!' -> code "control"
    '"' -> code "bunny"
    '#' -> code "hash"
    '$' -> code "ching"
    '%' -> code "mod"
    '&' -> code "address"
    '\x27' -> code "irk"
    '(' -> code "wax"
    ')' -> code "wane"
    '*' -> code "aster"
    '+' -> code "cross"
    ',' -> code "tail"
    '-' -> code "option"
    '.' -> code "hidden"
    '/' -> code "yes"
    '0' -> character c
    '1' -> character c
    '2' -> character c
    '3' -> character c
    '4' -> character c
    '5' -> character c
    '6' -> character c
    '7' -> character c
    '8' -> character c
    '9' -> character c
    ':' -> code "type"
    ';' -> code "break"
    '<' -> code "leftoid"
    '=' -> code "equals"
    '>' -> code "rightoid"
    '?' -> code "query"
    '@' -> code "whorl"
    'A' -> code "big" <> character (toLower c)
    'B' -> code "big" <> character (toLower c)
    'C' -> code "big" <> character (toLower c)
    'D' -> code "big" <> character (toLower c)
    'E' -> code "big" <> character (toLower c)
    'F' -> code "big" <> character (toLower c)
    'G' -> code "big" <> character (toLower c)
    'H' -> code "big" <> character (toLower c)
    'I' -> code "big" <> character (toLower c)
    'J' -> code "big" <> character (toLower c)
    'K' -> code "big" <> character (toLower c)
    'L' -> code "big" <> character (toLower c)
    'M' -> code "big" <> character (toLower c)
    'N' -> code "big" <> character (toLower c)
    'O' -> code "big" <> character (toLower c)
    'P' -> code "big" <> character (toLower c)
    'Q' -> code "big" <> character (toLower c)
    'R' -> code "big" <> character (toLower c)
    'S' -> code "big" <> character (toLower c)
    'T' -> code "big" <> character (toLower c)
    'U' -> code "big" <> character (toLower c)
    'V' -> code "big" <> character (toLower c)
    'W' -> code "big" <> character (toLower c)
    'X' -> code "big" <> character (toLower c)
    'Y' -> code "big" <> character (toLower c)
    'Z' -> code "big" <> character (toLower c)
    '[' -> code "lbrick"
    '\x5c' -> code "no"
    ']' -> code "rbrick"
    '^' -> code "hat"
    '_' -> code "under"
    '`' -> code "tick"
    'a' -> character c
    'b' -> character c
    'c' -> character c
    'd' -> character c
    'e' -> character c
    'f' -> character c
    'g' -> character c
    'h' -> character c
    'i' -> character c
    'j' -> character c
    'k' -> character c
    'l' -> character c
    'm' -> character c
    'n' -> character c
    'o' -> character c
    'p' -> character c
    'q' -> character c
    'r' -> character c
    's' -> character c
    't' -> character c
    'u' -> character c
    'v' -> character c
    'w' -> character c
    'x' -> character c
    'y' -> character c
    'z' -> character c
    '{' -> code "lsik"
    '|' -> code "pipe"
    '}' -> code "rsik"
    '~' -> code "knot"
    _ -> error ("Unknown letter in base-95 Morse code: " ++ show c)
  where code = concatMap character

data Mode = Simple | International | Base95 deriving Show

send :: Mode -> String -> FilePath -> [Command]
send m s f = page (string m s) f

string :: Mode -> String -> [Key]
string Base95 s =
  ((<> ["es"]) . intercalate ["ws"] .
   intercalate [["cs"]] .
   (map . map) (intersperse "es" . base95) .
   lines
  ) s
string m s =
  ((<> ["es"]) . intercalate ["ws"] .
   intercalate [["cs"]] .
   (map . map) (intersperse "es" . setting) .
   words
  ) s
  where
    setting =
      case m of
        Simple -> character
        International -> international

pageLength :: Int
pageLength = 100

page :: [Key] -> FilePath -> [Command]
page kk f
  | H.length kk < pageLength + 1 =
    [unwords
       ["sox -q",
        unwords (map (<> ".flac") kk),
        "\x22" <> f <> "\x22"
       ]
    ]
  | otherwise = page1 0 kk f
page1 p kk f
  | H.length kk < pageLength + 1 = thisPage : [all]
  | otherwise = thisPage : page1 (p + 1) (drop pageLength kk) f
  where
    pad n s = replicate (n - H.length s) '0' <> s
    thisPage =
      unwords
        ["sox -q",
         unwords (map (<> ".flac") (take pageLength kk)),
         "/tmp/out." <> pad 2 (show p) <> ".flac"
        ]
    all =
      unwords
        ["sox -q", 
         (unwords . map ((\p -> "/tmp/out." <> p <> ".flac") . pad 2 . show)) [0 .. p],
         "\x22" <> f <> "\x22"
        ]

run :: Command -> IO ()
runAll :: [Command] -> IO ()
run = void . system
runAll = mapM_ system

main :: IO ()
main = do
  -- -- This line debugs by printing the length, who is termed in frequency and speed.
  -- putStrLn ("DEBUG LENGTH: " ++ show length)
  morse_key <- lookupEnv "MORSE_KEY"
  case morse_key of
    Just "yes" -> run keys
    _ -> do
      morse_base95 <- lookupEnv "MORSE_BASE95"
      morse_intl <- lookupEnv "MORSE_INTL"
      let mode =
            case morse_base95 of
              Just "yes" -> Base95 
              _ ->
                case morse_intl of
                  Just "yes" -> International
                  _ -> Simple
      morse_play <- lookupEnv "MORSE_PLAY"
      morse_small <- lookupEnv "MORSE_SMALL"
      input <-
        case mode of
          Base95 -> getContents
          _ -> getLine
      args <- getArgs
      let output =
            case args of
              [output] ->
                if (not . null) output
                  then output
                 else "/tmp/out.flac"
              _ -> "/tmp/out.flac"
      runAll (send mode input output)
      case morse_small of
        Just "yes" -> do
          run ("ffmpeg -loglevel 8 -y -i " <> "\x22" <> output <> "\x22" <> " -c:0 libspeex -cbr_quality 0 " <> "\x22" <> output <> ".spx" <> "\x22")
          run ("rm " <> "\x22" <> output <> "\x22")
          case morse_play of
            Just "no" -> mempty
            _ -> run ("ffplay -loglevel 8 -showmode 0 -autoexit " <> "\x22" <> output <> ".spx" <> "\x22" <> " > /dev/null")
        _ ->
          case morse_play of
            Just "no" -> mempty
            _ -> run ("ffplay -loglevel 8 -showmode 0 -autoexit " <> "\x22" <> output <> "\x22" <> " > /dev/null")
