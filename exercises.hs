import Data.Maybe
import Data.Word

-- Exercises from Section 4.2 (recursion)

replicate' :: Int -> a -> [a]
replicate' n x 
    | n <= 0    = []
    | otherwise = x : replicate' (n - 1) x

even' :: Int -> Bool
even' 0 = True
even' n = odd'(n - 1)

odd' :: Int -> Bool 
odd' 0 = False 
odd' n = even' (n - 1)

-- Exercises from Section 4.4 (pattern matching)

-- Colors

data Color
  = RGB Word8 Word8 Word8
  deriving Show

data Brightness
  = Dark
  | Bright
  deriving (Show, Eq)

data EightColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving Show

data AnsiColor
  = AnsiColor Brightness EightColor
  deriving Show

isBright :: AnsiColor -> Bool
isBright (AnsiColor brightness _) = brightness == Bright

ansiColorToUbuntu :: AnsiColor -> Color
ansiColorToUbuntu ansicolor =
    case ansicolor of
        AnsiColor Dark Black -> RGB 1 1 1
        AnsiColor Bright Black -> RGB 128 128 128
        AnsiColor Dark Red -> RGB 222 56 43
        AnsiColor Bright Red -> RGB 255 0 0
        AnsiColor Dark Green -> RGB 57 181 74
        AnsiColor Bright Green -> RGB 0 255 0
        AnsiColor Dark Yellow -> RGB 255 199 6
        AnsiColor Bright Yellow -> RGB 255 255 0
        AnsiColor Dark Blue -> RGB 0 111 184
        AnsiColor Bright Blue -> RGB 0 0 255
        AnsiColor Dark Magenta -> RGB 118 38 113
        AnsiColor Bright Magenta -> RGB 255 0 255
        AnsiColor Dark Cyan -> RGB 44 181 233
        AnsiColor Bright Cyan -> RGB 0 255 255
        AnsiColor Dark White -> RGB 204 204 204
        AnsiColor Bright White -> RGB 255 255 255


-- isEmpty

isEmpty :: [a] -> Bool 
isEmpty ls =
    case listToMaybe ls of
        Nothing -> True
        _       -> False

isEmpty' :: [a] -> Bool 
isEmpty' [] = True
isEmpty' _  = False 

