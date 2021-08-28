module Unsafe where
import Data.Time.Clock

get_identifier :: IO String
get_identifier = fmap show getCurrentTime
