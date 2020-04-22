module Database.Author
    ( Author(..)
    , renderAuthor
    ) where

import Data.Time.Format
import Data.Time.LocalTime (ZonedTime)

data Author = Author {
    authorName :: String,
    authorEmail :: String,
    authorTimestamp :: ZonedTime
}

renderAuthor :: Author -> String
renderAuthor author =
    let ts = formatTime defaultTimeLocale "%s %z" (authorTimestamp author)
    in authorName author ++ " <" ++ authorEmail author ++ "> " ++ ts
