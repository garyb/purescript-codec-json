module Codec.JSON.DecodeError where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, over)
import Data.String as String
import JSON.Path as JP

-- | Type for failures while decoding, a path to the point in the JSON that failure occurred, a
-- | message describing the problem, and a list of further causes for the failure.
newtype DecodeError = DecodeError DecodeErrorDetails

type DecodeErrorDetails =
  { path ∷ JP.Path
  , message ∷ String
  , causes ∷ Array DecodeError
  }

derive instance Eq DecodeError
derive instance Ord DecodeError
derive instance Generic DecodeError _
derive instance Newtype DecodeError _

instance Show DecodeError where
  show (DecodeError err) = "(DecodeError " <> show err <> ")"

instance Semigroup DecodeError where
  append (DecodeError err1) (DecodeError err2) =
    DecodeError
      { path: JP.findCommonPrefix err1.path err2.path
      , message: altMessage
      , causes:
          (if err1.message == altMessage then err1.causes else [ DecodeError err1 ])
            <> (if err2.message == altMessage then err2.causes else [ DecodeError err2 ])
      }
    where
    altMessage ∷ String
    altMessage = "Failed to decode alternatives"

-- | Prints an `DecodeError` as a somewhat readable error message.
print ∷ DecodeError → String
print (DecodeError err) = pathPart <> err.message <> details
  where
  pathPart = if err.path == JP.Tip then "" else JP.print err.path <> ": "
  causes =
    map
      ( \e →
          String.replaceAll
            (String.Pattern "\n")
            (String.Replacement (if Array.length err.causes == 1 then "\n  " else "\n    "))
            (print (withPath (\p → fromMaybe p (JP.stripPrefix err.path p)) e))
      )
      err.causes
  details =
    case Array.length err.causes of
      0 → ""
      1 → ":\n  " <> String.joinWith "\n  " causes
      _ → ":\n  - " <> String.joinWith "\n  - " causes

-- | Updates the path for an error. The transformation is applied to the error itself along with
-- | its causes, recursively. This is intended for extending the path to elaborate on the location
-- | of errors from the top down.
withPath ∷ (JP.Path → JP.Path) → DecodeError → DecodeError
withPath f = over DecodeError \err → err { path = f err.path, causes = map (withPath f) err.causes }

-- | Starts a new context for the error, pushing the current error into `causes` and providing a
-- | new message. This is useful for cases where you want to introduce a higher level error, adding
-- | information about domain types, for example.
withContext ∷ String → DecodeError → DecodeError
withContext message =
  over DecodeError \err →
    { path: err.path
    , message
    , causes: [ DecodeError err ]
    }

-- | Constructs an error from a path and message (no further causes).
error ∷ JP.Path → String → DecodeError
error path message = DecodeError { path, message, causes: [] }

-- | Constructs a basic error from just a message.
basic ∷ String → DecodeError
basic = error JP.Tip

-- | Constructs an error with the message "No value found" and the specified path.
noValueFound ∷ JP.Path → DecodeError
noValueFound path = error path "No value found"
