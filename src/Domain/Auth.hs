-- Page 46
module Domain.Auth
  ( someFunc
  )
where

import           ClassyPrelude
import           Domain.Validation
import           Text.Regex.PCRE.Heavy
import           Control.Monad.Except

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type VerificationCode = Text

class Monad m => AuthRepo m
  where
  addAuth :: Auth -> m (Either RegistrationError VerificationCode)

class Monad m =>
      EmailVerificationNotif m
  where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

register
  :: (AuthRepo m, EmailVerificationNotif m)
  => Auth
  -> m (Either RegistrationError ())
register auth = runExceptT $ do
  vCode <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email vCode


newtype Email = Email
  { emailRaw :: Text
  } deriving (Show, Eq)

rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [Text] Email
mkEmail = validate
  Email
  [ regexMatches [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
                 "Not a valid email"
  ]

newtype Password = Password
  { passwordRaw :: Text
  } deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword = validate
  Password
  [ lengthBetween 5 50 "Should between 5 and 50"
  , regexMatches [re|\d|]    "Should contain number"
  , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
  , regexMatches [re|[a-z]|] "Should contain lowercase letter"
  ]

data Auth = Auth
  { authEmail    :: Email
  , authPassword :: Password
  } deriving (Show, Eq)

data RegistrationError =
  RegistrationErrorEmailTaken
  deriving (Show, Eq)
