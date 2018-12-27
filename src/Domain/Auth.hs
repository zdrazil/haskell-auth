module Domain.Auth
  ( someFunc
  )
where

import           ClassyPrelude

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Email = Email
  { emailRaw :: Text
  } deriving (Show, Eq)

rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [Text] Email
mkEmail = undefined

newtype Password = Password
  { passwordRaw :: Text
  } deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword = undefined

data Auth = Auth
  { authEmail    :: Text
  , authPassword :: Text
  } deriving (Show, Eq)

data RegistrationError =
  RegistrationErrorEmailTaken
  deriving (Show, Eq)
