{-# LANGUAGE QuasiQuotes #-}

module Domain.Auth
  ( someFunc
  , Auth(..)
  , Email
  , mkEmail
  , rawEmail
  , Password
  , mkPassword
  , rawPassword
  , UserId
  , VerificationCode
  , SessionId
  , RegistrationError(..)
  , EmailVerificationError(..)
  , LoginError(..)
   -- * Ports
  , AuthRepo(..)
  , EmailVerificationNotif(..)
  , SessionRepo(..)
   -- * Use cases
  , register
  , verifyEmail
  , login
  , resolveSessionId
  , getUser
  ) where

import           ClassyPrelude
import           Control.Monad.Except
import           Domain.Validation
import           Text.Regex.PCRE.Heavy

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Email = Email
  { emailRaw :: Text
  } deriving (Show, Eq, Ord)

rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [Text] Email
mkEmail =
  validate
    Email
    [ regexMatches
        [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
        "Not a valid email"
    ]

newtype Password = Password
  { passwordRaw :: Text
  } deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword =
  validate
    Password
    [ lengthBetween 5 50 "Should between 5 and 50"
    , regexMatches [re|\d|] "Should contain number"
    , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
    , regexMatches [re|[a-z]|] "Should contain lowercase letter"
    ]

data Auth = Auth
  { authEmail    :: Email
  , authPassword :: Password
  } deriving (Show, Eq)

data EmailVerificationError =
  EmailVerificationInvalidCode
  deriving (Show, Eq)

data LoginError
  = LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified
  deriving (Show, Eq)

data RegistrationError =
  RegistrationErrorEmailTaken
  deriving (Show, Eq)

type UserId = Int

type VerificationCode = Text

type SessionId = Text

class Monad m =>
      AuthRepo m
  where
  addAuth :: Auth -> m (Either RegistrationError VerificationCode)
  findEmailFromUserId :: UserId -> m (Maybe Email)
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError ())

class Monad m =>
      SessionRepo m
  where
  newSession :: UserId -> m SessionId
  findUserBySessionId :: SessionId -> m (Maybe UserId)

class Monad m =>
      EmailVerificationNotif m
  where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

instance AuthRepo IO where
  addAuth (Auth email pass) = do
    putStrLn $ "adding auth: " <> rawEmail email
    return $ Right "fake verification code"
  setEmailAsVerified vcode = do
    putStrLn $ "email verified: " <> vcode
    return $ Right ()

instance EmailVerificationNotif IO where
  notifyEmailVerification email vcode =
    putStrLn $ "Notify " <> rawEmail email <> " - " <> vcode

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId

login :: (AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth =
  runExceptT $ do
    result <- lift $ findUserByAuth auth
    case result of
      Nothing         -> throwError LoginErrorInvalidAuth
      Just (_, False) -> throwError LoginErrorEmailNotVerified
      Just (uId, _)   -> lift $ newSession uId

register ::
     (AuthRepo m, EmailVerificationNotif m)
  => Auth
  -> m (Either RegistrationError ())
register auth =
  runExceptT $ do
    vCode <- ExceptT $ addAuth auth
    let email = authEmail auth
    lift $ notifyEmailVerification email vCode

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserBySessionId

verifyEmail ::
     AuthRepo m => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail = setEmailAsVerified
