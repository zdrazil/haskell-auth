module Adapter.InMemory.Auth where

import           ClassyPrelude
import           Data.Has
import qualified Domain.Auth                   as D

data State = State
  { stateAuths            :: [(D.UserId, D.Auth)]
  , stateUnverifiedEmails :: Map D.VerificationCode D.Email
  , stateVerifiedEmails   :: Set D.Email
  , stateUserIdCounter    :: Int
  , stateNotifications    :: Map D.Email D.VerificationCode
  , stateSessions         :: Map D.SessionId D.UserId
  } deriving (Show, Eq)

initialState :: State
initialState = State { stateAuths            = []
                     , stateUnverifiedEmails = mempty
                     , stateVerifiedEmails   = mempty
                     , stateUserIdCounter    = 0
                     , stateNotifications    = mempty
                     , stateSessions         = mempty
                     }

type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

addAuth :: InMemory r m => Auth -> m (Either RegistrationError VerificationCode)
addAuth = undefined

setEmailAsVerified
  :: InMemory r m => VerificationCode -> m (Either EmailVerificationError ())
setEmailAsVerified = undefined

findUserByAuth :: InMemory r m => Auth -> m (Maybe (UserId, Bool))
findUserByAuth = undefined

findEmailFromUserId :: InMemory r m => UserId -> m (Maybe Email)
findEmailFromUserId = undefined

notifyEmailVerification :: InMemory r m => Email -> VerificationCode -> m ()
notifyEmailVerification = undefined

newSession :: InMemory r m => UserId -> m SessionId
newSession = undefined

findUserIdBySessionId :: InMemory r m => SessionId -> m (Maybe UserId)
findUserIdBySessionId = undefined
