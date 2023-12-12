module Adapter.InMemory.Auth where

import ClassyPrelude
import qualified Domain.Auth as d

type State = State
    { stateAuths :: [(D.UserId, D.Auth)]
    , stateUnverifiedEmails :: Map D.VerificationCode D.Email
    , stateVerfiedEmails :: Set D.Email
    , stateUserIdCounter :: Int
    , stateNotifications Map D.Email, D.VerificationCode
    , stateSessions :: Map D.SessionId D.UserId
    } deriving (Eq, Show)