module Domain.Auth (
  -- * Types
  Auth(..),
  Email,
  mkEmail,
  rawEmail,
  Password,
  mkPassword,
  rawPassword,
  UserId,
  VerificationCode,
  SessionId,
  RegistrationError(..),
  EmailVerificationError(..),
  LoginError(..),

  -- * Ports
  AuthRepo(..),
  EmailVerificationNotif(..),
  SessionRepo(..),

  -- * Use Cases
  register,
  verifyEmail,
  login,
  resolveSessionId,
  getUser
) where
  
import ClassyPrelude
import Domain.Validation
import Text.Regex.PCRE.Heavy
import Control.Monad.Except

data Auth = Auth
  { authEmail :: Email
  , authPassword :: Password
  } deriving (Show, Eq)

data RegistrationError
  = RegistrationErrorEmailTaken
  deriving (Show, Eq)

-- Email
newtype Email = Email { emailRaw :: Text } deriving (Show, Eq, Ord)

rawEmail :: Email -> Text
rawEmail = emailRaw

data EmailValdationError = EmailValidationErrorInvalidEmail

mkEmail :: Text -> Either [Text] Email
mkEmail = validate Email
  [ regexMatches
    [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
    "Not a valid email"
  ]


-- Password
newtype Password = Password { passwordRaw :: Text } deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

data PasswordValidationError
  = PasswordValidationErrorLength Int
  | PasswordValidationErrorMustContainUpperCase
  | PasswordValidationErrorMustContainLowerCase
  | PasswordValidationErrorMustContainNumber

mkPassword :: Text -> Either [Text] Password 
mkPassword = validate Password
  [ lengthBetween 5 50 "Should be between 5 and 50" 
  , regexMatches [re|\d|] "Should contain a number"
  , regexMatches [re|[A-Z]|] "Should contain upper case letter"
  , regexMatches [re|[a-z]|] "Should contain a lower case letter"
  ]

type VerificationCode = Text

class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError VerificationCode)
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError ())
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUserId :: UserId -> m (Maybe Email)
  
getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId

verifyEmail :: AuthRepo m => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail =  setEmailAsVerified

instance AuthRepo IO where
  addAuth (Auth email pass) = do
    putStrLn $ "adding auth: " <> rawEmail email
    return $ Right "fake verification code"

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

instance EmailVerificationNotif IO where
  notifyEmailVerification email vcode =
    putStrLn $ "Notify " <> rawEmail email <> " - " <> vcode

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserBySessionId :: SessionId -> m (Maybe UserId)

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserBySessionId

register :: (AuthRepo m, EmailVerificationNotif m) => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
  vCode <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email vCode

data EmailVerificationError = EmailVerificationErrorInvalidCode
  deriving (Eq, Show)



type UserId = Int

type SessionId = Text

login :: (AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just (_, False) -> throwError LoginErrorEmailNotVerified
    Just (uId, _) -> lift $ newSession uId

data LoginError = LoginErrorInvalidAuth | LoginErrorEmailNotVerified
  deriving (Show, Eq)

