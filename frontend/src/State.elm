module State exposing (..)

import File exposing (File)
import Url
import Http
import Browser
import RouteParser exposing (..)
import Browser.Navigation as Nav
import User exposing (..)
import Image exposing (..)
import GuessResult exposing (GuessResult)
import Session

type alias RegistrationForm =
    { displayname: String
    , username: String
    , password: String
    , password_again: String}

type Session = LoggedIn User
    | LoggedOut
    
type alias LoginForm =
    { username: String
    , password: String }

type alias GroupManagerState =
    { selectedGroup: Maybe Group
    , selectedUser: Maybe User
    , selectedPermission: Maybe Permission
    , loadedGroups: List Group}

type alias MediaManagerState =
    { known_metadata: List ImageMetadata }

type GameType
    = LocationGuessing
    | PictureGuessing
        
type GameState
    = NotPlaying
    | ChoosingCounty GameType
    | PictureGuessingState
      (Maybe ImageMetadata) -- current pic 
      Int -- current score
      Int -- count of tries
      Int -- county code
      Bool -- allow-for-usage
      (List File) -- submitted file
    | LocationGuessingState
      (Maybe ImageMetadata) -- current pic 
      Int -- current score
      Int -- count of tries
      Int -- county code
          
type alias UserSettingsFormState =
    { username: String
    , displayname: String
    , oldPassword: String
    , newPassword: String
    , newImage: Maybe File}
    
type alias Model =
    { route: Route
    , key: Nav.Key
    , registrationFormState: Maybe RegistrationForm
    , loginState: LoginForm
    , session: Session
    , groupManagerState: Maybe GroupManagerState
    , mediaManagerState: Maybe MediaManagerState
    , gameState: GameState
    , imageCounts: List PictureCount
    , sessionId: Maybe String
    , picGameSession: Maybe Session.Session
    , locationGameSession: Maybe Session.Session
    , usersettingsform: Maybe UserSettingsFormState
    , highestSessionData: Maybe Session.SessionHighscore}

type Nth
    = First
    | Second


type SessionType
    = Picture
    | Location       

type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | RegistrationDisplayname String
    | RegistrationUsername String
    | RegistrationPassword Nth String
    | SendRegistration RegistrationForm
    | DummyResponse (Result Http.Error ())
    | LoginResult (Result Http.Error User)
    | LoginUsername String
    | LoginPassword String
    | Login LoginForm
    | Logout
    | LogoutResult (Result Http.Error ())
    | SessionResult (Result Http.Error User)
    | GroupTreeResult (Result Http.Error (List Group))
    -- strings here are ids, because you cant set an 'a as elm's html option's value
    | AdminGroupSelected String
    | AdminUserSelected String
    | AdminSelectExistingAbility String
    | AdminDisallow
    | AdminAllow
    | AdminSelectNonExistingAbility String
    | AdminUserToGroup
    | AdminUserActivated User Bool
    | AdminUserFromGroup
    | SaveGroupManagerState
    | GotInputFiles (List File)
    | UploadedImage (Result Http.Error ())
    | GotPictureIds (Result Http.Error (List ImageMetadata))
    | RemovePicture ImageMetadata
    | RemovalResult (Result Http.Error Bool)
    | GotNextPicForGame (Result Http.Error (Maybe ImageMetadata))
    | MapClicked (Float, Float, Float)
    | ChoseCounty GameType String
    | SetAllowForUsage Bool
    | GotGameFiles (List File)
    | NoGpsFound ()
    | SubmitGuess
    | UploadedGuess (Result Http.Error GuessResult)
    | GotPictureCounts (Result Http.Error (List PictureCount))
    | GotSessionData SessionType (Result Http.Error Session.Session)
    | SaveLoggedInUser User UserSettingsFormState
    | ChangeUserField UserSettingsFormState String String
    | SavedUser (Result Http.Error User)
    | GotUserFile (List File)
    | GotGameSessionHighs (Result Http.Error Session.SessionHighscore)
