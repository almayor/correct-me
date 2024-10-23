module Main where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Either (isLeft, isRight)
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Data.Vector (Vector, toList)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant ((:>), (:<|>))
import Servant as S
import Servant.API.Flatten (flatten)
import Servant.Auth as SA
import Servant.Client
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher
import Text.Regex.TDFA ((=~))

import Lib
import Lib.Api
import Lib.Types
import Data.String (fromString)
import Data.Traversable (for)

import Utils

-- according to https://docs.servant.dev/en/stable/cookbook/jwt-and-basic-auth/JWTAndBasicAuth.html
-- Servant.Auth.Client only supports JWT auth, so we need to use BasicAuthClient
-- by swapping Auth '[SA.BasicAuth, SA.JWT] with BasicAuthData
type ClientAPI = "api" :> (PublicAppAPI :<|> S.BasicAuth "basic-auth" User :> ProtectedAppAPI)

-- Define the client functions
registerC :: UserReq -> ClientM LocPath
getUserC :: BasicAuthData -> UserID -> ClientM User
listUsersC :: BasicAuthData -> ClientM (Vector LocPath)
listPhrasesByUserC :: BasicAuthData -> UserID -> Bool -> ClientM (Vector LocPath)
listPhrasesC :: BasicAuthData -> Bool -> ClientM (Vector LocPath)
insertPhraseC :: BasicAuthData -> PhraseReq -> ClientM LocPath
getPhraseC :: BasicAuthData -> PhraseID -> ClientM Phrase
listAlternativesByPhraseC :: BasicAuthData -> PhraseID -> ClientM (Vector LocPath)
insertAlternativeC :: BasicAuthData -> PhraseID -> AlternativeReq -> ClientM LocPath
listAlternativesC :: BasicAuthData -> Maybe UserID -> ClientM (Vector LocPath)
getAlternativeC :: BasicAuthData -> AlternativeID -> ClientM Alternative
chooseAlternativeC :: BasicAuthData -> AlternativeID -> ClientM LocPath

registerC :<|> listUsersC :<|> getUserC :<|> listPhrasesByUserC :<|> listPhrasesC :<|> insertPhraseC :<|> getPhraseC :<|> listAlternativesByPhraseC :<|> insertAlternativeC :<|> listAlternativesC :<|> getAlternativeC :<|> chooseAlternativeC = client $ flatten (Proxy :: Proxy ClientAPI)

matchRegex :: String -> String -> Bool
matchRegex = flip (=~)

-- silencing output and disabling spell checker
withServer :: (Warp.Port -> IO()) -> IO()
withServer action = withSilencedOutput $ withModifiedEnv [("CORRECTME_SPELLER_ENABLED", "false")] $ do
    initDb -- initializing database from schema
    application <- getApplication
    Warp.testWithApplication (pure application) action

publicSpec :: Spec
publicSpec = with (withSilencedOutput(initDb >> getApplication)) $ do
         -- GET protected endpoints without authentication
        let endpoints = [
                  "/api/users"
                , "/api/users/1"
                , "/api/phrases"
                , "/api/phrases/1"
                , "/api/alternatives"
                , "/api/alternatives/1"
                ] :: [ByteString]
        forM_ endpoints $ \endpoint -> do
            describe ("GET " <> BS.unpack endpoint) $ do
                it "should return 401 without authentication" $ do
                    get endpoint `shouldRespondWith` 401

        -- GET docs and swagger without authentication
        let endpoints' = [
                  "/docs"
                , "/swagger.json"
                , "/swagger-ui"
                ] :: [ByteString]
        forM_ endpoints' $ \endpoint -> do
            describe ("GET " <> BS.unpack endpoint) $ do
                it "should return 200 without authentication" $ do
                    get endpoint `shouldRespondWith` 200

        -- GET non-existent endpoint
        describe "GET /non-existent" $ do
            it "should return 404 without authentication" $ do
                get "/non-existent" `shouldRespondWith` 404


businessLogicSpec :: Spec
businessLogicSpec =
    around withServer $ do
        -- create a servant-client ClientEnv
        baseUrl <- runIO $ parseBaseUrl "http://localhost"
        manager <- runIO $ newManager defaultManagerSettings
        let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

        describe "POST /api/users" $ do
            it "can sign up" $ \port -> do
                result <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                case result of
                    Right (LocPath path) -> path `shouldSatisfy` matchRegex "^/api/users/[0-9]+$"
                    Left err -> expectationFailure $ "Expected success but got error: " ++ show err
            
            it "can't sign up with the same username" $ \port -> do
                _      <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                result <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                case result of
                    Left (FailureResponse _ response) -> responseStatusCode response `shouldBe` status409
                    _ -> expectationFailure "Expected failure but got success"

            it "can't sign up an empty username or password" $ \port -> do
                resultEmptyU <- runClientM (registerC $ UserReq "" "test_pass1") (clientEnv port)
                resultEmptyU `shouldSatisfy` isLeft
                resultEmptyP <- runClientM (registerC $ UserReq "test_user1" "") (clientEnv port)
                resultEmptyP `shouldSatisfy` isLeft

            it "can't sign up with a username that is too long" $ \port -> do
                let longUsername = UserName . T.pack . concat . replicate 100 $ "test_user"
                result <- runClientM (registerC $ UserReq longUsername "test_pass1") (clientEnv port)
                result `shouldSatisfy` isLeft
            
        describe "GET /api/users" $ do
            it "can list users and find oneself among them" $ \port -> do
                Right ownLocPath <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                result <- runClientM (listUsersC $ BasicAuthData "test_user1" "test_pass1") (clientEnv port)
                case result of
                    Left err -> expectationFailure $ "Expected success but got error: " ++ show err
                    Right users -> do
                        users `shouldSatisfy` all (\(LocPath path) -> matchRegex "^/api/users/[0-9]+$" path)
                        ownLocPath `shouldSatisfy` (`elem` users)

        describe "GET /api/users/:id" $ do
            it "can get each user by id" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right locPaths <- runClientM (listUsersC basicAuth) (clientEnv port)
                forM_ locPaths $ \locPath -> do
                    let (LocPath path) = locPath
                    let userId = UserID . read . last . splitOn "/" $ path
                    result <- runClientM (getUserC basicAuth userId) (clientEnv port)
                    result `shouldSatisfy` isRight

        describe "GET /api/users/:id" $ do
            it "can get self by id and details match" $ \port -> do
                Right (LocPath path) <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let userId = UserID . read . last . splitOn "/" $ path
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right user <- runClientM (getUserC basicAuth userId) (clientEnv port)
                userUserName user `shouldBe` UserName "test_user1"

        describe "GET /api/users/:id" $ do
            it "can't get a user that doesn't exist" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right locPaths <- runClientM (listUsersC basicAuth) (clientEnv port)
                let existingIds = flip map (toList locPaths) $ \(LocPath path) ->
                        UserID . read . last . splitOn "/" $ path
                let userId = findMissing existingIds
                result <- runClientM (getUserC basicAuth userId) (clientEnv port)
                result `shouldSatisfy` isLeft

        describe "POST /api/phrases" $ do
            it "can insert a phrase" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right (LocPath path) <- runClientM (insertPhraseC basicAuth $ PhraseReq "test_phrase1") (clientEnv port)
                path `shouldSatisfy` matchRegex "^/api/phrases/[0-9]+$"

        describe "GET /api/phrases" $ do
            it "can't insert a phrase with an empty text" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                result <- runClientM (insertPhraseC basicAuth $ PhraseReq "") (clientEnv port)
                result `shouldSatisfy` isLeft

        describe "GET /api/phrases" $ do
            it "can list phrases and find new phrases among them" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right locPath1 <- runClientM (insertPhraseC basicAuth $ PhraseReq "test_phrase1") (clientEnv port)
                Right locPath2 <- runClientM (insertPhraseC basicAuth $ PhraseReq "test_phrase2") (clientEnv port)
                result <- runClientM (listPhrasesC basicAuth False) (clientEnv port)
                case result of
                    Left err -> expectationFailure $ "Expected success but got error: " ++ show err
                    Right locPaths -> do
                        locPath1 `shouldSatisfy` (`elem` locPaths)
                        locPath2 `shouldSatisfy` (`elem` locPaths)
        
        describe "GET /api/phrases/:id" $ do
            it "can get each phrase by id" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right locPaths <- runClientM (listPhrasesC basicAuth False) (clientEnv port)
                forM_ locPaths $ \locPath -> do
                    let (LocPath path) = locPath
                    let phraseId = PhraseID . read . last . splitOn "/" $ path
                    result <- runClientM (getPhraseC basicAuth phraseId) (clientEnv port)
                    result `shouldSatisfy` isRight

        describe "GET /api/phrases/:id" $ do
            it "can get new phrase by id and details match" $ \port -> do
                Right (LocPath userPath) <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let authorId = UserID . read . last . splitOn "/" $ userPath
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right (LocPath path) <- runClientM (insertPhraseC basicAuth
                    $ PhraseReq "test_phrase1") (clientEnv port)
                let phraseId = PhraseID . read . last . splitOn "/" $ path
                result <- runClientM (getPhraseC basicAuth phraseId) (clientEnv port)
                case result of
                    Left err -> expectationFailure $ "Expected success but got error: " ++ show err
                    Right phrase -> do
                        phraseText phrase `shouldBe` T.pack "test_phrase1"
                        phraseAuthorId phrase `shouldBe` authorId
                        phraseIsOpen phrase `shouldBe` True
                        phraseNumAlts phrase `shouldBe` 0
        
        describe "POST /api/alternatives" $ do
            it "can insert an alternative" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right (LocPath phrasePath) <- runClientM (insertPhraseC basicAuth
                    $ PhraseReq "test_phrase1") (clientEnv port)
                let phraseId = PhraseID . read . last . splitOn "/" $ phrasePath
                Right (LocPath path) <- runClientM (insertAlternativeC basicAuth phraseId
                    $ AlternativeReq "test_alternative1") (clientEnv port)
                path `shouldSatisfy` matchRegex "^/api/alternatives/[0-9]+$"
        
        describe "GET /api/alternatives" $ do
            it "can list alternatives and find new alternatives among them" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right (LocPath phrasePath) <- runClientM (insertPhraseC basicAuth
                    $ PhraseReq "test_phrase1") (clientEnv port)
                let phraseId = PhraseID . read . last . splitOn "/" $ phrasePath
                Right locPath1 <- runClientM (insertAlternativeC basicAuth phraseId
                    $ AlternativeReq "test_alternative1") (clientEnv port)
                Right locPath2 <- runClientM (insertAlternativeC basicAuth phraseId
                    $ AlternativeReq "test_alternative2") (clientEnv port)
                result <- runClientM (listAlternativesC basicAuth Nothing) (clientEnv port)
                case result of
                    Left err -> expectationFailure $ "Expected success but got error: " ++ show err
                    Right locPaths -> do
                        locPath1 `shouldSatisfy` (`elem` locPaths)
                        locPath2 `shouldSatisfy` (`elem` locPaths)
        
        describe "GET /api/alternatives" $ do
            it "can get each alternative by id" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right (LocPath phrasePath) <- runClientM (insertPhraseC basicAuth
                    $ PhraseReq "test_phrase1") (clientEnv port)
                let phraseId = PhraseID . read . last . splitOn "/" $ phrasePath
                Right locPath <- runClientM (insertAlternativeC basicAuth phraseId
                    $ AlternativeReq "test_alternative1") (clientEnv port)
                result <- runClientM (listAlternativesC basicAuth Nothing) (clientEnv port)
                case result of
                    Left err -> expectationFailure $ "Expected success but got error: " ++ show err
                    Right locPaths -> locPath `shouldSatisfy` (`elem` locPaths)
        
        describe "GET /api/alternatives/:id" $ do
            it "can get new alternative by id and details match" $ \port -> do
                Right (LocPath userPath) <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let authorId = UserID . read . last . splitOn "/" $ userPath
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right (LocPath phrasePath) <- runClientM (insertPhraseC basicAuth
                    $ PhraseReq "test_phrase1") (clientEnv port)
                let phraseId = PhraseID . read . last . splitOn "/" $ phrasePath
                Right (LocPath path) <- runClientM (insertAlternativeC basicAuth phraseId
                    $ AlternativeReq "test_alternative1") (clientEnv port)
                let altId = AlternativeID . read . last . splitOn "/" $ path
                result <- runClientM (getAlternativeC basicAuth altId) (clientEnv port)
                case result of
                    Left err -> expectationFailure $ "Expected success but got error: " ++ show err
                    Right alt -> do
                        altText alt `shouldBe` T.pack "test_alternative1"
                        altAuthorId alt `shouldBe` authorId

        describe "GET /api/phrases/:id" $ do
            it "can increase the number of alternatives when inserting an alternative" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right (LocPath phrasePath) <- runClientM (insertPhraseC basicAuth
                    $ PhraseReq "test_phrase1") (clientEnv port)
                let phraseId = PhraseID . read . last . splitOn "/" $ phrasePath
                Right _ <- runClientM (insertAlternativeC basicAuth phraseId
                    $ AlternativeReq "test_alternative1") (clientEnv port)
                Right phrase <- runClientM (getPhraseC basicAuth phraseId) (clientEnv port)
                phraseNumAlts phrase `shouldBe` 1

        describe "GET /api/phrases/:id" $ do
            it "can't get a phrase that doesn't exist" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right locPaths <- runClientM (listPhrasesC basicAuth False) (clientEnv port)
                let existingIds = flip map (toList locPaths) $ \(LocPath path) ->
                        PhraseID . read . last . splitOn "/" $ path
                let phraseId = findMissing existingIds
                result <- runClientM (getPhraseC basicAuth phraseId) (clientEnv port)
                result `shouldSatisfy` isLeft

        describe "GET /api/phrases/:id/alternatives" $ do
            it "can list alternatives and find new alternatives among them" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right (LocPath phrasePath) <- runClientM (insertPhraseC basicAuth
                    $ PhraseReq "test_phrase1") (clientEnv port)
                let phraseId = PhraseID . read . last . splitOn "/" $ phrasePath
                Right locPath1 <- runClientM (insertAlternativeC basicAuth phraseId
                    $ AlternativeReq "test_alternative1") (clientEnv port)
                Right locPath2 <- runClientM (insertAlternativeC basicAuth phraseId
                    $ AlternativeReq "test_alternative2") (clientEnv port)
                result <- runClientM (listAlternativesByPhraseC basicAuth phraseId) (clientEnv port)
                case result of
                    Left err -> expectationFailure $ "Expected success but got error: " ++ show err
                    Right locPaths -> do
                        locPath1 `shouldSatisfy` (`elem` locPaths)
                        locPath2 `shouldSatisfy` (`elem` locPaths)
        
        describe "POST /api/phrase/:id/alternatives" $ do
            it "can't insert an alternative for a non-existent phrase" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right locPaths <- runClientM (listPhrasesC basicAuth False) (clientEnv port)
                let existingIds = flip map (toList locPaths) $ \(LocPath path) ->
                        PhraseID . read . last . splitOn "/" $ path
                let phraseId = findMissing existingIds
                result <- runClientM (insertAlternativeC basicAuth phraseId
                    $ AlternativeReq "test_alternative1") (clientEnv port)
                result `shouldSatisfy` isLeft
        
        describe "POST /api/phrase/:id/alternatives" $ do
            it "can insert an alternative for a phrase that belongs to another user" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                _ <- runClientM (registerC $ UserReq "test_user2" "test_pass2") (clientEnv port)
                let basicAuth1 = BasicAuthData "test_user1" "test_pass1"
                let basicAuth2 = BasicAuthData "test_user2" "test_pass2"
                Right (LocPath phrasePath) <- runClientM (insertPhraseC basicAuth1
                    $ PhraseReq "test_phrase1") (clientEnv port)
                let phraseId = PhraseID . read . last . splitOn "/" $ phrasePath
                result <- runClientM (insertAlternativeC basicAuth2 phraseId
                    $ AlternativeReq "test_alternative1") (clientEnv port)
                result `shouldSatisfy` isRight

        describe "POST /api/phrase/:id/alternatives" $ do
            it "can't insert an alternative with an empty text" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right (LocPath phrasePath) <- runClientM (insertPhraseC basicAuth
                    $ PhraseReq "test_phrase1") (clientEnv port)
                let phraseId = PhraseID . read . last . splitOn "/" $ phrasePath
                result <- runClientM (insertAlternativeC basicAuth phraseId
                    $ AlternativeReq "") (clientEnv port)
                result `shouldSatisfy` isLeft

        describe "GET /api/alternatives/:id" $ do
            it "can't get an alternative that doesn't exist" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right locPaths <- runClientM (listAlternativesC basicAuth Nothing) (clientEnv port)
                let existingIds = flip map (toList locPaths) $ \(LocPath path) ->
                        AlternativeID . read . last . splitOn "/" $ path
                let altId = findMissing existingIds
                result <- runClientM (getAlternativeC basicAuth altId) (clientEnv port)
                result `shouldSatisfy` isLeft

        describe "PATCH /api/alternatives/:id" $ do
            it "can choose an alternative and close a phrase" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right (LocPath phrasePath) <- runClientM (insertPhraseC basicAuth
                    $ PhraseReq "test_phrase1") (clientEnv port)
                let phraseId = PhraseID . read . last . splitOn "/" $ phrasePath
                Right (LocPath altPath) <- runClientM (insertAlternativeC basicAuth phraseId
                    $ AlternativeReq "test_alternative1") (clientEnv port)
                let altId = AlternativeID . read . last . splitOn "/" $ altPath
                Right (LocPath path) <- runClientM (chooseAlternativeC basicAuth altId) (clientEnv port)
                path `shouldSatisfy` matchRegex "^/api/phrases/[0-9]+$"
                Right phrase <- runClientM (getPhraseC basicAuth phraseId) (clientEnv port)
                phraseIsOpen phrase `shouldBe` False
                phraseChosenAltId phrase `shouldBe` Just altId

        describe "PATCH /api/alternatives/:id" $ do
            it "can't choose an alternative that doesn't exist" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right locPaths <- runClientM (listAlternativesC basicAuth Nothing) (clientEnv port)
                let existingIds = flip map (toList locPaths) $ \(LocPath path) ->
                        AlternativeID . read . last . splitOn "/" $ path
                let altId = findMissing existingIds
                result <- runClientM (chooseAlternativeC basicAuth altId) (clientEnv port)
                result `shouldSatisfy` isLeft
        
        describe "PATCH /api/alternatives/:id" $ do
            it "can't create a new alternative for a closed phrase" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right (LocPath phrasePath) <- runClientM (insertPhraseC basicAuth
                    $ PhraseReq "test_phrase1") (clientEnv port)
                let phraseId = PhraseID . read . last . splitOn "/" $ phrasePath
                Right (LocPath altPath) <- runClientM (insertAlternativeC basicAuth phraseId
                    $ AlternativeReq "test_alternative1") (clientEnv port)
                let altId = AlternativeID . read . last . splitOn "/" $ altPath
                _ <- runClientM (chooseAlternativeC basicAuth altId) (clientEnv port)
                result <- runClientM (insertAlternativeC basicAuth phraseId
                    $ AlternativeReq "test_alternative2") (clientEnv port)
                result `shouldSatisfy` isLeft
        
        describe "PATCH /api/alternatives/:id" $ do
            it "can't choose an alternative for a closed phrase" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                let basicAuth = BasicAuthData "test_user1" "test_pass1"
                Right (LocPath phrasePath) <- runClientM (insertPhraseC basicAuth
                    $ PhraseReq "test_phrase1") (clientEnv port)
                let phraseId = PhraseID . read . last . splitOn "/" $ phrasePath
                Right (LocPath altPath1) <- runClientM (insertAlternativeC basicAuth phraseId
                    $ AlternativeReq "test_alternative1") (clientEnv port)
                let altId1 = AlternativeID . read . last . splitOn "/" $ altPath1
                Right (LocPath altPath2) <- runClientM (insertAlternativeC basicAuth phraseId
                    $ AlternativeReq "test_alternative2") (clientEnv port)
                let altId2 = AlternativeID . read . last . splitOn "/" $ altPath2
                _ <- runClientM (chooseAlternativeC basicAuth altId1) (clientEnv port)
                result <- runClientM (chooseAlternativeC basicAuth altId2) (clientEnv port)
                result `shouldSatisfy` isLeft

        describe "PATCH /api/alternatives/:id" $ do
            it "can't choose an alternative for a another user's phrase" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                _ <- runClientM (registerC $ UserReq "test_user2" "test_pass2") (clientEnv port)
                let basicAuth1 = BasicAuthData "test_user1" "test_pass1"
                let basicAuth2 = BasicAuthData "test_user2" "test_pass2"
                Right (LocPath phrasePath) <- runClientM (insertPhraseC basicAuth1
                    $ PhraseReq "test_phrase1") (clientEnv port)
                let phraseId = PhraseID . read . last . splitOn "/" $ phrasePath
                Right (LocPath altPath) <- runClientM (insertAlternativeC basicAuth1 phraseId
                    $ AlternativeReq "test_alternative1") (clientEnv port)
                let altId = AlternativeID . read . last . splitOn "/" $ altPath
                result <- runClientM (chooseAlternativeC basicAuth2 altId) (clientEnv port)
                result `shouldSatisfy` isLeft
        
        describe "PATCH /api/alternatives/:id" $ do
            it "can choose another user's alternative for own phrase" $ \port -> do
                _ <- runClientM (registerC $ UserReq "test_user1" "test_pass1") (clientEnv port)
                _ <- runClientM (registerC $ UserReq "test_user2" "test_pass2") (clientEnv port)
                let basicAuth1 = BasicAuthData "test_user1" "test_pass1"
                let basicAuth2 = BasicAuthData "test_user2" "test_pass2"
                Right (LocPath phrasePath) <- runClientM (insertPhraseC basicAuth1
                    $ PhraseReq "test_phrase1") (clientEnv port)
                let phraseId = PhraseID . read . last . splitOn "/" $ phrasePath
                Right (LocPath altPath) <- runClientM (insertAlternativeC basicAuth2 phraseId
                    $ AlternativeReq "test_alternative1") (clientEnv port)
                let altId = AlternativeID . read . last . splitOn "/" $ altPath
                result <- runClientM (chooseAlternativeC basicAuth1 altId) (clientEnv port)
                result `shouldSatisfy` isRight

main :: IO ()
main = hspec $ do
    publicSpec
    businessLogicSpec

