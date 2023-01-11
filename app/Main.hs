{-# LANGUAGE OverloadedStrings #-}

module Main where

--------------------------------------------------------------------------------

import Data.Default (Default (..))
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client (Response (..), httpLbs, parseRequest)
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManagerWith)
import Network.HTTP.Types.Status (statusCode)
import Network.TLS (ClientParams (..), Shared (..), Supported (..))
import Network.TLS.Extra (ciphersuite_strong)
import System.X509 (getSystemCertificateStore)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  store <- getSystemCertificateStore

  let tlsSettings =
        TLSSettings $
          ClientParams
            { clientUseMaxFragmentLength = Nothing,
              clientServerIdentification = ("httpbin.org", ""),
              clientUseServerNameIndication = True,
              clientWantSessionResume = def,
              clientShared = def {sharedCAStore = store},
              clientHooks = def,
              clientSupported = def {supportedCiphers = ciphersuite_strong},
              clientDebug = def,
              clientEarlyData = def
            }

  let managerSettings = mkManagerSettings tlsSettings Nothing
  manager <- newTlsManagerWith managerSettings

  initialRequest <- parseRequest "https://httpbin.org/get" -- "https://client.badssl.com/"
  let request = initialRequest

  response <- httpLbs request manager
  print $ "The status code was: " ++ show (statusCode $ responseStatus response)
  print $ responseBody response
