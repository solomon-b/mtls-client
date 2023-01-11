{-# LANGUAGE OverloadedStrings #-}

module Main where

--------------------------------------------------------------------------------

import Data.Default (Default (..))
import Data.X509 (CertificateChain (..))
import Data.X509.File (readKeyFile, readSignedObject)
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client (Response (..), httpLbs, parseRequest)
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManagerWith)
import Network.HTTP.Types.Status (statusCode)
import Network.TLS (ClientHooks (..), ClientParams (..), Shared (..), Supported (..))
import Network.TLS.Extra (ciphersuite_default)
import System.X509 (getSystemCertificateStore)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  store <- getSystemCertificateStore
  chain <- readSignedObject "badssl.com-client.pem"
  privKey <- head <$> readKeyFile "badssl.com-client.pem"

  let tlsSettings =
        TLSSettings $
          ClientParams
            { clientUseMaxFragmentLength = Nothing,
              clientServerIdentification = ("client.badssl.com", ""),
              clientUseServerNameIndication = True,
              clientWantSessionResume = def,
              clientShared = def {sharedCAStore = store},
              clientHooks =
                def
                  { onCertificateRequest = \_ -> pure $ Just (CertificateChain chain, privKey)
                  },
              clientSupported = def {supportedCiphers = ciphersuite_default},
              clientDebug = def,
              clientEarlyData = def
            }

  let managerSettings = mkManagerSettings tlsSettings Nothing
  manager <- newTlsManagerWith managerSettings

  initialRequest <- parseRequest "https://client.badssl.com/"
  let request = initialRequest

  response <- httpLbs request manager
  print $ "The status code was: " ++ show (statusCode $ responseStatus response)
  print $ responseBody response
