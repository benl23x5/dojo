
module Dojo.Data.Class.Registration where
import Dojo.Data.Class.Base
import Dojo.Trivia

import qualified Data.ByteString.Base64.URL     as B64
import qualified Data.ByteString.Char8          as BC
import qualified Crypto.Hash.MD5                as MD5
import qualified Data.Text                      as T


registrationLinkOfClass
 :: String       -- ^ Base web server name, with "http://" prefix.
 -> String       -- ^ Salt for the generated registration code.
 -> Class        -- ^ Class details
 -> Maybe (String, String)

registrationLinkOfClass sUrl sSalt classs
 | Just sHash   <- registrationHashOfClass sUrl sSalt classs
 = Just (sUrl ++ "?r=" ++ sHash, sHash)

 | otherwise
 = Nothing


registrationHashOfClass
 :: String       -- ^ Base web server name.
 -> String       -- ^ Salt for the generated registration code.
 -> Class        -- ^ Class details.
 -> Maybe String

registrationHashOfClass sUrl sSalt classs
 | Just (EventLocation sLocation) <- classLocation classs
 , Just (ClassDay      sDay)      <- classDay classs
 , Just (EventTime     todStart)  <- classTimeStart classs
 , Just (EventType     sType)     <- classType classs
 = let
        -- Build the source string.
        --  Changing any of these values will change
        --  the resulting hash.
        ssSource
         =  "|url:"   ++ sUrl
         ++ "|salt:"  ++ sSalt
         ++ "|loc:"   ++ sLocation
         ++ "|day:"   ++ sDay
         ++ "|start:" ++ show todStart
         ++ "|type:"  ++ sType

        -- Kick out these chars from the encoding as they
        --  are too hard to disambiguate if they need to
        --  be written down.
        csTrash = ['-', '_', 'O', '0']

        -- Candidate string for the identifier.
        --  The MD5 hash is 16 bytes, so the base-64 encoded
        --  string will be (16 * 8 / 6) chars long.
        --  We will probably get at least 8 good chars.
        sCandidate
         = take 8 $ filter (\c -> not $ elem c csTrash)
         $ T.unpack
         $ B64.encodeBase64
         $ MD5.hash
         $ BC.pack ssSource

        -- In the odd case the string does not contain enough
        -- good chars then bump the salt and try again.
        -- We're applying a crypto digest hash, so the new
        -- string is highly likely to be different.
    in  if length sCandidate < 8
         then registrationHashOfClass sUrl (sSalt ++ "X") classs
         else Just sCandidate

 | otherwise
 = Nothing