
module Dojo.Framework.Random where
import qualified System.Random                  as R
import qualified Crypto.Hash.MD5                as MD5
import qualified Data.ByteString.Char8          as BC
import qualified Data.ByteString.Base64.URL     as B64
import qualified Data.Text                      as T


-- | Generate a random device code based on the current time,
--   and a configured salt value.
newRandomDeviceCode :: String -> IO String
newRandomDeviceCode sSalt
 = do
        iRandom :: Integer <- R.randomIO
        let ssSource = show iRandom ++ sSalt

        -- Kick out these chars from the encoding as they
        --  are too hard to disambiguate if they need to
        --  be written down.
        let csTrash = ['-', '_', 'O', '0']

        -- Candidate string for the identifier.
        --  The MD5 hash is 16 bytes, so the base-64 encoded
        --  string will be (16 * 8 / 6) = 21 chars long.
        --  We will probably get at least 12 good chars.
        let sCandidate
                = take 8 $ filter (\c -> not $ elem c csTrash)
                $ T.unpack
                $ B64.encodeBase64
                $ MD5.hash
                $ BC.pack ssSource

        -- In the odd case the string does not contain enough
        -- good chars then bump the salt and try again.
        -- We're applying a crypto digest hash, so the new
        -- string is highly likely to be different.
        if length sCandidate < 8
         then newRandomDeviceCode (sSalt ++ "X")
         else return sCandidate

