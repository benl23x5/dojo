
module Dojo.Framework.Path where
import Dojo.Base
import qualified Text.Blaze as H

-- | A Path to a node.
data Path
        = Path
        { -- | User friendly name for the node.
          pathName      :: String

          -- | Base name of CGI file. eg 'Main.cgi'
        , pathBase      :: String

          -- | CGI key value pairs for this path.
        , pathFields    :: [(String, String)] }
        deriving Show

instance H.ToValue Path where
 toValue path = H.toValue (flatten path)


flatten  :: Path -> String
flatten path
        =  pathBase path
        ++ "?"
        ++ intercalate "&"
                [field ++ "=" ++ value
                        | (field, value) <- pathFields path ]


pathExtend :: Path -> [(String, String)] -> Path
pathExtend path fields
        = path { pathFields = pathFields path ++ fields }


(<&>) = pathExtend
