
module Dojo.Data.Class.Base where
import Dojo.Framework
import Dojo.Trivia
import qualified Data.Char as Char

-------------------------------------------------------------------------------
-- | A reoccuring class.
data Class
        = Class
        { classId               :: Maybe ClassId
        , classOwnerUserName    :: Maybe UserName
        , classType             :: Maybe EventType
        , classLocation         :: Maybe EventLocation
        , classDay              :: Maybe ClassDay
        , classTimeStart        :: Maybe EventTime
        , classTimeEnd          :: Maybe EventTime
        , classDateFirst        :: Maybe EventDate
        , classDateFinal        :: Maybe EventDate }
        deriving Show


-------------------------------------------------------------------------------
-- | Class entity.
classEntity :: Entity Class
classEntity
        = Entity
        { entityTable   = "v1_Class"
        , entityKey     = "ClassId"
        , entityFields  = classFields }


-- | Field definitions of the class entity.
classFields :: [Field Class]
classFields
 =  [ Field "ClassId"           "id"
        (fmap toSql . load @ClassId)
        (toSql . classId)
        (fmap toValue . classId)
        (\v x -> x { classId = fromSql v})

    , Field "OwnerUserName"     "owner user name"
        (fmap toSql . load @UserName)
        (toSql . classOwnerUserName)
        (fmap toValue . classOwnerUserName)
        (\v x -> x { classOwnerUserName = fromSql v})

    , Field "Type"              "type"
        (fmap toSql . load @EventType)
        (toSql . classType)
        (fmap toValue . classType)
        (\v x -> x { classType = fromSql v})

    , Field "Location"          "location"
        (fmap toSql . load @EventLocation)
        (toSql . classLocation)
        (fmap toValue . classLocation)
        (\v x -> x { classLocation = fromSql v})

    , Field "Day"               "day"
        (fmap toSql . load @ClassDay)
        (toSql . classDay)
        (fmap toValue . classDay)
        (\v x -> x { classDay = fromSql v})

    , Field "TimeStart"         "time start"
        (fmap toSql . load @EventTime)
        (toSql . classTimeStart)
        (fmap toValue . classTimeStart)
        (\v x -> x { classTimeStart = fromSql v})

    , Field "TimeEnd"           "time end"
        (fmap toSql . load @EventTime)
        (toSql . classTimeEnd)
        (fmap toValue . classTimeEnd)
        (\v x -> x { classTimeEnd = fromSql v})

    , Field "DateFirst"         "date first"
        (fmap toSql . load @EventDate)
        (toSql . classDateFirst)
        (fmap toValue . classDateFirst)
        (\v x -> x { classDateFirst = fromSql v})

    , Field "DateFinal"         "date final"
        (fmap toSql . load @EventDate)
        (toSql . classDateFinal)
        (fmap toValue . classDateFinal)
        (\v x -> x { classDateFinal = fromSql v})
    ]


-- | Zero valued class with all fields set to 'Nothing'.
zeroClass :: Class
zeroClass
        = Class
        { classId               = Nothing
        , classOwnerUserName    = Nothing
        , classType             = Nothing
        , classLocation         = Nothing
        , classDay              = Nothing
        , classTimeStart        = Nothing
        , classTimeEnd          = Nothing
        , classDateFirst        = Nothing
        , classDateFinal        = Nothing }


-- | Squash empty fields to null
--   TODO: squash all white fields to null as well.
load :: forall a. Parse a => String -> Either ParseError (Maybe a)
load ss
 | length ss == 0       = Right Nothing
 | otherwise
 = case parse @a ss of
        Left err        -> Left  err
        Right x         -> Right (Just x)


-- Constructors ---------------------------------------------------------------
-- | Construct a class from a list of Sql values for each field.
classOfSqlValues :: [SqlValue] -> Class
classOfSqlValues vs
 = foldl (\classs (v, inj) -> inj v classs) zeroClass
 $ zip vs $ map fieldFromSql classFields


-- Presentation --------------------------------------------------------------
-- | Take the display name of an event.
classDisplayName :: Class -> String
classDisplayName classs
 =  (fromMaybe "class" (fmap pretty $ classLocation classs))
 ++ (case classType classs of
        Nothing -> ""
        Just t  -> " " ++ pretty t)
 ++ (case classDay classs of
        Nothing -> ""
        Just d  -> " on " ++ pretty d)
 ++ (case classTimeStart classs of
        Nothing -> ""
        Just t  -> " at " ++ pretty t)


-- | Base file name to use if the class registration QC code is downloaded.
--   This is name of the file that ends up on the client's machine.
classQRCodeDownloadName :: Class -> String
classQRCodeDownloadName classs
 = let
        sLocation  = fromMaybe "Location"
                   $ fmap pretty $ classLocation classs

        sDay       = fromMaybe "Day"
                   $ fmap pretty $ classDay classs

        sTime      = filter (not . (== ':'))
                   $ fromMaybe "Time"
                   $ fmap pretty $ classTimeStart classs

   in   filter (not . Char.isSpace)
         $ "qr-class-" ++ sLocation ++ "-" ++ sDay ++ "-" ++ sTime


