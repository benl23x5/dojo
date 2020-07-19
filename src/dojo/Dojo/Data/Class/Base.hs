
module Dojo.Data.Class.Base where
import Dojo.Framework
import Dojo.Trivia


-------------------------------------------------------------------------------
-- | A reoccuring class.
data Class
        = Class
        { classId               :: ClassId
        , classType             :: EventType
        , classLocation         :: EventLocation
        , classDay              :: ClassDay
        , classTime             :: EventTime
        , classDateFirst        :: EventDate
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
        (\v x -> x { classId = fromSql v})

    , Field "Type"              "type"
        (fmap toSql . load @EventType)
        (toSql . classType)
        (\v x -> x { classType = fromSql v})

    , Field "Location"          "location"
        (fmap toSql . load @EventLocation)
        (toSql . classLocation)
        (\v x -> x { classLocation = fromSql v})

    , Field "Day"               "day"
        (fmap toSql . load @ClassDay)
        (toSql . classDay)
        (\v x -> x { classDay = fromSql v})

    , Field "Time"              "time"
        (fmap toSql . load @EventTime)
        (toSql . classTime)
        (\v x -> x { classTime = fromSql v})

    , Field "DateFirst"         "date first"
        (fmap toSql . load @EventDate)
        (toSql . classDateFirst)
        (\v x -> x { classDateFirst = fromSql v})

    , Field "DateFinal"         "date final"
        (fmap toSql . loadMaybe @EventDate)
        (toSql . classDateFinal)
        (\v x -> x { classDateFinal = fromSql v})
    ]


-- | Squash empty fields to null
load :: forall a. Parse a => String -> Either ParseError a
load ss
 = case parse @a ss of
        Left err        -> Left  err
        Right x         -> Right x


-- | Squash empty fields to null
--   TODO: squash all white fields to null as well.
loadMaybe :: forall a. Parse a => String -> Either ParseError (Maybe a)
loadMaybe ss
 | length ss == 0       = Right Nothing
 | otherwise
 = case parse @a ss of
        Left err        -> Left  err
        Right x         -> Right (Just x)
