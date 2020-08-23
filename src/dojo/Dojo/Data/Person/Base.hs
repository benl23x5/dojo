
module Dojo.Data.Person.Base where
import Dojo.Framework
import Dojo.Trivia


-------------------------------------------------------------------------------
-- | A Person known to the system.
data Person
        = Person
        { -- | The person id will be Nothing if the record has not been
          --   added to the database yet, but there is a PRIMARY KEY
          --   constraint on the field in the database.
          personId                      :: Maybe PersonId       -- PRIMARY KEY

          -- | Organization membership id.
        , personMemberId                :: Maybe PersonMemberId

          -- | If a person only has one name then use that as the "first name"
          --   and leave the others empty. We need at least one name to refer
          --   to them, but some people only have a single name.
        , personPreferredName           :: Maybe PersonName
        , personFirstName               :: Maybe PersonName     -- NOT NULL
        , personFamilyName              :: Maybe PersonName

        , personDateOfBirth             :: Maybe PersonDate
        , personPhoneMobile             :: Maybe PersonPhone
        , personPhoneFixed              :: Maybe PersonPhone
        , personEmail                   :: Maybe PersonEmail
        , personDojoHome                :: Maybe PersonDojo
        , personMembershipLevel         :: Maybe PersonMembershipLevel
        , personMembershipRenewal       :: Maybe PersonDate
        , personEmergencyName1          :: Maybe PersonName
        , personEmergencyPhone1         :: Maybe PersonPhone
        , personEmergencyName2          :: Maybe PersonName
        , personEmergencyPhone2         :: Maybe PersonPhone
        }
        deriving Show


-- | Create a zero person with just the first name.
zeroPerson :: Person
zeroPerson
        = Person
        { personId                      = Nothing
        , personMemberId                = Nothing
        , personPreferredName           = Nothing
        , personFirstName               = Nothing
        , personFamilyName              = Nothing
        , personDateOfBirth             = Nothing
        , personPhoneMobile             = Nothing
        , personPhoneFixed              = Nothing
        , personEmail                   = Nothing
        , personDojoHome                = Nothing
        , personMembershipLevel         = Nothing
        , personMembershipRenewal       = Nothing
        , personEmergencyName1          = Nothing
        , personEmergencyPhone1         = Nothing
        , personEmergencyName2          = Nothing
        , personEmergencyPhone2         = Nothing
        }


-- Entity  --------------------------------------------------------------------
-- | Person entity.
personEntity :: Entity Person
personEntity
        = Entity
        { entityTable   = "v1_Person"
        , entityKey     = "PersonId"
        , entityFields  = personFields }


-- | Field definitions of the person entity.
personFields :: [Field Person]
personFields
 =  [ Field "PersonId"              "id"
        (\s -> fmap toSql $ (parse s :: Either ParseError PersonId) )
        (toSql . personId)
        (\v x -> x { personId = fromSql v})

    , Field "MemberId"              "member id"
        (fmap toSql . loadInput @PersonMemberId)
        (toSql . personMemberId)
        (\v x -> x { personMemberId = fromSql v})

    , Field "PreferredName"         "preferred name"
        (fmap toSql . loadInput @PersonName)
        (toSql . personPreferredName)
        (\v x -> x { personPreferredName = fromSql v})

    , Field "FirstName"             "first name"
        (fmap toSql . loadInput @PersonName)
        (toSql . personFirstName)
        (\v x -> x { personFirstName = fromSql v})

    , Field "FamilyName"            "family name"
        (fmap toSql . loadInput @PersonName)
        (toSql . personFamilyName)
        (\v x -> x { personFamilyName = fromSql v})

    , Field "DateOfBirth"           "date of birth"
        (fmap toSql . loadInput @PersonDate)
        (toSql . personDateOfBirth)
        (\v x -> x { personDateOfBirth = fromSql v})

    , Field "PhoneMobile"           "mobile phone number"
        (fmap toSql . loadInput @PersonPhone)
        (toSql . personPhoneMobile)
        (\v x -> x { personPhoneMobile = fromSql v})

    , Field "PhoneFixed"            "fixed phone number"
        (fmap toSql . loadInput @PersonPhone)
        (toSql . personPhoneFixed)
        (\v x -> x { personPhoneFixed = fromSql v})

    , Field "Email"                 "email address"
        (fmap toSql . loadInput @PersonEmail)
        (toSql . personEmail)
        (\v x -> x { personEmail = fromSql v})

    , Field "DojoHome"              "home dojo"
        (fmap toSql . loadInput @PersonDojo)
        (toSql . personDojoHome)
        (\v x -> x { personDojoHome = fromSql v})

    , Field "MembershipLevel"       "membership level"
        (fmap toSql . loadInput @PersonMembershipLevel)
        (toSql . personMembershipLevel)
        (\v x -> x { personMembershipLevel = fromSql v})

    , Field "MembershipRenewal"     "membership renewal data"
        (fmap toSql . loadInput @PersonDate)
        (toSql . personMembershipRenewal)
        (\v x -> x { personMembershipRenewal = fromSql v})

    , Field "EmergencyName1"        "emergency contact name 1"
        (fmap toSql . loadInput @PersonName)
        (toSql . personEmergencyName1)
        (\v x -> x { personEmergencyName1 = fromSql v})

    , Field "EmergencyPhone1"       "emergency contact phone 1"
        (fmap toSql . loadInput @PersonPhone)
        (toSql . personEmergencyPhone1)
        (\v x -> x { personEmergencyPhone1 = fromSql v})

    , Field "EmergencyName2"        "emergency contact name 2"
        (fmap toSql . loadInput @PersonName)
        (toSql . personEmergencyName2)
        (\v x -> x { personEmergencyName2 = fromSql v})

    , Field "EmergencyPhone2"       "emergency contact phone 2"
        (fmap toSql . loadInput @PersonPhone)
        (toSql . personEmergencyPhone2)
        (\v x -> x { personEmergencyPhone2 = fromSql v})
    ]



-- | Like `personFields`, but without the field for the primary key.
personFieldsNoKey :: [Field Person]
personFieldsNoKey
 = [pf | pf <- personFields
       , fieldNameTable pf /= entityKey personEntity ]


-- Constructors ---------------------------------------------------------------
-- | Construct a person from a list of Sql values for each field.
personOfSqlValues :: [SqlValue] -> Person
personOfSqlValues vs
 = foldl (\person (v, inj) -> inj v person) zeroPerson
 $ zip vs $ map fieldFromSql personFields


-- | Load differences to a person record specified in a query path.
loadPerson
        :: [(String, String)]   -- ^ Table field name and new value.
        -> Person               -- ^ Old person to be updated.
        -> Either [LoadError] Person
loadPerson = loadEntity personEntity


-- Projections ----------------------------------------------------------------
-- | Field names of the person structure.
--   These need to match the names in the v1_Person database table.
personFieldNames  :: [String]
personFieldNames
 = map fieldNameTable
 $ entityFields personEntity


-- | Get the short name of a Person,
--   which is a perferred name if we have one, otherwise the first name.
personShortName :: Person -> Maybe String
personShortName person
 | Just (PersonName sPreferred) <- personPreferredName person
 = Just sPreferred

 | Just (PersonName sFirst) <- personFirstName person
 = Just sFirst

 | otherwise
 = Nothing


-- | Extract a person's fee status relative to the given event date.
--   If we have a renewal date and the event date is not after it then 'ok'
--   If the event date is after the renewal date then 'expired'
--   If there is no renewal data information then 'unknown'.
personFeeStatus :: EventDate -> Person -> PersonFeeStatus
personFeeStatus (EventDate dayEvent) person
 | Just (PersonDate dayRenewal) <- personMembershipRenewal person
 = if dayEvent <= dayRenewal
        then PersonFeeStatus "ok"
        else PersonFeeStatus "expired"
 | otherwise
 = PersonFeeStatus "unknown"


-- | Get the standard display name of a Person.
--   We use the prefered name if set, and ignore middle names.
personDisplayName :: Person -> Maybe String
personDisplayName person
 = case personFamilyName person of
        Nothing
         -> personShortName person

        Just (PersonName sFamily)
         -> fmap (\s -> s ++ " " ++ sFamily)
         $  personShortName person


-- Comparisons  ---------------------------------------------------------------
-- | Get the table field names of fields that differ in two person records.
diffPerson :: Person -> Person -> [String]
diffPerson = diffEntity personEntity

