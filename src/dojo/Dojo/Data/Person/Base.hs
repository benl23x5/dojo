
module Dojo.Data.Person.Base where
import Dojo.Framework
import Dojo.Trivia


-------------------------------------------------------------------------------
-- | A Person known to the system.
data Person
        = Person
        { -- | The person id will be Nothing if the record has not been
          --   added to the database yet, but there is a NOT NULL/PRIMARY KEY
          --   constraint on the field in the database.
          personId                      :: Maybe PersonId       -- PRIMARY KEY

          -- | Organization membership id.
        , personMemberId                :: Maybe PersonMemberId

          -- | If a person only has one name then use that as the "first name"
          --   and leave the others empty. We need at least one name to refer
          --   to them, but some people only have a single name.
        , personPreferredName           :: Maybe PersonName
        , personFirstName               :: PersonName           -- NOT NULL
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
zeroPerson :: String -> Person
zeroPerson firstName
        = Person
        { personId                      = Nothing
        , personMemberId                = Nothing
        , personPreferredName           = Nothing
        , personFirstName               = PersonName firstName
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
        (\s -> fmap toSql $ (parse s :: Either ParseError PersonMemberId) )
        (toSql . personMemberId)
        (\v x -> x { personMemberId = fromSql v})

    , Field "PreferredName"         "preferred name"
        (\s -> fmap toSql $ (parse s :: Either ParseError PersonName) )
        (toSql . personPreferredName)
        (\v x -> x { personPreferredName = fromSql v})

    , Field "FirstName"             "first name"
        (\s -> fmap toSql $ (parse s :: Either ParseError PersonName) )
        (toSql . personFirstName)
        (\v x -> x { personFirstName = fromSql v})

    , Field "FamilyName"            "family name"
        (\s -> fmap toSql $ (parse s :: Either ParseError PersonName) )
        (toSql . personFamilyName)
        (\v x -> x { personFamilyName = fromSql v})

    , Field "DateOfBirth"           "date of birth"
        (\s -> fmap toSql $ (parse s :: Either ParseError PersonDate) )
        (toSql . personDateOfBirth)
        (\v x -> x { personDateOfBirth = fromSql v})

    , Field "PhoneMobile"           "mobile phone number"
        (\s -> fmap toSql $ (parse s :: Either ParseError PersonPhone) )
        (toSql . personPhoneMobile)
        (\v x -> x { personPhoneMobile = fromSql v})

    , Field "PhoneFixed"            "fixed phone number"
        (\s -> fmap toSql $ (parse s :: Either ParseError PersonPhone) )
        (toSql . personPhoneFixed)
        (\v x -> x { personPhoneFixed = fromSql v})

    , Field "Email"                 "email address"
        (\s -> fmap toSql $ (parse s :: Either ParseError PersonEmail) )
        (toSql . personEmail)
        (\v x -> x { personEmail = fromSql v})

    , Field "DojoHome"              "home dojo"
        (\s -> fmap toSql $ (parse s :: Either ParseError PersonDojo) )
        (toSql . personDojoHome)
        (\v x -> x { personDojoHome = fromSql v})

    , Field "MembershipLevel"       "membership level"
        (\s -> fmap toSql $ (parse s :: Either ParseError PersonMembershipLevel) )
        (toSql . personMembershipLevel)
        (\v x -> x { personMembershipLevel = fromSql v})

    , Field "MembershipRenewal"     "membership renewal data"
        (\s -> fmap toSql $ (parse s :: Either ParseError PersonDate) )
        (toSql . personMembershipRenewal)
        (\v x -> x { personMembershipRenewal = fromSql v})

    , Field "EmergencyName1"        "emergency contact name 1"
        (\s -> fmap toSql $ (parse s :: Either ParseError PersonName) )
        (toSql . personEmergencyName1)
        (\v x -> x { personEmergencyName1 = fromSql v})

    , Field "EmergencyPhone1"       "emergency contact phone 1"
        (\s -> fmap toSql $ (parse s :: Either ParseError PersonPhone) )
        (toSql . personEmergencyPhone1)
        (\v x -> x { personEmergencyPhone1 = fromSql v})

    , Field "EmergencyName2"        "emergency contact name 2"
        (\s -> fmap toSql $ (parse s :: Either ParseError PersonName) )
        (toSql . personEmergencyName2)
        (\v x -> x { personEmergencyName2 = fromSql v})

    , Field "EmergencyPhone2"       "emergency contact phone 2"
        (\s -> fmap toSql $ (parse s :: Either ParseError PersonPhone) )
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
 = foldl (\person (v, inj) -> inj v person) (zeroPerson "")     -- TODO: avoid empty name init
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
personShortName :: Person -> String
personShortName person
 | Just (PersonName sPreferred) <- personPreferredName person
 = sPreferred

 | PersonName sFirst <- personFirstName person
 = sFirst


-- | Get the standard display name of a Person.
--   We use the prefered name if set, and ignore middle names.
personDisplayName :: Person -> String
personDisplayName person
 = case personFamilyName person of
        Nothing
         -> personShortName person

        Just (PersonName sFamily)
         -> personShortName person ++ " " ++ sFamily


-- Comparisons  ---------------------------------------------------------------
-- | Get the table field names of fields that differ in two person records.
diffPerson :: Person -> Person -> [String]
diffPerson = diffEntity personEntity

