
module Dojo.Data.Person.Base where
import Dojo.Framework
import Dojo.Base
import qualified Data.Time      as Time


-- | A Person known to the system.
data Person
        = Person
        { -- | We keep the system user id separate from the membership number
          --   so that we can add beginners that have not yet joined Aikikai.
          --
          --   The person will be Nothing if the record has not been added
          --   to the database yet.
          personId                      :: Maybe PersonId       -- PRIMARY KEY

          -- | Aikikai membership number.
        , personMemberId                :: PersonMemberId

          -- | If a person only has one name then use that as the "first name"
          --   and leave the others empty. We need at least one name to refer
          --   to them, but some people only have a single name.
        , personPreferredName           :: PersonName
        , personFirstName               :: PersonName           -- NOT NULL
        , personFamilyName              :: PersonName

        , personDateOfBirth             :: PersonDate
        , personPhoneMobile             :: PersonPhone
        , personPhoneFixed              :: PersonPhone
        , personEmail                   :: PersonEmail
        , personDojoHome                :: PersonDojo
        , personMembershipLevel         :: PersonMembershipLevel
        , personMembershipRenewal       :: PersonDate
        , personEmergencyName1          :: PersonName
        , personEmergencyPhone1         :: PersonPhone
        , personEmergencyName2          :: PersonName
        , personEmergencyPhone2         :: PersonPhone
        }
        deriving Show


data PersonId
        = PersonId Integer
        deriving (Show, Eq, Ord)

data PersonMemberId
        = PersonMemberId Integer
        deriving (Show, Eq)

data PersonName
        = PersonName String
        deriving (Show, Eq)

data PersonDate
        = PersonDate (Maybe Time.Day)
        deriving (Show, Eq)

data PersonPhone
        = PersonPhone String
        deriving (Show, Eq)

data PersonEmail
        = PersonEmail String
        deriving (Show, Eq)

data PersonDojo
        = PersonDojo String
        deriving (Show, Eq)

data PersonMembershipLevel
        = PersonMembershipLevel String
        deriving (Show, Eq)


-- toSql ----------------------------------------------------------------------
instance Convertible PersonId   SqlValue where
 safeConvert (PersonId n)               = safeConvert n

instance Convertible PersonMemberId SqlValue where
 safeConvert (PersonMemberId mid)       = safeConvert mid

instance Convertible PersonName  SqlValue where
 safeConvert (PersonName name)          = safeConvert name

instance Convertible PersonDate  SqlValue where
 safeConvert (PersonDate dob)           = safeConvert dob

instance Convertible PersonPhone SqlValue where
 safeConvert (PersonPhone mobile)       = safeConvert mobile

instance Convertible PersonEmail  SqlValue where
 safeConvert (PersonEmail  email)       = safeConvert email

instance Convertible PersonDojo SqlValue where
 safeConvert (PersonDojo mobile)        = safeConvert mobile

instance Convertible PersonMembershipLevel SqlValue where
 safeConvert (PersonMembershipLevel mobile) = safeConvert mobile


-- fromSql --------------------------------------------------------------------
instance Convertible SqlValue PersonId where
 safeConvert val        = liftM PersonId (safeConvert val)

instance Convertible SqlValue PersonMemberId where
 safeConvert val        = liftM PersonMemberId (safeConvert val)

instance Convertible SqlValue PersonName where
 safeConvert val        = liftM PersonName (safeConvert val)

instance Convertible SqlValue PersonDate where
 safeConvert val        = liftM PersonDate (safeConvert val)

instance Convertible SqlValue PersonPhone where
 safeConvert val        = liftM PersonPhone (safeConvert val)

instance Convertible SqlValue PersonEmail where
 safeConvert val        = liftM PersonEmail (safeConvert val)

instance Convertible SqlValue PersonDojo where
 safeConvert val        = liftM PersonDojo (safeConvert val)

instance Convertible SqlValue PersonMembershipLevel where
 safeConvert val        = liftM PersonMembershipLevel (safeConvert val)


-- Constructors ---------------------------------------------------------------
-- | Create a zero person with just the first name.
zeroPerson :: String -> Person
zeroPerson firstName
        = Person
        { personId                      = Nothing
        , personMemberId                = PersonMemberId 0
        , personPreferredName           = PersonName    ""
        , personFirstName               = PersonName firstName
        , personFamilyName              = PersonName    ""
        , personDateOfBirth             = PersonDate    Nothing
        , personPhoneMobile             = PersonPhone   ""
        , personPhoneFixed              = PersonPhone   ""
        , personEmail                   = PersonEmail   ""
        , personDojoHome                = PersonDojo    ""
        , personMembershipLevel         = PersonMembershipLevel ""
        , personMembershipRenewal       = PersonDate    Nothing
        , personEmergencyName1          = PersonName    ""
        , personEmergencyPhone1         = PersonPhone   ""
        , personEmergencyName2          = PersonName    ""
        , personEmergencyPhone2         = PersonPhone   ""
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
 =      [ Field "PersonId"              "id"
                (toSql . personId)

        , Field "MemberId"              "member id"
                (toSql . personMemberId)

        , Field "PreferredName"         "preferred name"
                (toSql . personPreferredName)

        , Field "FirstName"             "first name"
                (toSql . personFirstName)

        , Field "FamilyName"            "family name"
                (toSql . personFamilyName)

        , Field "DateOfBirth"           "date of birth"
                (toSql . personDateOfBirth)

        , Field "PhoneMobile"           "mobile phone number"
                (toSql . personPhoneMobile)

        , Field "PhoneFixed"            "fixed phone number"
                (toSql . personPhoneFixed)

        , Field "Email"                 "email address"
                (toSql . personEmail)

        , Field "DojoHome"              "home dojo"
                (toSql . personDojoHome)

        , Field "MembershipLevel"       "membership level"
                (toSql . personMembershipLevel)

        , Field "MembershipRenewal"     "membership renewal data"
                (toSql . personMembershipRenewal)

        , Field "EmergencyName1"        "emergency contact name 1"
                (toSql . personEmergencyName1)

        , Field "EmergencyPhone1"       "emergency contact phone 1"
                (toSql . personEmergencyPhone1)

        , Field "EmergencyName2"        "emergency contact name 2"
                (toSql . personEmergencyName2)

        , Field "EmergencyPhone2"       "emergency contact phone 2"
                (toSql . personEmergencyPhone2)
        ]


-- | Like `personFields`, but without the field for the primary key.
personFieldsNoKey :: [Field Person]
personFieldsNoKey
 = [pf | pf <- personFields
       , fieldNameTable pf /= entityKey personEntity ]


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
 = if sPreferred == "" then sFirst else sPreferred
 where
        PersonName sPreferred   = personPreferredName person
        PersonName sFirst       = personFirstName person


-- | Get the standard display name of a Person.
--   We use the prefered name if set, and ignore middle names.
personDisplayName :: Person -> String
personDisplayName person
 = first ++ " " ++ family
 where
        PersonName family   = personFamilyName person

        PersonName first
         | personPreferredName person == PersonName ""
         = personFirstName person

         | otherwise
         = personPreferredName person

