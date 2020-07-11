
module Dojo.Data.Person.Base where
import Dojo.Framework
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
        , personFirstName               :: PersonName   -- NOT NULL
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
personEntity :: Entity Person
personEntity
        = Entity
        { entityTable   = "v1_Person"
        , entityKey     = "PersonId"
        , entityFields  = personFields }


personFields :: [Field Person]
personFields
 =      [ Field "PersonId"
                "id"

        , Field "MemberId"
                "member id"

        , Field "PreferredName"
                "preferred name"

        , Field "FirstName"
                "first name"

        , Field "FamilyName"
                "family name"

        , Field "DateOfBirth"
                "date of birth"

        , Field "PhoneMobile"
                "mobile phone number"

        , Field "PhoneFixed"
                "fixed phone number"

        , Field "Email"
                "email address"

        , Field "DojoHome"
                "home dojo"

        , Field "MembershipLevel"
                "membership level"

        , Field "MembershipRenewal"
                "membership renewal data"

        , Field "EmergencyName1"
                "emergency contact name 1"

        , Field "EmergencyPhone1"
                "emergency contact phone 1"

        , Field "EmergencyName2"
                "emergency contact name 2"

        , Field "EmergencyPhone2"
                "emergency contact phone 2"
        ]


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

