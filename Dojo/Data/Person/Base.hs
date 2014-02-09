
module Dojo.Data.Person.Base
        ( Person                (..)
        , PersonId              (..)
        , PersonMemberId        (..)
        , PersonName            (..)
        , PersonDateOfBirth     (..)
        , PersonMobile          (..)
        , PersonEmail           (..)
        , zeroPerson
        , personFieldNames
        , personDisplayName)
where
import qualified Data.Time      as Time


-- | A Person known to the system.
data Person
        = Person
        { -- | We keep the system user id separate from the membership number
          --   so that we can add beginners that have not yet joined Aikikai.
          personId              :: PersonId     -- PRIMARY KEY

          -- | Aikikai membership number.
        , personMemberId        :: PersonMemberId 

          -- | Prefered, short name.
        , personPreferedName    :: PersonName

          -- | If a person only has one name then use that as the "first name"
          --   and leave the others empty.
        , personFirstName       :: PersonName   -- NOT NULL

          -- | Other space separated names.
        , personMiddleName      :: PersonName

          -- | Family name.
        , personFamilyName      :: PersonName

          -- | Date of birth.
        , personDateOfBirth     :: PersonDateOfBirth

          -- | Mobile number.
        , personMobile          :: PersonMobile

          -- | Email address.
        , personEmail           :: PersonEmail 

          -- | Creation time.
        , personCreateTime      :: PersonCreateTime

          -- | Creation date.
        , personCreateDate      :: PersonCreateDate }


data PersonId           
        = PersonId Integer
        deriving (Eq, Ord)

data PersonMemberId
        = PersonMemberId Integer
        deriving Eq

data PersonName         
        = PersonName String         
        deriving Eq

data PersonDateOfBirth  
        = PersonDateOfBirth (Maybe Time.Day)
        deriving Eq

data PersonMobile
        = PersonMobile String
        deriving Eq

data PersonEmail
        = PersonEmail String
        deriving Eq


-- Constructors ---------------------------------------------------------------
-- | Create a zero person with just the first name.
zeroPerson :: String -> Person
zeroPerson firstName
        = Person
        { personId              = PersonId 0
        , personMemberId        = PersonMemberId 0
        , personPreferedName    = PersonName    ""
        , personFirstName       = PersonName firstName
        , personMiddleName      = PersonName    ""
        , personFamilyName      = PersonName    ""
        , personDateOfBirth     = PersonDateOfBirth Nothing
        , personMobile          = PersonMobile  ""
        , personEmail           = PersonEmail   "" }


-- Projections ----------------------------------------------------------------
-- | Field names of the person structure.
personFieldNames  :: [String]
personFieldNames
 =      [ "PersonId"
        , "MemberId"
        , "PreferedName"
        , "FirstName"
        , "MiddleName"
        , "FamilyName"
        , "DateOfBirth"
        , "Mobile"
        , "Email" ]


-- | Get the standard display name of a Person.
--   We use the prefered name if set, and ignore middle names.
personDisplayName :: Person -> String
personDisplayName person
 = first ++ " " ++ family
 where  
        PersonName family   = personFamilyName person

        PersonName first   
         | personPreferedName person == PersonName ""
         = personFirstName person

         | otherwise
         = personPreferedName person

