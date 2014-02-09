
module Dojo.Data.Person 
        ( Person                (..)
        , PersonId              (..)
        , PersonMemberId        (..)
        , PersonName            (..)
        , PersonDateOfBirth     (..)
        , PersonMobile          (..)
        , PersonEmail           (..)

        -- * Constructors
        , zeroPerson

        -- * Projections
        , personFieldNames
        , personDisplayName

        -- * Database
        , personOfSqlValues
        , getPeople
        , getPerson
        , insertPerson
        , updatePerson

        -- * Search
        , Found(..)
        , findPerson

        -- * Presentation
        , niceNameOfPersonField

        -- * Operators
        , diffPerson
        , loadPerson)
where
import Dojo.Data.Person.Search
import Dojo.Data.Person.Presentation
import Dojo.Data.Person.Database
import Dojo.Data.Person.Base
import Dojo.Framework


-- Diff -----------------------------------------------------------------------
-- | Get the list of field names that are different in these two people.
diffPerson :: Person -> Person -> [String]
diffPerson p1 p2
 = concat
        [ comp "PersonId"       personId
        , comp "MemberId"       personMemberId
        , comp "PreferedName"   personPreferedName 
        , comp "FirstName"      personFirstName
        , comp "MiddleName"     personMiddleName
        , comp "FamilyName"     personFamilyName
        , comp "DateOfBirth"    personDateOfBirth 
        , comp "Mobile"         personMobile
        , comp "Email"          personEmail ]

 where  comp str f
         = if f p1 == f p2 then [] else [str]


-- Loading --------------------------------------------------------------------
loadPerson
        :: [(String, String)] 
        -> Person
        -> Either [(String, String, ParseError)] Person

loadPerson inputs person
 = let load name def
         = case lookup name inputs of
                Nothing  -> Right def
                Just str -> case parse str of
                                Right val       -> Right val
                                Left err        -> Left [(name, str, err)]
   in do
        mid     <- load "MemberId"      (personMemberId     person)
        pname   <- load "PreferedName"  (personPreferedName person)
        fname   <- load "FirstName"     (personFirstName    person)
        mname   <- load "MiddleName"    (personMiddleName   person)
        lname   <- load "FamilyName"    (personFamilyName   person)
        dob     <- load "DateOfBirth"   (personDateOfBirth  person)
        mobile  <- load "Mobile"        (personMobile       person)
        email   <- load "Email"         (personEmail        person)

        return  $ person
                { personMemberId        = mid
                , personPreferedName    = pname
                , personFirstName       = fname
                , personMiddleName      = mname
                , personFamilyName      = lname
                , personDateOfBirth     = dob
                , personMobile          = mobile
                , personEmail           = email }

