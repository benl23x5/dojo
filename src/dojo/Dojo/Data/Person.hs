
module Dojo.Data.Person
        ( Person                (..)
        , PersonId              (..)
        , PersonMemberId        (..)
        , PersonName            (..)
        , PersonDate            (..)
        , PersonPhone           (..)
        , PersonEmail           (..)
        , PersonMembershipLevel (..)

        -- * Constructors
        , zeroPerson

        -- * Projections
        , personFieldNames
        , personShortName
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
        [ comp "PersonId"               personId
        , comp "MemberId"               personMemberId
        , comp "PreferredName"          personPreferredName
        , comp "FirstName"              personFirstName
        , comp "FamilyName"             personFamilyName
        , comp "DateOfBirth"            personDateOfBirth
        , comp "PhoneMobile"            personPhoneMobile
        , comp "PhoneFixed"             personPhoneFixed
        , comp "Email"                  personEmail
        , comp "DojoHome"               personDojoHome
        , comp "MembershipLevel"        personMembershipLevel
        , comp "MembershipRenewal"      personMembershipRenewal
        , comp "EmergencyName1"         personEmergencyName1
        , comp "EmergencyPhone1"        personEmergencyPhone1
        , comp "EmergencyName2"         personEmergencyName2
        , comp "EmergencyPhone2"        personEmergencyPhone2
        ]

 where  comp str f
         = if f p1 == f p2 then [] else [str]


-- Loading --------------------------------------------------------------------
-- TODO: collect multiple errors.
-- | Load differences to a person record specified in the query path.
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
        mid     <- load "MemberId"              (personMemberId     person)
        pname   <- load "PreferredName"         (personPreferredName person)
        fname   <- load "FirstName"             (personFirstName    person)
        lname   <- load "FamilyName"            (personFamilyName   person)
        dob     <- load "DateOfBirth"           (personDateOfBirth  person)
        mobile  <- load "PhoneMobile"           (personPhoneMobile  person)
        fixed   <- load "PhoneFixed"            (personPhoneFixed   person)
        email   <- load "Email"                 (personEmail        person)
        dojo    <- load "DojoHome"              (personDojoHome     person)
        level   <- load "MembershipLevel"       (personMembershipLevel person)
        renewal <- load "MembershipRenewal"     (personMembershipRenewal person)
        ename1  <- load "EmergencyName1"        (personEmergencyName1 person)
        ephone1 <- load "EmergencyPhone1"       (personEmergencyPhone1 person)
        ename2  <- load "EmergencyName2"        (personEmergencyName1 person)
        ephone2 <- load "EmergencyPhone2"       (personEmergencyPhone1 person)

        return  $ person
                { personMemberId                = mid
                , personPreferredName           = pname
                , personFirstName               = fname
                , personFamilyName              = lname
                , personDateOfBirth             = dob
                , personPhoneMobile             = mobile
                , personPhoneFixed              = fixed
                , personEmail                   = email
                , personDojoHome                = dojo
                , personMembershipLevel         = level
                , personMembershipRenewal       = renewal
                , personEmergencyName1          = ename1
                , personEmergencyPhone1         = ephone1
                , personEmergencyName2          = ename2
                , personEmergencyPhone2         = ephone2
                }


