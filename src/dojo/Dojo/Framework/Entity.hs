
module Dojo.Framework.Entity where
import Dojo.Framework.Parse
import Dojo.Base

-------------------------------------------------------------------------------
-- | An entity in the data model,
--     backed by a single database table.
data Entity entity
        = Entity
        { -- | Name of the database table describing this entity.
          entityTable   :: String

          -- | Name of the field in the table which is the primary key.
        , entityKey     :: String

          -- | Fields in the table.
        , entityFields  :: [Field entity] }


-- | Describes a field of the given entity.
data Field entity
        = Field
        { -- | Name of the field in the database table.
          fieldNameTable        :: String

          -- | Human readable name to use in forms.
        , fieldNameForm         :: String

          -- | Parse a string to the SqlValue for this field.
        , fieldParse            :: String -> Either ParseError SqlValue

          -- | Project out the field as an SQL value.
        , fieldToSql            :: entity -> SqlValue

          -- | Update the correspondingx field in the entity.
        , fieldFromSql          :: SqlValue -> entity -> entity }


-------------------------------------------------------------------------------
-- | Get a list of all table field names for this entity.
tableFieldNamesOfEntity :: Entity e -> [String]
tableFieldNamesOfEntity e
 = map fieldNameTable $ entityFields e


-- | Get a list of all form field names for this entity.
formFieldNamesOfEntity :: Entity e -> [String]
formFieldNamesOfEntity e
 = map fieldNameForm $ entityFields e


-- | Produce the form field for a table field name.
formTableNameOfEntity :: Entity e -> String -> Maybe String
formTableNameOfEntity e sNameTable
 = case [fieldNameForm f
                | f <- entityFields e
                , fieldNameTable f == sNameTable ] of
    [fn] -> Just fn
    _    -> Nothing


-------------------------------------------------------------------------------
type LoadError = (String, String, ParseError)

-- TODO: collect multiple errors.
-- TODO: better unknown field error.
loadEntity
        :: Entity e             -- ^ Entity specification.
        -> [(String, String)]   -- ^ Table field names and new values
        -> e                    -- ^ Entity to load into.
        -> Either [LoadError] e

loadEntity _entity [] e
 = Right e

loadEntity entity ((sName, sValue) : rest) e
 = case find (\x -> sName == fieldNameTable x)
             (entityFields entity) of

        Nothing
         -> error $ "unknown field" ++ sName

        Just field
         -> case fieldParse field sValue of
                Right val -> loadEntity entity rest (fieldFromSql field val e)
                Left err  -> Left [(sName, sValue, err)]


-------------------------------------------------------------------------------
-- | Get the list of field names that are different between two entities.
diffEntity :: Entity entity -> entity -> entity -> [String]
diffEntity entity e1 e2
 = [ fieldNameTable field
        | field <- entityFields entity
        , fieldToSql field e1 /= fieldToSql field e2 ]


-------------------------------------------------------------------------------
-- | Yield a raw sql query
--    `SELECT fields,.. FROM tableName` for this entity,
--   for all fields.
sqlSelectAllFromEntity :: Entity e -> String
sqlSelectAllFromEntity e
 = unlines
 [ "SELECT " ++ (intercalate ", " $ tableFieldNamesOfEntity e)
 , "FROM "   ++ entityTable e ]


-- | Yield a raw sql statement
--    `INSERT INTO tableName (fields, ...) VALUES (?, ...)`
--   for all fields besides the primary key.
--   We assume the primary key is auto increment and will be added
--    by the database.
sqlInsertAllIntoEntity :: Entity e -> String
sqlInsertAllIntoEntity e
 = unlines
 [ "INSERT INTO " ++ entityTable e
 , "(" ++        (intercalate ", " $ fsNames) ++ ")"
 , "VALUES (" ++ (intercalate ", " $ replicate nNames "?") ++ ")" ]
 where
        fsNames = [ n | n <- tableFieldNamesOfEntity e
                      , n /= entityKey e]
        nNames  = length fsNames


-- | Yield a raw sql statement
--     `UPDATE tableName SET field=?, ... WHERE fieldKey = ?`
sqlUpdateAllOfEntity :: Entity e -> String
sqlUpdateAllOfEntity e
 = unlines
 [ "UPDATE " ++ entityTable e
 , "SET " ++   (intercalate ", " [n ++ "=?" | n <- fsNames])
 , "WHERE " ++ entityKey e ++ "=?" ]
 where
        fsNames = [ n | n <- tableFieldNamesOfEntity e
                      , n /= entityKey e]

