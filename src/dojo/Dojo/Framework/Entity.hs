
module Dojo.Framework.Entity where
import Data.List

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
        , fieldNameForm         :: String }


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
--   We assume the primary key is auto increment and will be added by the database.
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

