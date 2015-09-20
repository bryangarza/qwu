module DB.Table.Relationship where

import Opaleye ( Column
               , Table(Table)
               , required
               , PGInt4
               , PGText
               , PGUuid )

import Data.Profunctor.Product (p2)

type AccountPostTableW = (Column PGInt4, Column PGUuid)
type AccountPostTableR = (Column PGInt4, Column PGUuid)

accountPostTable :: Table AccountPostTableW AccountPostTableR
accountPostTable = Table "accountPostTable" (p2 ( required "postId"
                                                , required "accountId"))
