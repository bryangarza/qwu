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

accountDeckTable :: Table AccountPostTableW AccountPostTableR
accountDeckTable = Table "accountDeckTable" (p2 ( required "postId"
                                                , required "accountId"))
