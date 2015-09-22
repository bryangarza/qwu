CREATE TABLE "accountTable" (
       "accountId" UUID  PRIMARY KEY,
       "username"  TEXT  NOT NULL,
       "email"     TEXT  NOT NULL,
       "password"  TEXT  NOT NULL
);

CREATE TABLE "postTable" (
       "postId"    SERIAL      PRIMARY KEY,
       "body"      TEXT        NOT NULL,
       "ts"        TIMESTAMPTZ NOT NULL
       "accountId" UUID        REFERENCES "accountTable" ON DELETE CASCADE,
);


-- CREATE TABLE "accountPostTable" (
--        "postId"    INT  REFERENCES "postTable"    ON DELETE CASCADE,
--        "accountId" UUID REFERENCES "accountTable" ON DELETE CASCADE,
--        PRIMARY KEY ("postId", "accountId")
-- );
