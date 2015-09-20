CREATE TABLE "postTable" (
       "postId" SERIAL    PRIMARY KEY,
       "body"   TEXT      NOT NULL,
       "ts"     TIMESTAMP NOT NULL
);

CREATE TABLE "accountTable" (
       "accountId" UUID  PRIMARY KEY,
       "username"  TEXT  NOT NULL,
       "email"     TEXT  NOT NULL,
       "password"  TEXT  NOT NULL
);

CREATE TABLE "accountPostTable" (
       "postId"    INT  REFERENCES "postTable"    ON DELETE CASCADE,
       "accountId" UUID REFERENCES "accountTable" ON DELETE CASCADE,
       PRIMARY KEY ("postId", "accountId")
);
