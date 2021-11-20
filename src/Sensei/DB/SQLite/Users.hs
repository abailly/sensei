{-# LANGUAGE OverloadedStrings #-}
module Sensei.DB.SQLite.Users where

import Sensei.DB.SQLite.Migration(Migration(..))

createUsers :: Migration
createUsers =
  Migration
    "createUsersTable"
    [ mconcat
        [ "create table if not exists users ",
          "( id integer primary key",
          ", uid text unique not null",
          ", user text unique not null",
          ", profile text not null",
          ");"
        ]
    ]
    "drop table users;"
