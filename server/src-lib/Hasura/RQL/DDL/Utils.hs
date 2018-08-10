{-# LANGUAGE OverloadedStrings #-}

module Hasura.RQL.DDL.Utils where

import qualified Database.PG.Query as Q

clearHdbViews :: Q.Query
clearHdbViews =
  "DO $$ DECLARE \
   \ r RECORD; \
   \ BEGIN \
   \   FOR r IN (select routine_name from information_schema.routines where routine_schema = 'hdb_views') LOOP \
   \     EXECUTE 'DROP FUNCTION hdb_views.' || quote_ident(r.routine_name) || '() CASCADE'; \
   \   END LOOP; \
   \ END $$ "
