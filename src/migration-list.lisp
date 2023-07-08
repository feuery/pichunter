(defpackage pichunter.migration-list
  (:use :cl)
  (:import-from :pichunter.migrations
   :defmigration))

(in-package pichunter.migration-list)

(defmigration #P"init-main-tables.sql")
(defmigration #P"users.sql")
(defmigration #P"groups.sql")
