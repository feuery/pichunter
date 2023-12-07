(defpackage pichunter.migration-list
  (:use :cl)
  (:import-from :pichunter.migrations
   :defmigration))

(in-package pichunter.migration-list)

;; Load this by C-c C-k, *compile-file-path* and *load-pathname* don't handle C-c C-cing single defmigrations

(defmigration #P"init-main-tables.sql")
(defmigration #P"user-media.sql")
(defmigration #P"users.sql")
(defmigration #P"groups.sql")

(defmigration #P"place_data.sql")

(defmigration #P"session.sql")

(defmigration #P"users-banned.sql")
