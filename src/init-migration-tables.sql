CREATE SCHEMA IF NOT EXISTS pichunter;

CREATE TABLE IF NOT EXISTS migrations_tracker
(
	filename TEXT NOT NULL PRIMARY KEY,
	checksum TEXT NOT NULL,
	installed_successfully BOOLEAN NOT NULL DEFAULT FALSE
)
