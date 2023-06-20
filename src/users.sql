CREATE TABLE pichunter.users
(
  id SERIAL,
  username VARCHAR(100) NOT NULL UNIQUE,
  password CHAR(128) NOT NULL, 	--SHA-512
  display_name VARCHAR(1000) NOT NULL DEFAULT '',
  img_id uuid NULL,
  FOREIGN KEY (img_id) REFERENCES pichunter.pictures(id)
  ON UPDATE CASCADE
  ON DELETE SET NULL,
  PRIMARY KEY(ID));
