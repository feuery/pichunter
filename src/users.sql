CREATE TABLE pichunter.user
(
  id SERIAL,
  username VARCHAR(100) NOT NULL UNIQUE,
  password CHAR(128) NOT NULL, 	--SHA-512
  display_name VARCHAR(1000) NOT NULL DEFAULT '',
  img_id uuid NULL,
  activated BOOLEAN NOT NULL DEFAULT FALSE,  
  FOREIGN KEY (img_id) REFERENCES pichunter.pictures(id)
  ON UPDATE CASCADE
  ON DELETE SET NULL,
  PRIMARY KEY(ID));
