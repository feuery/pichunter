CREATE TABLE pichunter.county (
       code INT NOT NULL PRIMARY KEY,
       name TEXT NOT NULL
);

CREATE TABLE pichunter.municipality (
       code INT NOT NULL PRIMARY KEY,
       county_code INT NOT NULL,
       FOREIGN KEY(county_code) REFERENCES pichunter.county(code) ON DELETE CASCADE ON UPDATE CASCADE
);

ALTER TABLE pichunter.pictures
ADD COLUMN county_code INT NOT NULL;

ALTER TABLE pichunter.pictures
ADD CONSTRAINT county_fk FOREIGN KEY (county_code) REFERENCES pichunter.county ( code ) ON UPDATE CASCADE ON DELETE CASCADE;

