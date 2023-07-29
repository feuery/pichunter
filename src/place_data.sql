CREATE TABLE county (
       code INT NOT NULL PRIMARY KEY,
       name TEXT NOT NULL
);

CREATE TABLE municipality (
       code INT NOT NULL PRIMARY KEY,
       county_code INT NOT NULL,
       FOREIGN KEY(county_code) REFERENCES county(code) ON DELETE CASCADE ON UPDATE CASCADE
);

ALTER TABLE pictures
ADD COLUMN county_code INT NOT NULL;

ALTER TABLE pictures
ADD CONSTRAINT county_fk FOREIGN KEY (county_code) REFERENCES county ( code ) ON UPDATE CASCADE ON DELETE CASCADE;

