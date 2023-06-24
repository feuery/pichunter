CREATE TABLE IF NOT EXISTS pichunter.usergroup (
       ID SERIAL,
       Name VARCHAR NOT NULL UNIQUE,
       Description VARCHAR(2000) NOT NULL DEFAULT '',
       Img_location VARCHAR(2000) NOT NULL DEFAULT '',
       PRIMARY KEY (ID));

INSERT INTO pichunter.usergroup(Name, Description) VALUES ('Admins', 'Group for admins');
INSERT INTO pichunter.usergroup(Name, Description) VALUES ('Users', 'Group for ordinary mortals');

CREATE TABLE IF NOT EXISTS pichunter.groupmapping (
       UserID INT NOT NULL,
       GroupID INT NOT NULL,
       PRIMARY KEY(UserID, GroupID),
       FOREIGN KEY(UserID) REFERENCES pichunter.user(ID)
       	       ON UPDATE CASCADE
	       ON DELETE CASCADE,
       FOREIGN KEY(GroupID) REFERENCES pichunter.usergroup(ID)
       	       ON UPDATE CASCADE
	       ON DELETE CASCADE);

CREATE TABLE pichunter.permission (
       ID SERIAL,
       action varchar(2000),
       PRIMARY KEY (ID));

INSERT INTO pichunter.permission(action) VALUES('view-picture');
INSERT INTO pichunter.permission(action) VALUES('insert-picture');
       
CREATE TABLE pichunter.grouppermission (
       PermissionID INT,
       GroupID INT,

       PRIMARY KEY(PermissionID, GroupID),
       FOREIGN KEY(PermissionID) REFERENCES pichunter.permission(ID)
       	       ON UPDATE CASCADE
	       ON DELETE CASCADE,
       FOREIGN KEY(GroupID) REFERENCES pichunter.usergroup(ID)
       	       ON UPDATE CASCADE
	       ON DELETE CASCADE);

INSERT INTO pichunter.grouppermission VALUES (1, 1);
INSERT INTO pichunter.grouppermission VALUES (2, 1);

CREATE VIEW pichunter.user_abilities AS
SELECT "user".id, "user".username, perm.action
FROM pichunter.user "user"
JOIN pichunter.groupmapping gm on "user".ID = gm.UserID
JOIN pichunter.usergroup ug on gm.GroupID = ug.ID
join pichunter.grouppermission gp on gp.GroupID = ug.ID
JOIN pichunter.permission perm on perm.ID = gp.PermissionID;
