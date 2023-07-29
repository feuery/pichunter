CREATE TABLE IF NOT EXISTS usergroup (
       ID SERIAL,
       Name VARCHAR NOT NULL UNIQUE,
       Description VARCHAR(2000) NOT NULL DEFAULT '',
       Img_location VARCHAR(2000) NOT NULL DEFAULT '',
       PRIMARY KEY (ID));

INSERT INTO usergroup(Name, Description) VALUES ('Admins', 'Group for admins');

CREATE TABLE IF NOT EXISTS groupmapping (
       UserID INT NOT NULL,
       GroupID INT NOT NULL,
       PRIMARY KEY(UserID, GroupID),
       FOREIGN KEY(UserID) REFERENCES users(ID)
       	       ON UPDATE CASCADE
	       ON DELETE CASCADE,
       FOREIGN KEY(GroupID) REFERENCES usergroup(ID)
       	       ON UPDATE CASCADE
	       ON DELETE CASCADE);

CREATE TABLE permission (
       ID SERIAL,
       action varchar(2000),
       PRIMARY KEY (ID));

INSERT INTO permission(action) VALUES('view-picture');
INSERT INTO permission(action) VALUES('insert-picture');
INSERT INTO permission(action) VALUES('can-admin');
       
CREATE TABLE grouppermission (
       PermissionID INT,
       GroupID INT,

       PRIMARY KEY(PermissionID, GroupID),
       FOREIGN KEY(PermissionID) REFERENCES permission(ID)
       	       ON UPDATE CASCADE
	       ON DELETE CASCADE,
       FOREIGN KEY(GroupID) REFERENCES usergroup(ID)
       	       ON UPDATE CASCADE
	       ON DELETE CASCADE);

INSERT INTO grouppermission(PermissionID, GroupID)
SELECT permission.ID, usergroup.ID
FROM permission permission
JOIN usergroup usergroup ON 1=1;

INSERT INTO usergroup(Name, Description) VALUES ('Users', 'Group for ordinary mortals');

CREATE VIEW user_abilities AS
SELECT "user".id, "user".username, perm.action
FROM users "user"
JOIN groupmapping gm on "user".ID = gm.UserID
JOIN usergroup ug on gm.GroupID = ug.ID
join grouppermission gp on gp.GroupID = ug.ID
JOIN permission perm on perm.ID = gp.PermissionID;
