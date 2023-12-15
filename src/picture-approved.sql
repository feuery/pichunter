ALTER TABLE pictures
ADD COLUMN approver int null;

ALTER TABLE pictures
ADD CONSTRAINT fk_approver FOREIGN KEY(approver) REFERENCES users(id);

