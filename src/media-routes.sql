-- name: api-pictures @query
-- returns: :array-hash
SELECT id, filename, latitude, longitude, null as "session-id", approver
FROM pictures
WHERE approver IS NOT NULL;

-- name: unapproved-images @query
-- returns: :array-hash
SELECT id, filename, latitude, longitude, null as "session-id", approver
FROM pictures
WHERE approver IS NULL;

-- name: approve-image @execute
UPDATE pictures
SET approver = $1
WHERE id = $2;


-- name: unapprove-image @execute
UPDATE pictures
SET approver = null
WHERE id = $1;
