-- name: api-pictures @query
-- returns: :array-hash
SELECT id, filename, latitude, longitude, null as "session-id"
FROM pictures
