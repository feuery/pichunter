-- name: get-session-id @query
-- returns: :plist
SELECT id FROM game_session WHERE completed_at IS NULL AND user_id = $1 AND gametype = $2;

-- name: insert-new-session @query
INSERT INTO game_session (user_id, gametype) VALUES ($1, $2) returning id;

-- name: get-next-pic
-- returns: :array-hash
SELECT pic.id, pic.filename, pic.latitude, pic.longitude, pic.approver
FROM pictures pic
JOIN game_session session ON session.user_id = $1 AND completed_at IS NULL AND gametype = $2
WHERE pic.id NOT IN (select picture_id from game_session_guess where session_id = session.id)
ORDER BY random()
LIMIT 1;
