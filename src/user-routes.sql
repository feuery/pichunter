-- name: make-everybody-member-of-everything! @execute
-- joku-muu-attribuutti: lol 
-- Insert groupmappings between every user and
-- every group that exists in the db. This probably
-- should not be called unless count(*) from users
-- == something small

INSERT INTO groupmapping (UserID, GroupID)
SELECT "user".ID, "group".ID
FROM users "user"
JOIN usergroup "group" ON 1=1;

-- name: activate-everybody! @execute
-- activates every user in the db
UPDATE users
SET activated = true;

-- name: users-exists? @query
-- returns: :single
-- tells us if there exists an user in the db
SELECT EXISTS (SELECT * FROM users);

-- name: insert-user @execute
-- inserts (doesn't upsert) an user into db
INSERT INTO users(username, password, display_name) VALUES ($1, $2, $3);

-- name: select-user-by-login @query
-- returns: (:dao user :single)
SELECT id, username, display_name, img_id FROM users WHERE username = $1 AND password = $2 AND NOT banned; 

-- name: data-for-frontend-with-password @query
-- returns: :array-hash

SELECT "user".id, "user".username, "user".display_name AS "displayName", "user".img_id AS "imgId", json_agg("abilities".action) as abilities, activated as "activated?", banned as "banned?"
FROM users "user"
JOIN user_abilities "abilities" ON "abilities".id = "user".id
WHERE "user".username = $1 AND "user".password = $2 AND NOT "user".banned
GROUP BY "user".id;

-- name: data-for-frontend-without-password @query
-- returns: :array-hash
SELECT "user".id, "user".username, "user".display_name AS "displayName", "user".img_id AS "imgId", json_agg("abilities".action) as abilities, activated as "activated?", banned as "banned?"
FROM users "user"
JOIN user_abilities "abilities" ON "abilities".id = "user".id
WHERE "user".id = $1
GROUP BY "user".id;

-- name: user-by-id @query
-- returns: (:dao user :single)
SELECT id, username, display_name, img_id FROM users WHERE id = $1;

-- name: update-names @execute
UPDATE users 
SET username = $1,
    display_name = $2
WHERE id = $3;

-- name: get-password-by-id @query
SELECT password FROM users WHERE id = $1;

-- name: update-everything @execute
UPDATE users 
SET username = $1,
    display_name = $2,
    password = $3
WHERE id = $4 AND password = $5;

-- name: insert-avatar @query
insert into avatar (filename, mime, data) values ($1, $2, $3) returning id;

-- name: update-avatar-reference @execute
UPDATE users SET img_id = $1 WHERE id = $2;

-- name: get-all-users @query
-- returns: :plists
-- returns everything about every user on the system :D

select * from users;

-- name: get-session-scores @query
-- returns: :array-hash

SELECT
    a.gametype,
    max(a.count) AS "correct",
    max(b.count) AS "all_guesses"
FROM (
    SELECT
        session_id,
        count(*),
        session.gametype
    FROM
        game_session_guess guess
        JOIN game_session session ON session.id = guess.session_id
    WHERE
        correctly_guessed AND session.user_id = $1
    GROUP BY
        session_id,
        session.gametype) AS a
    JOIN (
        SELECT
            session_id,
            count(*),
            session.gametype
        FROM
            game_session_guess guess
        JOIN game_session session ON session.id = guess.session_id
	WHERE session.user_id = $1
        GROUP BY
            session_id,
            session.gametype) b ON b.session_id = a.session_id
GROUP BY
    a.gametype;

-- name: find-by-username @query
-- returns: :plists
select * from users where username = $1;
