 CREATE TABLE locationguessing_session (
       ID UUID PRIMARY KEY NOT NULL default gen_random_uuid(),
       user_id int not null,
       started_at timestamp not null default now(),
       completed_at timestamp null default null,
       foreign key (user_id) references users(id)
       ON UPDATE CASCADE
       ON DELETE CASCADE
);

CREATE TABLE locationguessing_session_guess (
       ID SERIAL NOT NULL PRIMARY KEY,
       session_id UUID NOT NULL,
       picture_id UUID NOT NULL,
       correctly_guessed boolean not null,
       foreign key(session_id) REFERENCES locationguessing_session(ID)
       ON UPDATE CASCADE
       ON DELETE CASCADE,

       foreign key(picture_id) references pictures(id)
       ON UPDATE CASCADE
       ON DELETE CASCADE       
);
