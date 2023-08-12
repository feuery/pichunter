CREATE TABLE pictureguessing_session (
       ID SERIAL PRIMARY KEY NOT NULL,
       user_id int not null,
       picture_id uuid not null,
       guessed_at timestamp null,
       foreign key (user_id) references users(id)
       on delete cascade
       on update cascade,
       foreign key (picture_id) references pictures(id)
       on delete cascade
       on update cascade);       
