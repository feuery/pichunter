CREATE TABLE pichunter.pictureguessing_session (
       ID SERIAL PRIMARY KEY NOT NULL,
       user_id int not null,
       picture_id uuid not null,
       guessed_at timestamp null,
       foreign key (user_id) references pichunter.user(id)
       on delete cascade
       on update cascade,
       foreign key (picture_id) references pichunter.pictures(id)
       on delete cascade
       on update cascade);       
