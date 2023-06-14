CREATE TABLE pichunter.pictures
(
	id uuid not null primary key default gen_random_uuid(),
	filename text not null,
	mime text not null,
	data bytea not null
);
