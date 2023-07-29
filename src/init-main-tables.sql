CREATE TABLE pictures
(
	id uuid not null primary key default gen_random_uuid(),
	filename text not null,
	mime text not null,
	latitude float not null,
	longitude float not null,
	data bytea not null
	
);
