-- :name insert-media :<!
insert into pichunter.pictures (filename, data, mime) values (:name, :data, :mime) returning id


-- :name delete-picture* :!
delete from pichunter.pictures where id = :id;

-- :name get-media :? :1
select filename, data, mime from pichunter.pictures where id = :id::uuid
