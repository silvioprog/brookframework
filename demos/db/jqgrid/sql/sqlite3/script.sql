create table person (
	id integer constraint 'pk_person' primary key autoincrement not null,
	name character varying(50) not null unique
);
