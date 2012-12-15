create table person (
	id serial not null,
	name character varying(50) not null,
	constraint pk_person
		primary key(id)
);