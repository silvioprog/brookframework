create table person (
	id serial not null,
	name character varying(50) not null,
	constraint pk_person
		primary key(id)
);

create table post (
	id serial not null,
	title varchar(500) not null,
	post text not null,
	author varchar(30) not null,
	postdate timestamp default current_timestamp,
	constraint pk_posts
		primary key (id)
);