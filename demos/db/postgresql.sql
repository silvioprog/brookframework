create table person (
	id serial not null,
	name character varying(50) not null,
	constraint pk_person
		primary key(id)
);

commit;

insert into person(name) values ('Silvio Clecio');
insert into person(name) values ('Luciano Souza');
insert into person(name) values ('Joao Morais');
insert into person(name) values ('Waldir Paim');
insert into person(name) values ('Gilson Nunes');
insert into person(name) values ('Luiz Americo');
insert into person(name) values ('Mario Ray Mahardhika');
insert into person(name) values ('Michael van Ham');
insert into person(name) values ('Yuriy Pilgun');
insert into person(name) values ('Reinier Olislagers');
insert into person(name) values ('Michael Canneyt');

create table people ( 
	id serial primary key not null,
	active boolean not null,
	name varchar(50) not null unique,
	age integer not null,
	weight numeric not null,
	lastupdate timestamp not null
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