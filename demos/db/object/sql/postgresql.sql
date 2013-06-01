create table person ( 
	id integer primary key not null,
	active boolean not null,
	name varchar(50) not null unique,
	age integer not null,
	weight numeric not null,
	lastupdate timestamp not null
);