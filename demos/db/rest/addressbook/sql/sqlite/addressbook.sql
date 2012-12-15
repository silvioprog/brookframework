create table contacts (
	id integer primary key autoincrement not null,
	name varchar(50) not null unique
);

create table phones (
	id integer primary key autoincrement not null,
	contactid integer not null
	references contacts(id)
		on delete cascade
		on update cascade,
	number varchar(20) not null unique
);