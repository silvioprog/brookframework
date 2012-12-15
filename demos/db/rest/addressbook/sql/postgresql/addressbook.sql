create table contacts (
	id serial not null,
	name varchar(50) not null unique,
	constraint pk_contacts
		primary key(id) 
);

create table phones (
	id serial not null,
	contactid integer not null,
	number varchar(20) not null unique,
	constraint pk_phones
		primary key(id),
	constraint fk_phones
		foreign key(contactid)
		references contacts(id)
		on delete cascade
		on update cascade
);