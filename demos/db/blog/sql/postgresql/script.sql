create table post (
	id serial not null,
	title varchar(500) not null,
	post text not null,
	author varchar(30) not null,
	postdate timestamp default current_timestamp,
	constraint pk_posts
		primary key (id)
);