create table post (
	id integer constraint 'pk_post' primary key autoincrement not null,
	title varchar(500) not null,
	post text not null,
	author varchar(30) not null,
	postdate datetime default(current_timestamp)
);
