create table if not exists Packages (
    id integer primary key autoincrement not null,
    name        text not null,
    output      text default "out",
    system      text not null,
    path        text not null, -- store path, e.g. /gnu/store/abcd...-foo
    -- path        text unique not null, -- TODO: Make unique?  Maybe to slow.
    version     text not null,
    guix        text not null
);

create virtual table if not exists Files using fts5(
    subpath,
    package
);
