create table reporter (
  "id" serial not null,
  "name" text,
  "email" text,

  primary key ("id")
);

create table report (
  "id" serial not null,
  "reporter_id" int,
  "identifier" text,
  "start" timestamptz,
  "end" timestamptz,

  primary key ("id"),
  foreign key ("reporter_id") references reporter ("id") on update cascade on delete cascade
);

/*
create table policy (
  "id" serial not null,
  "adkim" text,
  "aspf" text,
  "p" text,
  "sp" text,
  "pct" int,
  "fo" int,

  primary key ("id")
)
*/

create table report_record (
  "time" timestamptz not null,
  "report_id" int,
  /* TODO */

  foreign key ("report_id") references report ("id") on update cascade on delete cascade
);
select create_hypertable('report_record', 'time');
