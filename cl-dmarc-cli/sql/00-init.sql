drop table if exists report_record;
drop table if exists report;
drop table if exists reporter;

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
  "starttime" timestamptz,
  "endtime" timestamptz,

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
  "starttime" timestamptz not null,
  "endtime" timestamptz not null,
  "report_id" int,
  "policy_adkim" text,
  "policy_aspf" text,
  "policy_p" text,
  "policy_sp" text,
  "policy_pct" int,
  "policy_fo" int,

  /* TODO */

  foreign key ("report_id") references report ("id") on update cascade on delete cascade
);
select create_hypertable('report_record', 'starttime');
create index report_record_end_idx on report_record ("endtime");
