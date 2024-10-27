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
  "policy_adkim" text,
  "policy_aspf" text,
  "policy_p" text,
  "policy_sp" text,
  "policy_pct" int,
  "policy_fo" int,

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

create table report_auth_evaluation (
  "starttime" timestamptz not null,
  "endtime" timestamptz not null,
  "report_id" int,

  /* record.row */
  "source_ip" text,
  "count" int,

  /* record.row.policy_evaluated */
  "disposition" text,
  "dkim" boolean,
  "spf" boolean,

  /* record.identifiers */
  "envelope_from" text,
  "envelope_to" text,
  "header_from" text,

  /* record.auth_results */

  /* TODO */

  foreign key ("report_id") references report ("id") on update cascade on delete cascade
);
select create_hypertable('report_record', 'starttime');
create index report_record_end_idx on report_record ("endtime");

create table report_dkim_evaluation (
  "starttime" timestamptz not null,
  "endtime" timestamptz not null,

  "domain" text,
  "selector" text,
  "result" text
);
select create hypertable('report_dkim_evaluation', 'startime');

create table report_spf_evaluation (
  "startime" timestamptz not null,
  "endtime" timestamptz not null,

  "domain" text,
  "scope" text,
  "result" text
);
select create hypertable('report_spf_evaluation', 'startime');
