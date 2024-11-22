/*drop index if exists report_end_idx;*/

drop table if exists spf_evaluation cascade;
drop table if exists dkim_evaluation cascade;
drop table if exists evaluation cascade;
drop table if exists report cascade;
drop table if exists reporter cascade;

drop type if exists spf_result cascade;
drop type if exists spf_domain_scope cascade;
drop type if exists dkim_result cascade;
drop type if exists policy_override cascade;
drop type if exists dmarc_result cascade;
drop type if exists disposition cascade;
drop type if exists alignment cascade;


create type alignment as enum ('strict', 'relaxed');
create type disposition as enum ('none', 'quarantine', 'reject');
create type dmarc_result as enum ('pass', 'fail');
create type policy_override as enum ('forwarded', 'sampled_out', 'trusted_forwarder', 'mailing_list', 'local_policy', 'other');
create type dkim_result as enum ('none', 'pass', 'fail', 'policy', 'neutral', 'temperror', 'permerror');
create type spf_domain_scope as enum ('helo', 'mfrom');
create type spf_result as enum ('none', 'neutral', 'pass', 'fail', 'softfail', 'temperror', 'permerror');


create table reporter (
  "id" serial not null,

  "org_name" text,
  "email" text,
  "extra_contact_info" text,

  primary key ("id")
);


create table report (
  "report_id" text not null,
  "begin" timestamptz not null,
  "end" timestamptz not null,
  "reporter_id" int,

  "error" text,
  "policy_domain" text,
  "policy_adkim" alignment,
  "policy_aspf" alignment,
  "policy_p" disposition,
  "policy_sp" disposition,
  "policy_pct" int,
  "policy_fo" text,

  primary key ("report_id", "begin"),
  foreign key ("reporter_id") references reporter ("id") on update cascade on delete cascade
);
select create_hypertable('report', 'begin');
create index report_end_idx on report ("end");


create table evaluation (
  "id" serial not null,
  "begin" timestamptz not null,
  "report_id" text,

  /* record.row */
  "source_ip" inet,
  "count" int,

  /* record.row.policy_evaluated */
  "disposition" disposition,
  "dkim" dmarc_result,
  "spf" dmarc_result,

  /* record.identifiers */
  "envelope_from" text,
  "envelope_to" text,
  "header_from" text,

  primary key ("id", "begin"),
  foreign key ("report_id", "begin") references report ("report_id", "begin") on update cascade on delete cascade
);
select create_hypertable('evaluation', 'begin');


create table dkim_evaluation (
  "id" serial not null,
  "begin" timestamptz not null,
  "evaluation_id" int,

  "domain" text,
  "selector" text,
  "result" dkim_result,
  "human_result" text,

  primary key ("id", "begin"),
  foreign key ("evaluation_id", "begin") references evaluation ("id", "begin") on update cascade on delete cascade
);
select create_hypertable('dkim_evaluation', 'begin');


create table spf_evaluation (
  "id" serial not null,
  "begin" timestamptz not null,
  "evaluation_id" int,

  "domain" text,
  "scope" spf_domain_scope,
  "result" spf_result,

  primary key ("id", "begin"),
  foreign key ("evaluation_id", "begin") references evaluation ("id", "begin") on update cascade on delete cascade
);
select create_hypertable('spf_evaluation', 'begin');
