/*drop table if exists report_record;*/
drop table if exists report;
drop table if exists reporter;

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
  "reporter_id" int,

  "begin" timestamptz,
  "end" timestamptz,
  "error" text,

  "policy_domain" text,
  "policy_adkim" alignment,
  "policy_aspf" alignment,
  "policy_p" disposition,
  "policy_sp" disposition,
  "policy_pct" int,
  "policy_fo" text,

  primary key ("report_id"),
  foreign key ("reporter_id") references reporter ("id") on update cascade on delete cascade
);
select create_hypertable('report', 'begin');
create index report_end_idx on report ("end");

create table policy_evaluated (
  "begin" timestamptz not null,
  "end" timestamptz not null,
  "report_id" text,

  /* record.row */
  "source_ip" text,
  "count" int,

  /* record.row.policy_evaluated */
  "disposition" disposition,
  "dkim" dmarc_result,
  "spf" dmarc_result,

  /* record.identifiers */
  "envelope_from" text,
  "envelope_to" text,
  "header_from" text,

  /* record.auth_results */

  /* TODO */

  foreign key ("report_id") references report ("report_id") on update cascade on delete cascade
);
select create_hypertable('policy_evaluated', 'begin');
create index policy_evaluated_end_idx on policy_evaluated ("end");

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
