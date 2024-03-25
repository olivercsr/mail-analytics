use chrono::{DateTime, Utc};

#[derive(Debug)]
pub enum Adkim {
    Relaxed,
    Strict
}

#[derive(Debug)]
pub enum Aspf {
    Relaxed,
    Strict
}

#[derive(Debug)]
pub struct Dkim {
    pub domain: String,
    pub result: bool,
    pub selector: Option<String>
}

#[derive(Debug)]
pub struct Spf {
    pub domain: String,
    pub result: bool
}

#[derive(Debug)]
pub struct AuthResults {
    pub dkim: Dkim,
    pub spf: Spf
}

#[derive(Debug)]
pub struct Identifiers {
    pub envelope_from: String,
    pub header_from: String
}

#[derive(Debug)]
pub struct PolicyEvaluated {
    disposition: String,
    dkim: bool,
    spf: bool
}

#[derive(Debug)]
pub struct Row {
    source_id: String,
    count: usize,
    policy_evaluated: PolicyEvaluated
}

#[derive(Debug)]
pub struct Record {
    pub rows: Vec<Row>,
    pub identifiers: Identifiers,
    pub auth_results: AuthResults
}

#[derive(Debug)]
pub struct PolicyPublished {
    pub domain: String,
    pub adkim: Adkim,
    pub aspf: Aspf,
    pub p: (), // TODO
    pub sp: (), // TODO
    pub percentage: u8,
    pub fo: () // TODO
}

#[derive(Debug)]
pub struct DateRange {
    pub begin: DateTime<Utc>,
    pub end: DateTime<Utc>
}

#[derive(Debug)]
pub struct ReportMetadata {
    pub report_id: String,
    pub ord_name: String,
    pub email: String,
    pub date_range: DateRange
}

#[derive(Debug)]
pub struct Report {
    pub report_metadata: ReportMetadata,
    //pub policy_published: PolicyPublished,
    //pub records: Vec<Record>,
}
