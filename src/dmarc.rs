use chrono::{DateTime, Utc};

pub enum Adkim {
    Relaxed,
    Strict
}

pub enum Aspf {
    Relaxed,
    Strict
}

pub enum Action {
    None,
    Quarantine,
    Reject
}

pub struct Policy {
    pub domain: String,
    pub adkim: Adkim,
    pub aspf: Aspf,
    pub percentage: u8,
    pub action: Action
}

pub struct Mailer {
    pub host: String
}

pub struct DkimResult {
    pub domain: String,
    pub selector: String,
    pub result: bool
}

pub struct SpfResult {
    pub domain: String,
    pub result: bool
}

pub struct Result {
    pub mailer: Mailer,
    pub header_from: String,
    pub dkim: DkimResult,
    pub spf: SpfResult,
    pub count: usize
}

pub struct Reporter {
    pub name: String,
    pub email: String,
    pub info: String
}

#[derive(Debug)]
pub struct Report {
    //reporter: Reporter,
    pub id: String,
    pub start: DateTime<Utc>,
    pub end: DateTime<Utc>,
    //policy: Policy
}
