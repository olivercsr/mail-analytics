enum Adkim {
    Relaxed,
    Strict
}

enum Aspf {
    Relaxed,
    Strict
}

enum Action {
    None,
    Quarantine,
    Reject
}

struct Policy {
    domain: String,
    adkim: Adkim,
    aspf: Aspf,
    percentage: u8,
    action: Action
}

struct Mailer {
    host: String
}

struct DkimResult {
    domain: String,
    selector: String,
    result: bool
}

struct SpfResult {
    domain: String,
    result: bool
}

struct Result {
    mailer: Mailer,
    header_from: String,
    dkim: DkimResult,
    spf: SpfResult,
    count: usize
}

struct Reporter {
    name: String,
    email: String,
    info: String
}

struct Report {
    reporter: Reporter,
    id: String,
    start: Time,
    end: Time,
    policy: Policy
}
