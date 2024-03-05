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

struct Sender {
    host: String
}

struct Result {
    sender: Sender,
    dkim_pass: bool,
    spf_pass: bool
}

struct Report {
    id: String,
    start: Time,
    end: Time,
    sender: String
}
