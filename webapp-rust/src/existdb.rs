#[derive(Debug, Clone)]
pub struct ExistDb {
    pub uri: String,
}

impl ExistDb {
    pub async fn query_db(
        &self,
        query: &str
    ) {

    }
}

