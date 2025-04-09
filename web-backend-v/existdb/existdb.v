module existdb

//import time
import net.http

const api_path := '/exist/rest'

pub struct Config {
pub:
  baseurl string
  username string
  password string
  collection string
}

pub struct ExistDb {
  config Config
}

pub fn new_existdb(config Config) ExistDb {
  return ExistDb{config}
}

pub fn (db ExistDb) query_row_count() !string {
  url := '${db.config.baseurl}${api_path}/${db.config.collection}'
  //response := http.get(url)!
  q := '<query xmlns="http://exist.sourceforge.net/NS/exist" start="1" max="5" cache="yes" session-id="">
  <text>
<![CDATA[
xquery version "3.1";

for \$row in collection(\'/dmarc\')/feedback/record/row
    group by \$ip := \$row/source_ip/text()
    let \$rowCount := count(\$row)
    let \$totalCount := sum(\$row/count)
    let \$avgCount := \$totalCount div \$rowCount
    order by \$ip
    return
        <item>
            <ip>{\$ip}</ip>
            <rows>{\$rowCount}</rows>
            <count>{\$totalCount}</count>
            <avgCount>{\$avgCount}</avgCount>
        </item>
]]>
  </text>
  <properties>
    <property name="foo" value="bar"/>
  </properties>
</query>'
  response := http.post(url, q)!

  return response.body
}

