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

pub fn (db ExistDb) query_count() !string {
  url := '${db.config.baseurl}${api_path}/${db.config.collection}'
  q := '<query xmlns="http://exist.sourceforge.net/NS/exist"
    xmlns:sx="http://exist-db.org/xquery/types/serialized"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    start="1" max="5" cache="no" session-id="123">
  <text>
<![CDATA[
xquery version "3.1";

declare variable \$wantedBegin external;
declare variable \$wantedEnd external;

for \$row in collection("/dmarc")/feedback/record/row
    let \$metadata := \$row/ancestor::feedback/report_metadata
    let \$beginTimestamp := \$metadata/date_range/begin/text()
    let \$endTimestamp := \$metadata/date_range/end/text()
    where \$endTimestamp >= \$wantedBegin and \$beginTimestamp <= \$endTimestamp
    group by \$g := \$row/count > 0
    let \$count := sum(\$row/count)
    return
        <item>
            <count>{\$count}</count>
        </item>
]]>
  </text>
  <variables>
    <variable>
        <qname>
            <localname>wantedBegin</localname>
        </qname>
        <sx:sequence>
            <sx:value type="xs:integer">1728864100</sx:value>
        </sx:sequence>
    </variable>
    <variable>
        <qname>
            <localname>wantedEnd</localname>
        </qname>
        <sx:sequence>
            <sx:value type="xs:integer">1728864300</sx:value>
        </sx:sequence>
    </variable>
  </variables>
  <properties>
    <property name="foo" value="bar"/>
  </properties>
</query>'

  response := http.post(url, q)!

  return response.body
}

