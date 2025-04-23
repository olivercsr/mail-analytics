module existdb

//import time
import net.http
import encoding.xml

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
  q := $tmpl("templates/query_row_count.xquery")
//  q := '<query xmlns="http://exist.sourceforge.net/NS/exist" start="1" max="5" cache="yes" session-id="">
//  <text>
//<![CDATA[
//xquery version "3.1";
//
//for \$row in collection(\'/dmarc\')/feedback/record/row
//    group by \$ip := \$row/source_ip/text()
//    let \$rowCount := count(\$row)
//    let \$totalCount := sum(\$row/count)
//    let \$avgCount := \$totalCount div \$rowCount
//    order by \$ip
//    return
//        <item>
//            <ip>{\$ip}</ip>
//            <rows>{\$rowCount}</rows>
//            <count>{\$totalCount}</count>
//            <avgCount>{\$avgCount}</avgCount>
//        </item>
//]]>
//  </text>
//  <properties>
//    <property name="foo" value="bar"/>
//  </properties>
//</query>'

println('=====================================')
println(q)
println('=====================================')
  response := http.post(url, q)!

  return response.body
}

pub fn (db ExistDb) query_count() !string {
  url := '${db.config.baseurl}${api_path}/${db.config.collection}'
  q := '<query xmlns="http://exist.sourceforge.net/NS/exist"
    xmlns:sx="http://exist-db.org/xquery/types/serialized"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    start="1" max="15" cache="no" session-id="123">
  <text>
<![CDATA[
xquery version "3.1";

declare variable \$wantedBegin external;
declare variable \$wantedEnd external;

let \$daySeconds := 24 * 60 * 60

let \$wantedBeginDT := xs:dateTime(\'1970-01-01T00:00:00Z\') + xs:dayTimeDuration(\'PT\' || \$wantedBegin || \'S\')
let \$wantedEndDT := xs:dateTime(\'1970-01-01T00:00:00Z\') + xs:dayTimeDuration(\'PT\' || \$wantedEnd || \'S\')
let \$wantedDays := fn:days-from-duration(\$wantedEndDT - \$wantedBeginDT)

for \$day in 0 to \$wantedDays
    let \$dayDiff := \$day * \$daySeconds
    let \$dayBeginTS := \$wantedBegin + \$dayDiff
    let \$dayBeginDT := xs:dateTime(\'1970-01-01T00:00:00Z\') + xs:dayTimeDuration(\'PT\' || \$dayBeginTS || \'S\')
    let \$dayEndTS := \$dayBeginTS + \$daySeconds - 1
    let \$dayEndDT := xs:dateTime(\'1970-01-01T00:00:00Z\') + xs:dayTimeDuration(\'PT\' || \$dayEndTS || \'S\')
    for \$row in collection(\'/dmarc\')/feedback/record/row
        let \$metadata := \$row/ancestor::feedback/report_metadata
        let \$rowBeginTS := \$metadata/date_range/begin/text()
        let \$rowEndTS := \$metadata/date_range/end/text()
        where (\$rowBeginTS <= \$dayEndTS and \$rowEndTS >= \$dayBeginTS)
        and \$row/count > 0
        let \$rowDays := (\$rowEndTS - \$rowBeginTS) div \$daySeconds
        let \$divisor := max([\$rowDays, 1e0])
        let \$dividedRowCount := \$row/count div \$divisor
        group by \$dayBeginTS, \$dayEndTS
        order by \$dayBeginTS, \$dayEndTS
        return
            <item>
                <wanted>{\$dayDiff} = {\$wantedDays} = {\$day} = {\$divisor}</wanted>
                <begin>{\$dayBeginTS} = {\$dayBeginDT}</begin>
                <end>{\$dayEndTS} = {\$dayEndDT}</end>
                <rowbegin>{\$rowBeginTS}</rowbegin>
                <rowend>{\$rowEndTS}</rowend>
                <rowcount>{sum(\$row/count)}</rowcount>
                <dividedrowcount>{sum(\$dividedRowCount)}</dividedrowcount>
            </item>

]]>
  </text>
  <variables>
    <variable>
        <qname>
            <localname>wantedBegin</localname>
        </qname>
        <sx:sequence>
            <sx:value type="xs:integer">1735689600</sx:value>
        </sx:sequence>
    </variable>
    <variable>
        <qname>
            <localname>wantedEnd</localname>
        </qname>
        <sx:sequence>
            <sx:value type="xs:integer">1742974400</sx:value>
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

