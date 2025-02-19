xquery version "3.1";

for $row in collection('/mycoll01')/feedback/record/row
    let $metadata := $row/ancestor::feedback/report_metadata
    let $beginTimestamp := $metadata/date_range/begin/text()
    let $beginDateTime := xs:dateTime('1970-01-01T00:00:00+00:00') + xs:dayTimeDuration('PT' || $beginTimestamp || 'S')
    order by $beginDateTime
    return
        <item>
            <begin>{$beginDateTime}</begin>
        </item>
