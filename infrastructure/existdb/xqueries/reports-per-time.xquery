xquery version "3.1";

declare variable $wantedBegin external;
declare variable $wantedEnd external;

let $daySeconds := 24 * 60 * 60

let $wantedBeginDT := xs:dateTime('1970-01-01T00:00:00Z') + xs:dayTimeDuration('PT' || $wantedBegin || 'S')
let $wantedEndDT := xs:dateTime('1970-01-01T00:00:00Z') + xs:dayTimeDuration('PT' || $wantedEnd || 'S')
let $wantedDays := fn:days-from-duration($wantedEndDT - $wantedBeginDT)

for $day in 0 to $wantedDays
    let $dayDiff := $day * $daySeconds
    let $dayBeginTS := $wantedBegin + $dayDiff
    let $dayBeginDT := xs:dateTime('1970-01-01T00:00:00Z') + xs:dayTimeDuration('PT' || $dayBeginTS || 'S')
    let $dayEndTS := $dayBeginTS + $daySeconds - 1
    let $dayEndDT := xs:dateTime('1970-01-01T00:00:00Z') + xs:dayTimeDuration('PT' || $dayEndTS || 'S')
    for $record in collection('/dmarc')/feedback/record
        let $row := $record/row
        let $metadata := $row/ancestor::feedback/report_metadata
        let $rowBeginTS := $metadata/date_range/begin/text()
        let $rowEndTS := $metadata/date_range/end/text()
        where ($rowBeginTS <= $dayEndTS and $rowEndTS >= $dayBeginTS)
        and $row/count > 0
        let $rowDays := ($rowEndTS - $rowBeginTS) div $daySeconds
        let $divisor := max([$rowDays, 1e0])
        let $dividedRowCount := $row/count div $divisor
        let $dkimResult := $row/policy_evaluated/dkim/text()
        let $spfResult := $row/policy_evaluated/spf/text()
        group by $dayBeginTS, $dayEndTS, $spfResult, $dkimResult
        order by $dayBeginTS, $dayEndTS
        return
            <item>
                <wanted>{$dayDiff} = {$wantedDays} = {$day} = {$divisor}</wanted>
                <begin>{$dayBeginTS} = {$dayBeginDT}</begin>
                <end>{$dayEndTS} = {$dayEndDT}</end>
                <rowbegin>{$rowBeginTS}</rowbegin>
                <rowend>{$rowEndTS}</rowend>
                <rowcount>{sum($row/count)}</rowcount>
                <spf>{$spfResult}</spf>
                <dkim>{$dkimResult}</dkim>
                <dividedrowcount>{$dividedRowCount}</dividedrowcount>
                <dividedrowcountsum>{sum($dividedRowCount)}</dividedrowcountsum>
            </item>
