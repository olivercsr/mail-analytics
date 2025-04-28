for $row in collection('/dmarc')/feedback/record/row
    group by $ip := $row/source_ip/text()
    let $rowCount := count($row)
    let $totalCount := sum($row/count)
    let $avgCount := $totalCount div $rowCount
    order by $ip
    return
        <item>
            <ip>{$ip}</ip>
            <rows>{$rowCount}</rows>
            <count>{$totalCount}</count>
            <avgCount>{$avgCount}</avgCount>
        </item>
