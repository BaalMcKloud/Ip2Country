<?php

// Function that translates the 32-bit unsigned long to a 4-byte string.
// Uses BCmath since php itself can't handle it. Also returns a group ID
// for the IP address - presently the high 13 bits - to calculate the index.
function ip2bin($ip)
{
    $lo = (int)bcmod($ip, 65536);
    $hi = (int)bcdiv($ip, 65536, 0);
    
    return array(chr($hi >> 8) . chr($hi) . chr($lo >> 8) . chr($lo), $hi >> 3);
}

// Open the csv file
$fp = fopen('data/IpToCountry.csv', 'r');

// Initialize
$group    = null;
$count    = 0;
$data     = array();
$index    = array();
$t0       = time();
$datapos  = 0;
$testdata = '';

// For each row...
while (($row = fgetcsv($fp, 255)) !== false)
    if ($row && count($row) > 4 && substr(trim($row[0]), 0, 1) != '#')
    {
        list($iplow, $iphigh, , , $iso) = $row;
        
        if (strlen($iso) != 2)
            echo('Invalid ISO code: ' + $iso);
        else
        {
            // Translate IP to 4-byte binary strings
            list($blow, $bgroup)  = ip2bin($iplow);
            list($bhigh)          = ip2bin($iphigh);

            $tlow = substr($blow, 0, 3) . chr((ord($blow[3]) + ord($bhigh[3])) >> 1);
            $testdata .= $blow . $iso . $bhigh . $iso . $tlow . $iso;
        
            // New index group?
            if ($group !== $bgroup)
            {
                $group = $bgroup;
                $index[$group] = array($blow, $datapos);
                $data[$group] = '';
            }

            // Add IP/county to data
            $data[$group] .= $blow . $iso;
            $datapos += 6;
            $count++;
        }
    }

// Build the index using the stored data locations and chunk lengths
$indexbin = '';
$maxlen = 0;
foreach($index as $group => &$index0)
{
    $len = strlen($data[$group]);
    $maxlen = max($maxlen, $len);
    $indexbin .= $index0[0] . pack('LL', $index0[1], $len);
}

$databin = implode('', $data);
$databin .= "\xFF\xFF\xFF\xFFZZ"; // Add one more for guaranteed upper bound
fclose($fp);

// Save data files
file_put_contents('data/ip2country.dat', pack('LL', strlen($indexbin), strlen($databin)) . $indexbin . $databin);
file_put_contents('data/test.dat', $testdata);

// Display statistics
echo "$count records processed in ", time() - $t0, " seconds\r\n";
echo count($data). " index records with max group length = $maxlen\r\n";
