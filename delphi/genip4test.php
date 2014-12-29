<?php

mt_srand();

for ($i=1; $i<=1000; $i++)
    for ($j=1; $j<=1000; $j++)
        echo mt_rand(0,255), '.', mt_rand(0,255), '.', mt_rand(0, 255), '.', mt_rand(0, 255), "\r\n";
