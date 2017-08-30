#!/bin/bash
cd /Users/nfiedler/projects/akashita/_build/test/logs/ct_run.nonode@nohost.2017-12-07_18.05.38/lib.akashita.logs/run.2017-12-07_18.05.38/log_private/photos-2017-12-07
tar -C /a1c0sxxywxyh1r4befaqq72gf2/photos -f - -c --exclude .VolumeIcon.icns --exclude .fseventsd --exclude .Trashes . | split -d -a 5 -b 64K - photos
exit ${PIPESTATUS[0]}
