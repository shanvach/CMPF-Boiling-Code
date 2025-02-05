# cache the value of current working directory

#FlashSha="5aa7289e"

FlashOptions="incompFlow/PoolBoiling -auto -maxblocks=250 -2d -nxb=8 -nyb=8 +amrex +serialIO +nolwf -site=$SiteHome +incomp -tomlfile=$JobWorkDir/job.input --with-unitmods"


if [ $Profile = True ]; then
	FlashOptions="$FlashOptions +hpctoolkit"
fi
