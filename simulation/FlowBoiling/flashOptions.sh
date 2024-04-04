# cache the value of current working directory

FlashSha="c41729f0"

FlashOptions="incompFlow/FlowBoiling -auto -maxblocks=100 -2d -nxb=16 -nyb=16 +amrex +parallelIO -site=$SiteHome +incomp"

if [ $Profile = True ]; then
	FlashOptions="$FlashOptions +hpctoolkit"
fi
