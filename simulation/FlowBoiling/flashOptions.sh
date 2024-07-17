# cache the value of current working directory

#FlashSha="5aa7289e"

FlashOptions="incompFlow/FlowBoiling -auto -maxblocks=100 -2d -nxb=16 -nyb=16 +amrex +parallelIO -site=$SiteHome +incomp"

if [ $Profile = True ]; then
	FlashOptions="$FlashOptions +hpctoolkit"
fi
