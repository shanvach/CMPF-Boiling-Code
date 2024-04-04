# cache the value of current working directory

FlashSha="c6f9ad1e"

FlashOptions="incompFlow/CrossFlow -auto -maxblocks=100 -2d -nxb=16 -nyb=16 +amrex +parallelIO -site=$SiteHome +incomp"

if [ $Profile = True ]; then
	FlashOptions="$FlashOptions +hpctoolkit"
fi
