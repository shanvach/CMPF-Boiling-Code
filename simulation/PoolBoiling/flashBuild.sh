# cache the value of current working directory
NodeDir=$(realpath .)

# run Flash-X setup
cd $FLASHX_HOME && echo Flash-X HEAD is at $(git rev-parse --short HEAD)
cd $FLASHX_HOME && git checkout $FlashSha && ./setup $FlashOptions

# compile the simulation and copy files
cd $FLASHX_HOME/object && make && cp flashx $NodeDir/ && cp setup_params $NodeDir/ && cp *hdf5_htr* $NodeDir/ && cp flash.par $NodeDir/

# copy AMReX configuration
cd $AMREX2D_HOME/lib/pkgconfig && cp amrex.pc $NodeDir/

# chdir into node directory and do clean up
cd $NodeDir #&& rm -rf $FLASHX_HOME/object
