
#module load intel/2021.4.0
module load gcc/9.4.0
# Load MPI module. This should be available as standard module on a cluster.
# If not, build your own MPI and update PATH, LD_LIBRARY_PATH
#module load openmpi-4.1.1
module load openmpi
# Set MPI_HOME by quering path loaded by site module
export MPI_HOME=$(which mpicc | sed s/'\/bin\/mpicc'//)

# Load HDF5 module in desired configuration if available. If not specified
# the HDF5 will be built when setting up software
module load hdf5

# Path to parallel HDF5 installtion with fortran support
export HDF5_HOME=$(which h5pfc | sed s/'\/bin\/h5pfc'//)
#export HDF5_HOME="/home/svachhan/hdf5-1.14.3_build"
#module load flashxtest
