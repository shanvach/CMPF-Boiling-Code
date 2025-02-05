#!/bin/bash
# Slurm sbatch parameters section:
#SBATCH -n 1
#SBATCH -t 4:00:00
#SBATCH --mem-per-cpu=1024
#    SBATCH -share
#SBATCH --exclusive
#SBATCH --partition=standard
#SBATCH --export=NONE
#

# Section to ensure we have the "module" command defined
unalias tap >& /dev/null
if [ -f ~/.bash_profile ]; then
	source ~/.bash_profile
elif [ -f ~/.profile ]; then
	source ~/.profile
fi
export SLURM_EXPORT_ENV=ALL
module purge
module load hpcc/zaratan
module load gcc/9.4
module load openmpi
module load hello-umd/1.5
module load hdf5
export LD_LIBRARY_PATH="${HDF5_ROOT}:${LD_LIBRARY_PATH}"
export LD_LIBRARY_PATH="${HYPRE_ROOT}:${LD_LIBRARY_PATH}"
#export LD_LIBRARY_PATH="/home3/jqiao1/flash-distro/TecioLib:${LD_LIBRARY_PATH}"

#TMPWORKDIR="/scratch/zt1/project/ariaz-prj/user/$USER/ood-job.${SLURM_JOBID}"
#mkdir $TMPWORKDIR



# Section to output information identifying the job, etc.
echo "Slurm job ${SLURM_JOBID} running on"
hostname
echo "To run on ${SLURM_NTASKS} CPU cores across ${SLURM_JOB_NUM_NODES} nodes"
echo "All nodes: ${SLURM_JOB_NODELIST}"
date
pwd
echo "Loaded modules are:"
module list
#echo "Job will be started out of $TMPWORKDIR"

export OMPI_MCA_mpi_cuda_support=0


#====================================================

#ln -s ${TMPWORKDIR} ${SLURM_SUBMIT_DIR}/work-dir

#make 1

#cd $TMPWORKDIR

#MYEXEDIR="/home/jqiao/flash_run/3D_sphere_160_160_160/"
#echo "Using executable $MYEXEDIR"

mpirun -np 1 flashx > output.${SLURM_JOBID} 2>&1


#====================================================


echo "Job finished with exit code $ECODE. Work dir is $TMPWORKDIR"
date

# Exit with the cached exit code
exit $ECODE

