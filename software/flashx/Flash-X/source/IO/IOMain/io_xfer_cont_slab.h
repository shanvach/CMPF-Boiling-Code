#ifndef IO_XFER_CONT_SLAB_H
#define IO_XFER_CONT_SLAB_H

#include <assert.h>
#include <string.h>
#include <stdio.h>

#include "constants.h"
#include "Simulation.h"
#include "io_flash.h"
#include "mangle_names.h"
#include "io_flash.h"

#if defined (FLASH_IO_HDF5)
#include "io_h5_xfer_wrapper.h"
#endif
#if defined (FLASH_IO_PNETCDF)
#include "io_ncmpi_xfer_wrapper.h"
#endif

#ifdef USE_IO_C_INTERFACE
#ifdef FTOC
#undef FTOC
#endif
#define FTOC(x) x
#endif

void FTOC(io_xfer_cont_slab)(const int * const pMyPE,
                             const io_fileID_t * const pFileID,
                             const int * const pLibType,
                             const int * const pXferType,
			     const int * const pTypeMatchedXfer,
			     const char datasetName[],
                             const int * const pNameLength,
                             const int memSize[],
                             const int memStart[],
                             const int memCount[],
                             const int * const pMemType,
                             const int diskStart[],
                             const int diskCount[],
                             const int * const pDims,
                             void * pData,
                             const int numFileBlks[],
			     int * const pErr);
#endif
