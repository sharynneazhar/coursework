//  Created by Gary J. Minden on 10/20/2015.
//  Copyright 2015 Gary J. Minden. All rights reserved.
//
//  Updates:
//
//      B51020 -- initial version
//
//
//	Description:
//			This program simulates a three port register file.
//

#ifndef __RegisterFile_H_
#define __RegisterFile_H_

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdarg.h>

//
//	Define RegisterFile structures
//
#define Registers_Nbr 32

typedef int RegisterFile[Registers_Nbr];

//*****************************************************************************
//
// Prototypes for the APIs.
//
//*****************************************************************************

extern void RegisterFile_Cycle( RegisterFile theRegisterFile,
								uint32_t RdAddr_S, uint32_t* RdValue_S,
								uint32_t RdAddr_T, uint32_t* RdValue_T,
								bool WrtEnb, uint32_t WrtAddr, uint32_t WrtValue );

extern void RegisterFile_Read( RegisterFile theRegisterFile,
								uint32_t RdAddr_S, uint32_t* RdValue_S,
								uint32_t RdAddr_T, uint32_t* RdValue_T );

extern void RegisterFile_Write( RegisterFile theRegisterFile,
								bool WrtEnb, uint32_t WrtAddr, uint32_t WrtValue );

extern void RegisterFile_Dump( RegisterFile theRegisterFile );

#endif		// __RegisterFile_H_
