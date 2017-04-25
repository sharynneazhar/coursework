//  Created by Gary J. Minden on 10/20/2015.
//  Copyright 2015 Gary J. Minden. All rights reserved.
//
//  Updates:
//
//      B51020 -- initial version
//
//
//	Description:
//			This program simulates a CPU RegisterFile.
//

#include <stdio.h>
#include <time.h>
#include <strings.h>
#include <stdlib.h>
#include <unistd.h>

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdarg.h>

#include "RegisterFile_01.h"

//
//	RegisterFile_Cycle performs one cycle of the register file. This involves
//		two read operations and one write operation. The write operation is
//		gated by a WrtEnb signal. The write operation is performed after
//		the read operations which better reflects the behavior of a 
//		hardware register file.
//
extern void RegisterFile_Cycle( RegisterFile theRegisterFile,
								uint32_t RdAddr_S, uint32_t* RdValue_S,
								uint32_t RdAddr_T, uint32_t* RdValue_T,
								bool WrtEnb, uint32_t WrtAddr, uint32_t WrtValue ) {
								
	printf( "RegisterFile_Cycle: RdAddr_S: %02d; RdAddr_T: %02d; WrtEnb: %01d; WrtAddr: %02d\n",
				RdAddr_S, RdAddr_T, WrtEnb, WrtAddr );
	
	//
	//	Read the operands
	//
	*RdValue_S = theRegisterFile[RdAddr_S];
	*RdValue_T = theRegisterFile[RdAddr_T];
	
	//
	//	If enabled, write data to the register file.
	//
	if ( WrtEnb == true ) {
		theRegisterFile[WrtAddr] = WrtValue;
	}
				
}

//
//	In many cases it is only necessary to read value from a register file.
//		RegisterFile_Read just does the value read operation.
//
extern void RegisterFile_Read( RegisterFile theRegisterFile,
								uint32_t RdAddr_S, uint32_t* RdValue_S,
								uint32_t RdAddr_T, uint32_t* RdValue_T ) {
								
	printf( "RegisterFile_Read: RdAddr_S: %02d; RdAddr_T: %02d;\n",
				RdAddr_S, RdAddr_T );
	
	//
	//	Read the operands
	//
	*RdValue_S = theRegisterFile[RdAddr_S];
	*RdValue_T = theRegisterFile[RdAddr_T];
					
}

//
//	In many cases it is only necessary to write a value to the register file.
//		RegisterFile_Write just does the write operation. The WrtEnb argument
//		is included for consistency.
//
extern void RegisterFile_Write( RegisterFile theRegisterFile,
								bool WrtEnb, uint32_t WrtAddr, uint32_t WrtValue ) {
								
	printf( "RegisterFile_Write:  WrtEnb: %01d; WrtAddr: %02d\n",
				WrtEnb, WrtAddr );
	
	//
	//	If enabled, write data to the register file.
	//
	if ( WrtEnb == true ) {
		theRegisterFile[WrtAddr] = WrtValue;
	}
				
}

//
//	RegisterFile_Dump prints the contents of a register file to standard output
//		(stdout). RegisterFile_Dump is useful for debugging purposes. In the future,
//		this subroutine should include a file descriptor indicating the output.
//
extern void RegisterFile_Dump( RegisterFile theRegisterFile ) {

	//
	//	This routine prints four values per line in hexadecimal format.
	//		The number of lines is determined by the number of registers
	//		in the register file. We assume the number of registers is 
	//		a multiple of 4. If not, some registers will not be printed.
	//
	uint32_t	Lines_Nbr = ( Registers_Nbr / 4 );
	uint32_t	Line_Idx;
	
	for ( Line_Idx = 0; Line_Idx < Lines_Nbr; Line_Idx++ ) {
		printf( "%08X:  %08X  %08X  %08X  %08X\n",
					(Line_Idx * 4), 
					theRegisterFile[(Line_Idx * 4) + 0],
					theRegisterFile[(Line_Idx * 4) + 1],
					theRegisterFile[(Line_Idx * 4) + 2],
					theRegisterFile[(Line_Idx * 4) + 3] );
	}
}
