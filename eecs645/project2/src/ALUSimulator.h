//*****************************************************************************
//--ALUSimulator.h
//
//		Author: 		Gary J. Minden
//		Organization:	KU/EECS/EECS 645
//		Date:			2017-04-22 (B70422)
//		Version:		1.0
//		Description:	This is the prototype for a simple ALU simulator
//		Notes:
//
//*****************************************************************************
//

#ifndef ALU_SIMULATOR_H
#define ALU_SIMULATOR_H

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "RegisterFile_01.h"

extern void ALUSimulator( RegisterFile theRegisterFile,
													uint32_t OpCode,
													uint32_t Rs,
													uint32_t Rt,
													uint32_t Rd,
													uint32_t ShiftAmt,
													uint32_t FunctionCode,
													uint32_t ImmediateValue,
													uint32_t* Status );

#endif
