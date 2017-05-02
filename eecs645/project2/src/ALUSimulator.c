//*****************************************************************************
//--ALUSimulator.c
//
//		Author: 		    Gary J. Minden
//		Organization:	  KU/EECS/EECS 645
//		Date:			      2017-04-22 (B70422)
//		Version:		    1.0
//		Description:	  This is the prototype for a simple ALU simulator
//
//*****************************************************************************
//

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdarg.h>

#include <stdio.h>

#include "RegisterFile_01.h"
#include "ALUSimulator.h"

extern void ALUSimulator( RegisterFile theRegisterFile,
													uint32_t OpCode,
													uint32_t Rs,
													uint32_t Rt,
													uint32_t Rd,
													uint32_t ShiftAmt,
													uint32_t FunctionCode,
													uint32_t ImmediateValue,
													uint32_t* Status) {

	switch (OpCode) {
		case 0:
			switch (FunctionCode) {
				case 0: // SLL, NOOP
					Rd = Rt << ShiftAmt;
					break;
				case 2: // SRL
					Rd = Rt >> ShiftAmt;
					break;
				case 3: // SRA
					Rd = Rt >> ShiftAmt;
					break;
				case 4: // SLLV
					Rd = Rt << Rs;
					break;
				case 6: // SRLV
					Rd = Rt >> Rs;
					break;
				case 32: // ADD
					Rd = Rs + Rt;
					break;
				case 33: // ADDU
					Rd = Rs + Rt;
					break;
				case 34: //SUB
					Rd = Rs - Rt;
					break;
				case 35: //SUBU
					Rd = Rs - Rt;
					break;
				case 36: //AND
					break;
				case 37: //OR
					break;
				case 38: //XOR
					break;
				case 42: //SLT
					Rd = (Rs < Rt);
					break;
				case 43: //SLTU
					break;
				default:
					printf(">> ERROR: Unknown FunctionCode.");
			}
			break;
		case 8: // ADDI
			Rd = Rs + ImmediateValue;
			if ((Rs > 0 && (Rd < Rs || Rd < ImmediateValue)) ||
					((signed) Rs < 0 && (Rd > Rs || Rd > ImmediateValue))) {
				unsigned int status = 12;
				Status = &status;
			}
			break;
		case 9: // ADDIU
			Rd = Rs + ImmediateValue;
			break;
		case 10: // SLTI
			Rd = (Rs < ImmediateValue);
			break;
		case 11: // SLTIU
			Rd = ((unsigned) Rs < (unsigned) ImmediateValue);
			break;
		default:
			printf(">> ERROR: Unknown OpCode.");
	}

	RegisterFile_Write(theRegisterFile, true, Rt, Rd);

	// printf(">>ALU: Opcode: %02X; Rs: %02X; Rt: %02X; Rd: %02X;\n",
	// 			OpCode,
	// 			Rs,
	// 			Rt,
	// 			Rd);
	//
	// printf(">>>>ALU: ShiftAmt: %02X; FunctionCode: %02X; ImmediateValue: %04X;\n",
	// 			ShiftAmt,
	// 			FunctionCode,
	// 			ImmediateValue);

}
