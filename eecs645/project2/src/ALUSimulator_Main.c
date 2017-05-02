//*****************************************************************************
//--ALUSimulator_Main
//
//		Author: 		  Gary J. Minden
//		Organization:	KU/EECS/EECS 645
//		Date:			    2017-04-22 (B70422)
//		Version:		  1.1
//		Description:	This is the main program to manage and control
//						      a simple ALU simulator
//
//*****************************************************************************
//

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include "RegisterFile_01.h"
#include "ALUSimulator.h"

//
//	Define a structure to hold a decoded MIPS instruction.
//	Not all fields are valid for each instruction.
//	For example, the instruction is a R-type, the immediate
//		field is not valid.
//	The OpCode field determines the instruction type.
//
typedef struct MIPS_Instruction {
				uint32_t	OpCode;
				uint32_t Rs;
				uint32_t Rt;
				uint32_t Rd;
				uint32_t ShiftAmt;
				uint32_t FunctionCode;
				uint32_t ImmediateValue;
} MIPS_Instruction;

#define Instructions_Nbr 5

MIPS_Instruction MIPS_Instruction_Seq[Instructions_Nbr];

RegisterFile Primary_RegisterFile;

//
//*****************************************************************************
//
//	Defines for decoding a MIPS instruction. Only R-format and I-format
//	are handled for now.
//
#define   MIPS_Immediate_Exp    16
#define   MIPS_Immediate_Nbr    ( 1 << MIPS_Immediate_Exp )
#define   MIPS_Immediate_Mask   ( MIPS_Immediate_Nbr - 1 )
#define   MIPS_Immediate_Offset 0

#define   MIPS_Function_Exp     6
#define   MIPS_Function_Nbr     ( 1 << MIPS_Function_Exp )
#define   MIPS_Function_Mask    ( MIPS_Function_Nbr - 1 )
#define   MIPS_Function_Offset  0

#define   MIPS_Shift_Exp        5
#define   MIPS_Shift_Nbr        ( 1 << MIPS_Shift_Exp )
#define   MIPS_Shift_Mask       ( MIPS_Shift_Nbr - 1)
#define   MIPS_Shift_Offset     ( MIPS_Function_Exp )

#define   MIPS_Rd_Exp	          5
#define   MIPS_Rd_Nbr           ( 1 << MIPS_Rd_Exp )
#define   MIPS_Rd_Mask          ( MIPS_Rd_Nbr - 1)
#define   MIPS_Rd_Offset        ( MIPS_Shift_Offset + MIPS_Shift_Exp )

#define   MIPS_Rt_Exp				    5
#define   MIPS_Rt_Nbr           ( 1 << MIPS_Rt_Exp )
#define   MIPS_Rt_Mask          ( MIPS_Rt_Nbr - 1)
#define   MIPS_Rt_Offset        ( MIPS_Rd_Offset + MIPS_Rd_Exp )

#define   MIPS_Rs_Exp           5
#define   MIPS_Rs_Nbr           ( 1 << MIPS_Rs_Exp )
#define   MIPS_Rs_Mask          ( MIPS_Rs_Nbr - 1)
#define   MIPS_Rs_Offset        ( MIPS_Rt_Offset + MIPS_Rt_Exp )

#define   MIPS_OpCode_Exp       6
#define   MIPS_OpCode_Nbr       ( 1 << MIPS_OpCode_Exp )
#define   MIPS_OpCode_Mask      ( MIPS_OpCode_Nbr - 1)
#define   MIPS_OpCode_Offset    ( MIPS_Rs_Offset + MIPS_Rs_Exp )


extern void MIPS_Instruction_Dump( MIPS_Instruction theMIPSInstruction ) {
	printf( ">>Opcode: %02X; Rs: %02X; Rt: %02X; Rd: %02X;\n",
				theMIPSInstruction.OpCode,
				theMIPSInstruction.Rs,
				theMIPSInstruction.Rt,
				theMIPSInstruction.Rd );

	printf( ">>>>ShiftAmt: %02X; FunctionCode: %02X; ImmediateValue: %04X;\n",
				theMIPSInstruction.ShiftAmt,
				theMIPSInstruction.FunctionCode,
				theMIPSInstruction.ImmediateValue );

}

void MIPS_Offset_Report() {
	printf( ">>>>Immediate Offset: %d\n", MIPS_Function_Offset );
	printf( ">>>>Function Offset: %d\n", MIPS_Function_Offset );
	printf( ">>>>Shift Offset: %d\n", MIPS_Shift_Offset );
	printf( ">>>>Rd Offset: %d\n", MIPS_Rd_Offset );
	printf( ">>>>Rt Offset: %d\n", MIPS_Rt_Offset );
	printf( ">>>>Rs Offset: %d\n", MIPS_Rs_Offset );
	printf( ">>>>OpCode Offset: %d\n", MIPS_OpCode_Offset );
}

//
//	The variable "theMIPS_Instruction" is a 32-bit MIPS instruction.
//	The variable "theMIPSInstruction_Struct" is a structure
//		representing a MIPS instruction for simulation.
//

extern void MIPS_Decode(uint32_t theMIPS_Instruction,
												MIPS_Instruction* theMIPSInstruction_Struct ) {
	//
	//	This subroutine does not distinguish between
	//		a R-format or an I-format instruction.
	//
	//	First extract the Immediate portion of the instruction.
	//
	theMIPSInstruction_Struct->ImmediateValue = (theMIPS_Instruction & MIPS_Immediate_Mask);

	//
	//	Extract the remaining R-format fields
	//
	theMIPSInstruction_Struct->FunctionCode = (theMIPS_Instruction & MIPS_Function_Mask);

	theMIPSInstruction_Struct->ShiftAmt = ((theMIPS_Instruction >> MIPS_Shift_Offset) &
					MIPS_Shift_Mask);

	theMIPSInstruction_Struct->Rd = ((theMIPS_Instruction >> MIPS_Rd_Offset) &
					MIPS_Rd_Mask);

	theMIPSInstruction_Struct->Rt = ((theMIPS_Instruction >>  MIPS_Rt_Offset) &
					MIPS_Rt_Mask);

	theMIPSInstruction_Struct->Rs = ((theMIPS_Instruction >> MIPS_Rs_Offset) &
					MIPS_Rs_Mask);

	theMIPSInstruction_Struct->OpCode = ((theMIPS_Instruction >> MIPS_OpCode_Offset) &
					MIPS_OpCode_Mask);

  // MIPS_Instruction_Dump( *theMIPSInstruction_Struct );

}

//
//*****************************************************************************
//
int32_t main() {


	uint32_t	Files_Nbr = 3;
	uint32_t	Files_Idx;
	char*		  Filenames[] = { "instructions/MIPS_Instructions_01.txt" };
	FILE*		  MIPS_Iinstruction_File;
	uint32_t	FReadStatus;

	char		Test_String[128];
	uint32_t	aMIPS_Instruction;

	uint32_t	ALUStatus = 0;

  // MIPS_Offset_Report();

	//
	//	Load Primary_RegisterFile
	//
	RegisterFile_Write( Primary_RegisterFile, true, 0X01, 0X05 );
	RegisterFile_Write( Primary_RegisterFile, true, 0X02, 0X10 );
	RegisterFile_Write( Primary_RegisterFile, true, 0X03, 0X17 );
	RegisterFile_Write( Primary_RegisterFile, true, 0X04, 0X390 );
	RegisterFile_Write( Primary_RegisterFile, true, 0X05, 0X1010 );

	printf( "Initial RegisterFile: ========================================\n" );
	RegisterFile_Dump( Primary_RegisterFile );

	//
	//	Open MIPS instruction file
	//
	Files_Idx = 0;
	MIPS_Iinstruction_File = fopen( Filenames[Files_Idx], "r" );
	if ( MIPS_Iinstruction_File == NULL ) {
		printf( ">>>> File open error.\n" );
		return( 0 );
	}

  // printf( ">>>>File opened.\n" );

	while (1) {
		FReadStatus = fscanf( MIPS_Iinstruction_File, "%x", &aMIPS_Instruction );

		//
		//	Check for end of file
		//
		if ( FReadStatus == EOF ) {
			break;
		}

		printf( "Instruction: %08X\n", aMIPS_Instruction );
		MIPS_Decode( aMIPS_Instruction, &MIPS_Instruction_Seq[0] );
		MIPS_Instruction_Dump( MIPS_Instruction_Seq[0] );

		ALUSimulator( Primary_RegisterFile,
									MIPS_Instruction_Seq[0].OpCode,
									MIPS_Instruction_Seq[0].Rs,
									MIPS_Instruction_Seq[0].Rt,
									MIPS_Instruction_Seq[0].Rd,
									MIPS_Instruction_Seq[0].ShiftAmt,
									MIPS_Instruction_Seq[0].FunctionCode,
									MIPS_Instruction_Seq[0].ImmediateValue,
									&ALUStatus );

	}

	fclose( MIPS_Iinstruction_File );

	printf( "Final RegisterFile: ========================================\n" );
	RegisterFile_Dump( Primary_RegisterFile );
}
