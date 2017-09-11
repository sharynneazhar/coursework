
a.out:     file format elf64-x86-64


Disassembly of section .interp:

0000000000400238 <.interp>:
  400238:	2f                   	(bad)  
  400239:	6c                   	insb   (%dx),%es:(%rdi)
  40023a:	69 62 36 34 2f 6c 64 	imul   $0x646c2f34,0x36(%rdx),%esp
  400241:	2d 6c 69 6e 75       	sub    $0x756e696c,%eax
  400246:	78 2d                	js     400275 <_init-0x2eb>
  400248:	78 38                	js     400282 <_init-0x2de>
  40024a:	36 2d 36 34 2e 73    	ss sub $0x732e3436,%eax
  400250:	6f                   	outsl  %ds:(%rsi),(%dx)
  400251:	2e 32 00             	xor    %cs:(%rax),%al

Disassembly of section .note.ABI-tag:

0000000000400254 <.note.ABI-tag>:
  400254:	04 00                	add    $0x0,%al
  400256:	00 00                	add    %al,(%rax)
  400258:	10 00                	adc    %al,(%rax)
  40025a:	00 00                	add    %al,(%rax)
  40025c:	01 00                	add    %eax,(%rax)
  40025e:	00 00                	add    %al,(%rax)
  400260:	47                   	rex.RXB
  400261:	4e 55                	rex.WRX push %rbp
  400263:	00 00                	add    %al,(%rax)
  400265:	00 00                	add    %al,(%rax)
  400267:	00 02                	add    %al,(%rdx)
  400269:	00 00                	add    %al,(%rax)
  40026b:	00 06                	add    %al,(%rsi)
  40026d:	00 00                	add    %al,(%rax)
  40026f:	00 20                	add    %ah,(%rax)
  400271:	00 00                	add    %al,(%rax)
	...

Disassembly of section .note.gnu.build-id:

0000000000400274 <.note.gnu.build-id>:
  400274:	04 00                	add    $0x0,%al
  400276:	00 00                	add    %al,(%rax)
  400278:	14 00                	adc    $0x0,%al
  40027a:	00 00                	add    %al,(%rax)
  40027c:	03 00                	add    (%rax),%eax
  40027e:	00 00                	add    %al,(%rax)
  400280:	47                   	rex.RXB
  400281:	4e 55                	rex.WRX push %rbp
  400283:	00 29                	add    %ch,(%rcx)
  400285:	e9 fe 58 6a 82       	jmpq   ffffffff82aa5b88 <__TMC_END__+0xffffffff824a4b50>
  40028a:	3f                   	(bad)  
  40028b:	e8 89 ee c4 3f       	callq  4004f119 <__TMC_END__+0x3fa4e0e1>
  400290:	73 a0                	jae    400232 <_init-0x32e>
  400292:	de f1                	fdivp  %st,%st(1)
  400294:	1f                   	(bad)  
  400295:	df                   	.byte 0xdf
  400296:	ab                   	stos   %eax,%es:(%rdi)
  400297:	37                   	(bad)  

Disassembly of section .gnu.hash:

0000000000400298 <.gnu.hash>:
  400298:	03 00                	add    (%rax),%eax
  40029a:	00 00                	add    %al,(%rax)
  40029c:	08 00                	or     %al,(%rax)
  40029e:	00 00                	add    %al,(%rax)
  4002a0:	01 00                	add    %eax,(%rax)
  4002a2:	00 00                	add    %al,(%rax)
  4002a4:	06                   	(bad)  
  4002a5:	00 00                	add    %al,(%rax)
  4002a7:	00 88 c0 20 01 00    	add    %cl,0x120c0(%rax)
  4002ad:	04 40                	add    $0x40,%al
  4002af:	09 08                	or     %ecx,(%rax)
  4002b1:	00 00                	add    %al,(%rax)
  4002b3:	00 0a                	add    %cl,(%rdx)
  4002b5:	00 00                	add    %al,(%rax)
  4002b7:	00 0c 00             	add    %cl,(%rax,%rax,1)
  4002ba:	00 00                	add    %al,(%rax)
  4002bc:	42                   	rex.X
  4002bd:	45 d5                	rex.RB (bad) 
  4002bf:	ec                   	in     (%dx),%al
  4002c0:	bb e3 92 7c d8       	mov    $0xd87c92e3,%ebx
  4002c5:	71 58                	jno    40031f <_init-0x241>
  4002c7:	1c b9                	sbb    $0xb9,%al
  4002c9:	8d                   	(bad)  
  4002ca:	f1                   	icebp  
  4002cb:	0e                   	(bad)  
  4002cc:	eb d3                	jmp    4002a1 <_init-0x2bf>
  4002ce:	ef                   	out    %eax,(%dx)
  4002cf:	0e                   	(bad)  

Disassembly of section .dynsym:

00000000004002d0 <.dynsym>:
	...
  4002e8:	0b 00                	or     (%rax),%eax
  4002ea:	00 00                	add    %al,(%rax)
  4002ec:	20 00                	and    %al,(%rax)
	...
  4002fe:	00 00                	add    %al,(%rax)
  400300:	81 00 00 00 12 00    	addl   $0x120000,(%rax)
	...
  400316:	00 00                	add    %al,(%rax)
  400318:	64 00 00             	add    %al,%fs:(%rax)
  40031b:	00 12                	add    %dl,(%rdx)
	...
  40032d:	00 00                	add    %al,(%rax)
  40032f:	00 88 00 00 00 12    	add    %cl,0x12000000(%rax)
	...
  400345:	00 00                	add    %al,(%rax)
  400347:	00 27                	add    %ah,(%rdi)
  400349:	00 00                	add    %al,(%rax)
  40034b:	00 20                	add    %ah,(%rax)
	...
  40035d:	00 00                	add    %al,(%rax)
  40035f:	00 36                	add    %dh,(%rsi)
  400361:	00 00                	add    %al,(%rax)
  400363:	00 20                	add    %ah,(%rax)
	...
  400375:	00 00                	add    %al,(%rax)
  400377:	00 4a 00             	add    %cl,0x0(%rdx)
  40037a:	00 00                	add    %al,(%rax)
  40037c:	20 00                	and    %al,(%rax)
	...
  40038e:	00 00                	add    %al,(%rax)
  400390:	9a                   	(bad)  
  400391:	00 00                	add    %al,(%rax)
  400393:	00 10                	add    %dl,(%rax)
  400395:	00 19                	add    %bl,(%rcx)
  400397:	00 34 10             	add    %dh,(%rax,%rdx,1)
  40039a:	60                   	(bad)  
	...
  4003a7:	00 ad 00 00 00 10    	add    %ch,0x10000000(%rbp)
  4003ad:	00 1a                	add    %bl,(%rdx)
  4003af:	00 38                	add    %bh,(%rax)
  4003b1:	10 60 00             	adc    %ah,0x0(%rax)
	...
  4003c0:	a1 00 00 00 10 00 1a 	movabs 0x34001a0010000000,%eax
  4003c7:	00 34 
  4003c9:	10 60 00             	adc    %ah,0x0(%rax)
	...
  4003d8:	6b 00 00             	imul   $0x0,(%rax),%eax
  4003db:	00 12                	add    %dl,(%rdx)
  4003dd:	00 0b                	add    %cl,(%rbx)
  4003df:	00 60 05             	add    %ah,0x5(%rax)
  4003e2:	40 00 00             	add    %al,(%rax)
	...
  4003ed:	00 00                	add    %al,(%rax)
  4003ef:	00 71 00             	add    %dh,0x0(%rcx)
  4003f2:	00 00                	add    %al,(%rax)
  4003f4:	12 00                	adc    (%rax),%al
  4003f6:	0f 00 94 07 40 00 00 	lldt   0x40(%rdi,%rax,1)
  4003fd:	00 
	...

Disassembly of section .dynstr:

0000000000400408 <.dynstr>:
  400408:	00 6c 69 62          	add    %ch,0x62(%rcx,%rbp,2)
  40040c:	6d                   	insl   (%dx),%es:(%rdi)
  40040d:	75 6c                	jne    40047b <_init-0xe5>
  40040f:	2e 73 6f             	jae,pn 400481 <_init-0xdf>
  400412:	00 5f 49             	add    %bl,0x49(%rdi)
  400415:	54                   	push   %rsp
  400416:	4d 5f                	rex.WRB pop %r15
  400418:	64 65 72 65          	fs gs jb 400481 <_init-0xdf>
  40041c:	67 69 73 74 65 72 54 	imul   $0x4d547265,0x74(%ebx),%esi
  400423:	4d 
  400424:	43 6c                	rex.XB insb (%dx),%es:(%rdi)
  400426:	6f                   	outsl  %ds:(%rsi),(%dx)
  400427:	6e                   	outsb  %ds:(%rsi),(%dx)
  400428:	65 54                	gs push %rsp
  40042a:	61                   	(bad)  
  40042b:	62                   	(bad)  
  40042c:	6c                   	insb   (%dx),%es:(%rdi)
  40042d:	65 00 5f 5f          	add    %bl,%gs:0x5f(%rdi)
  400431:	67 6d                	insl   (%dx),%es:(%edi)
  400433:	6f                   	outsl  %ds:(%rsi),(%dx)
  400434:	6e                   	outsb  %ds:(%rsi),(%dx)
  400435:	5f                   	pop    %rdi
  400436:	73 74                	jae    4004ac <_init-0xb4>
  400438:	61                   	(bad)  
  400439:	72 74                	jb     4004af <_init-0xb1>
  40043b:	5f                   	pop    %rdi
  40043c:	5f                   	pop    %rdi
  40043d:	00 5f 4a             	add    %bl,0x4a(%rdi)
  400440:	76 5f                	jbe    4004a1 <_init-0xbf>
  400442:	52                   	push   %rdx
  400443:	65 67 69 73 74 65 72 	imul   $0x6c437265,%gs:0x74(%ebx),%esi
  40044a:	43 6c 
  40044c:	61                   	(bad)  
  40044d:	73 73                	jae    4004c2 <_init-0x9e>
  40044f:	65 73 00             	gs jae 400452 <_init-0x10e>
  400452:	5f                   	pop    %rdi
  400453:	49 54                	rex.WB push %r12
  400455:	4d 5f                	rex.WRB pop %r15
  400457:	72 65                	jb     4004be <_init-0xa2>
  400459:	67 69 73 74 65 72 54 	imul   $0x4d547265,0x74(%ebx),%esi
  400460:	4d 
  400461:	43 6c                	rex.XB insb (%dx),%es:(%rdi)
  400463:	6f                   	outsl  %ds:(%rsi),(%dx)
  400464:	6e                   	outsb  %ds:(%rsi),(%dx)
  400465:	65 54                	gs push %rsp
  400467:	61                   	(bad)  
  400468:	62                   	(bad)  
  400469:	6c                   	insb   (%dx),%es:(%rdi)
  40046a:	65 00 6d 79          	add    %ch,%gs:0x79(%rbp)
  40046e:	5f                   	pop    %rdi
  40046f:	6d                   	insl   (%dx),%es:(%rdi)
  400470:	75 6c                	jne    4004de <_init-0x82>
  400472:	00 5f 69             	add    %bl,0x69(%rdi)
  400475:	6e                   	outsb  %ds:(%rsi),(%dx)
  400476:	69 74 00 5f 66 69 6e 	imul   $0x696e6966,0x5f(%rax,%rax,1),%esi
  40047d:	69 
  40047e:	00 6c 69 62          	add    %ch,0x62(%rcx,%rbp,2)
  400482:	63 2e                	movslq (%rsi),%ebp
  400484:	73 6f                	jae    4004f5 <_init-0x6b>
  400486:	2e 36 00 70 72       	cs add %dh,%ss:0x72(%rax)
  40048b:	69 6e 74 66 00 5f 5f 	imul   $0x5f5f0066,0x74(%rsi),%ebp
  400492:	6c                   	insb   (%dx),%es:(%rdi)
  400493:	69 62 63 5f 73 74 61 	imul   $0x6174735f,0x63(%rdx),%esp
  40049a:	72 74                	jb     400510 <_init-0x50>
  40049c:	5f                   	pop    %rdi
  40049d:	6d                   	insl   (%dx),%es:(%rdi)
  40049e:	61                   	(bad)  
  40049f:	69 6e 00 5f 65 64 61 	imul   $0x6164655f,0x0(%rsi),%ebp
  4004a6:	74 61                	je     400509 <_init-0x57>
  4004a8:	00 5f 5f             	add    %bl,0x5f(%rdi)
  4004ab:	62 73                	(bad)  
  4004ad:	73 5f                	jae    40050e <_init-0x52>
  4004af:	73 74                	jae    400525 <_init-0x3b>
  4004b1:	61                   	(bad)  
  4004b2:	72 74                	jb     400528 <_init-0x38>
  4004b4:	00 5f 65             	add    %bl,0x65(%rdi)
  4004b7:	6e                   	outsb  %ds:(%rsi),(%dx)
  4004b8:	64 00 47 4c          	add    %al,%fs:0x4c(%rdi)
  4004bc:	49                   	rex.WB
  4004bd:	42                   	rex.X
  4004be:	43 5f                	rex.XB pop %r15
  4004c0:	32 2e                	xor    (%rsi),%ch
  4004c2:	32 2e                	xor    (%rsi),%ch
  4004c4:	35                   	.byte 0x35
	...

Disassembly of section .gnu.version:

00000000004004c6 <.gnu.version>:
  4004c6:	00 00                	add    %al,(%rax)
  4004c8:	00 00                	add    %al,(%rax)
  4004ca:	02 00                	add    (%rax),%al
  4004cc:	00 00                	add    %al,(%rax)
  4004ce:	02 00                	add    (%rax),%al
  4004d0:	00 00                	add    %al,(%rax)
  4004d2:	00 00                	add    %al,(%rax)
  4004d4:	00 00                	add    %al,(%rax)
  4004d6:	01 00                	add    %eax,(%rax)
  4004d8:	01 00                	add    %eax,(%rax)
  4004da:	01 00                	add    %eax,(%rax)
  4004dc:	01 00                	add    %eax,(%rax)
  4004de:	01 00                	add    %eax,(%rax)

Disassembly of section .gnu.version_r:

00000000004004e0 <.gnu.version_r>:
  4004e0:	01 00                	add    %eax,(%rax)
  4004e2:	01 00                	add    %eax,(%rax)
  4004e4:	77 00                	ja     4004e6 <_init-0x7a>
  4004e6:	00 00                	add    %al,(%rax)
  4004e8:	10 00                	adc    %al,(%rax)
  4004ea:	00 00                	add    %al,(%rax)
  4004ec:	00 00                	add    %al,(%rax)
  4004ee:	00 00                	add    %al,(%rax)
  4004f0:	75 1a                	jne    40050c <_init-0x54>
  4004f2:	69 09 00 00 02 00    	imul   $0x20000,(%rcx),%ecx
  4004f8:	b2 00                	mov    $0x0,%dl
  4004fa:	00 00                	add    %al,(%rax)
  4004fc:	00 00                	add    %al,(%rax)
	...

Disassembly of section .rela.dyn:

0000000000400500 <.rela.dyn>:
  400500:	f8                   	clc    
  400501:	0f 60 00             	punpcklbw (%rax),%mm0
  400504:	00 00                	add    %al,(%rax)
  400506:	00 00                	add    %al,(%rax)
  400508:	06                   	(bad)  
  400509:	00 00                	add    %al,(%rax)
  40050b:	00 05 00 00 00 00    	add    %al,0x0(%rip)        # 400511 <_init-0x4f>
  400511:	00 00                	add    %al,(%rax)
  400513:	00 00                	add    %al,(%rax)
  400515:	00 00                	add    %al,(%rax)
	...

Disassembly of section .rela.plt:

0000000000400518 <.rela.plt>:
  400518:	18 10                	sbb    %dl,(%rax)
  40051a:	60                   	(bad)  
  40051b:	00 00                	add    %al,(%rax)
  40051d:	00 00                	add    %al,(%rax)
  40051f:	00 07                	add    %al,(%rdi)
  400521:	00 00                	add    %al,(%rax)
  400523:	00 02                	add    %al,(%rdx)
	...
  40052d:	00 00                	add    %al,(%rax)
  40052f:	00 20                	add    %ah,(%rax)
  400531:	10 60 00             	adc    %ah,0x0(%rax)
  400534:	00 00                	add    %al,(%rax)
  400536:	00 00                	add    %al,(%rax)
  400538:	07                   	(bad)  
  400539:	00 00                	add    %al,(%rax)
  40053b:	00 03                	add    %al,(%rbx)
	...
  400545:	00 00                	add    %al,(%rax)
  400547:	00 28                	add    %ch,(%rax)
  400549:	10 60 00             	adc    %ah,0x0(%rax)
  40054c:	00 00                	add    %al,(%rax)
  40054e:	00 00                	add    %al,(%rax)
  400550:	07                   	(bad)  
  400551:	00 00                	add    %al,(%rax)
  400553:	00 04 00             	add    %al,(%rax,%rax,1)
	...

Disassembly of section .init:

0000000000400560 <_init>:
  400560:	48 83 ec 08          	sub    $0x8,%rsp
  400564:	48 8b 05 8d 0a 20 00 	mov    0x200a8d(%rip),%rax        # 600ff8 <_DYNAMIC+0x1e0>
  40056b:	48 85 c0             	test   %rax,%rax
  40056e:	74 05                	je     400575 <_init+0x15>
  400570:	e8 4b 00 00 00       	callq  4005c0 <__libc_start_main@plt+0x10>
  400575:	48 83 c4 08          	add    $0x8,%rsp
  400579:	c3                   	retq   

Disassembly of section .plt:

0000000000400580 <printf@plt-0x10>:
  400580:	ff 35 82 0a 20 00    	pushq  0x200a82(%rip)        # 601008 <_GLOBAL_OFFSET_TABLE_+0x8>
  400586:	ff 25 84 0a 20 00    	jmpq   *0x200a84(%rip)        # 601010 <_GLOBAL_OFFSET_TABLE_+0x10>
  40058c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000400590 <printf@plt>:
  400590:	ff 25 82 0a 20 00    	jmpq   *0x200a82(%rip)        # 601018 <_GLOBAL_OFFSET_TABLE_+0x18>
  400596:	68 00 00 00 00       	pushq  $0x0
  40059b:	e9 e0 ff ff ff       	jmpq   400580 <_init+0x20>

00000000004005a0 <my_mul@plt>:
  4005a0:	ff 25 7a 0a 20 00    	jmpq   *0x200a7a(%rip)        # 601020 <_GLOBAL_OFFSET_TABLE_+0x20>
  4005a6:	68 01 00 00 00       	pushq  $0x1
  4005ab:	e9 d0 ff ff ff       	jmpq   400580 <_init+0x20>

00000000004005b0 <__libc_start_main@plt>:
  4005b0:	ff 25 72 0a 20 00    	jmpq   *0x200a72(%rip)        # 601028 <_GLOBAL_OFFSET_TABLE_+0x28>
  4005b6:	68 02 00 00 00       	pushq  $0x2
  4005bb:	e9 c0 ff ff ff       	jmpq   400580 <_init+0x20>

Disassembly of section .plt.got:

00000000004005c0 <.plt.got>:
  4005c0:	ff 25 32 0a 20 00    	jmpq   *0x200a32(%rip)        # 600ff8 <_DYNAMIC+0x1e0>
  4005c6:	66 90                	xchg   %ax,%ax

Disassembly of section .text:

00000000004005d0 <_start>:
  4005d0:	31 ed                	xor    %ebp,%ebp
  4005d2:	49 89 d1             	mov    %rdx,%r9
  4005d5:	5e                   	pop    %rsi
  4005d6:	48 89 e2             	mov    %rsp,%rdx
  4005d9:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
  4005dd:	50                   	push   %rax
  4005de:	54                   	push   %rsp
  4005df:	49 c7 c0 90 07 40 00 	mov    $0x400790,%r8
  4005e6:	48 c7 c1 20 07 40 00 	mov    $0x400720,%rcx
  4005ed:	48 c7 c7 d1 06 40 00 	mov    $0x4006d1,%rdi
  4005f4:	e8 b7 ff ff ff       	callq  4005b0 <__libc_start_main@plt>
  4005f9:	f4                   	hlt    
  4005fa:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000400600 <deregister_tm_clones>:
  400600:	b8 3f 10 60 00       	mov    $0x60103f,%eax
  400605:	55                   	push   %rbp
  400606:	48 2d 38 10 60 00    	sub    $0x601038,%rax
  40060c:	48 83 f8 0e          	cmp    $0xe,%rax
  400610:	48 89 e5             	mov    %rsp,%rbp
  400613:	76 1b                	jbe    400630 <deregister_tm_clones+0x30>
  400615:	b8 00 00 00 00       	mov    $0x0,%eax
  40061a:	48 85 c0             	test   %rax,%rax
  40061d:	74 11                	je     400630 <deregister_tm_clones+0x30>
  40061f:	5d                   	pop    %rbp
  400620:	bf 38 10 60 00       	mov    $0x601038,%edi
  400625:	ff e0                	jmpq   *%rax
  400627:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40062e:	00 00 
  400630:	5d                   	pop    %rbp
  400631:	c3                   	retq   
  400632:	0f 1f 40 00          	nopl   0x0(%rax)
  400636:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  40063d:	00 00 00 

0000000000400640 <register_tm_clones>:
  400640:	be 38 10 60 00       	mov    $0x601038,%esi
  400645:	55                   	push   %rbp
  400646:	48 81 ee 38 10 60 00 	sub    $0x601038,%rsi
  40064d:	48 c1 fe 03          	sar    $0x3,%rsi
  400651:	48 89 e5             	mov    %rsp,%rbp
  400654:	48 89 f0             	mov    %rsi,%rax
  400657:	48 c1 e8 3f          	shr    $0x3f,%rax
  40065b:	48 01 c6             	add    %rax,%rsi
  40065e:	48 d1 fe             	sar    %rsi
  400661:	74 15                	je     400678 <register_tm_clones+0x38>
  400663:	b8 00 00 00 00       	mov    $0x0,%eax
  400668:	48 85 c0             	test   %rax,%rax
  40066b:	74 0b                	je     400678 <register_tm_clones+0x38>
  40066d:	5d                   	pop    %rbp
  40066e:	bf 38 10 60 00       	mov    $0x601038,%edi
  400673:	ff e0                	jmpq   *%rax
  400675:	0f 1f 00             	nopl   (%rax)
  400678:	5d                   	pop    %rbp
  400679:	c3                   	retq   
  40067a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000400680 <__do_global_dtors_aux>:
  400680:	80 3d ad 09 20 00 00 	cmpb   $0x0,0x2009ad(%rip)        # 601034 <_edata>
  400687:	75 11                	jne    40069a <__do_global_dtors_aux+0x1a>
  400689:	55                   	push   %rbp
  40068a:	48 89 e5             	mov    %rsp,%rbp
  40068d:	e8 6e ff ff ff       	callq  400600 <deregister_tm_clones>
  400692:	5d                   	pop    %rbp
  400693:	c6 05 9a 09 20 00 01 	movb   $0x1,0x20099a(%rip)        # 601034 <_edata>
  40069a:	f3 c3                	repz retq 
  40069c:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004006a0 <frame_dummy>:
  4006a0:	bf 10 0e 60 00       	mov    $0x600e10,%edi
  4006a5:	48 83 3f 00          	cmpq   $0x0,(%rdi)
  4006a9:	75 05                	jne    4006b0 <frame_dummy+0x10>
  4006ab:	eb 93                	jmp    400640 <register_tm_clones>
  4006ad:	0f 1f 00             	nopl   (%rax)
  4006b0:	b8 00 00 00 00       	mov    $0x0,%eax
  4006b5:	48 85 c0             	test   %rax,%rax
  4006b8:	74 f1                	je     4006ab <frame_dummy+0xb>
  4006ba:	55                   	push   %rbp
  4006bb:	48 89 e5             	mov    %rsp,%rbp
  4006be:	ff d0                	callq  *%rax
  4006c0:	5d                   	pop    %rbp
  4006c1:	e9 7a ff ff ff       	jmpq   400640 <register_tm_clones>

00000000004006c6 <foo>:
  4006c6:	55                   	push   %rbp
  4006c7:	48 89 e5             	mov    %rsp,%rbp
  4006ca:	b8 02 00 00 00       	mov    $0x2,%eax
  4006cf:	5d                   	pop    %rbp
  4006d0:	c3                   	retq   

00000000004006d1 <main>:
  4006d1:	55                   	push   %rbp
  4006d2:	48 89 e5             	mov    %rsp,%rbp
  4006d5:	48 83 ec 10          	sub    $0x10,%rsp
  4006d9:	c7 45 f0 0a 00 00 00 	movl   $0xa,-0x10(%rbp)
  4006e0:	b8 00 00 00 00       	mov    $0x0,%eax
  4006e5:	e8 dc ff ff ff       	callq  4006c6 <foo>
  4006ea:	89 45 f4             	mov    %eax,-0xc(%rbp)
  4006ed:	8b 55 f4             	mov    -0xc(%rbp),%edx
  4006f0:	8b 45 f0             	mov    -0x10(%rbp),%eax
  4006f3:	89 d6                	mov    %edx,%esi
  4006f5:	89 c7                	mov    %eax,%edi
  4006f7:	e8 a4 fe ff ff       	callq  4005a0 <my_mul@plt>
  4006fc:	89 45 fc             	mov    %eax,-0x4(%rbp)
  4006ff:	8b 55 f4             	mov    -0xc(%rbp),%edx
  400702:	8b 45 f0             	mov    -0x10(%rbp),%eax
  400705:	8b 4d fc             	mov    -0x4(%rbp),%ecx
  400708:	89 c6                	mov    %eax,%esi
  40070a:	bf b0 07 40 00       	mov    $0x4007b0,%edi
  40070f:	b8 00 00 00 00       	mov    $0x0,%eax
  400714:	e8 77 fe ff ff       	callq  400590 <printf@plt>
  400719:	b8 00 00 00 00       	mov    $0x0,%eax
  40071e:	c9                   	leaveq 
  40071f:	c3                   	retq   

0000000000400720 <__libc_csu_init>:
  400720:	41 57                	push   %r15
  400722:	41 56                	push   %r14
  400724:	41 89 ff             	mov    %edi,%r15d
  400727:	41 55                	push   %r13
  400729:	41 54                	push   %r12
  40072b:	4c 8d 25 ce 06 20 00 	lea    0x2006ce(%rip),%r12        # 600e00 <__frame_dummy_init_array_entry>
  400732:	55                   	push   %rbp
  400733:	48 8d 2d ce 06 20 00 	lea    0x2006ce(%rip),%rbp        # 600e08 <__init_array_end>
  40073a:	53                   	push   %rbx
  40073b:	49 89 f6             	mov    %rsi,%r14
  40073e:	49 89 d5             	mov    %rdx,%r13
  400741:	4c 29 e5             	sub    %r12,%rbp
  400744:	48 83 ec 08          	sub    $0x8,%rsp
  400748:	48 c1 fd 03          	sar    $0x3,%rbp
  40074c:	e8 0f fe ff ff       	callq  400560 <_init>
  400751:	48 85 ed             	test   %rbp,%rbp
  400754:	74 20                	je     400776 <__libc_csu_init+0x56>
  400756:	31 db                	xor    %ebx,%ebx
  400758:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40075f:	00 
  400760:	4c 89 ea             	mov    %r13,%rdx
  400763:	4c 89 f6             	mov    %r14,%rsi
  400766:	44 89 ff             	mov    %r15d,%edi
  400769:	41 ff 14 dc          	callq  *(%r12,%rbx,8)
  40076d:	48 83 c3 01          	add    $0x1,%rbx
  400771:	48 39 dd             	cmp    %rbx,%rbp
  400774:	75 ea                	jne    400760 <__libc_csu_init+0x40>
  400776:	48 83 c4 08          	add    $0x8,%rsp
  40077a:	5b                   	pop    %rbx
  40077b:	5d                   	pop    %rbp
  40077c:	41 5c                	pop    %r12
  40077e:	41 5d                	pop    %r13
  400780:	41 5e                	pop    %r14
  400782:	41 5f                	pop    %r15
  400784:	c3                   	retq   
  400785:	90                   	nop
  400786:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  40078d:	00 00 00 

0000000000400790 <__libc_csu_fini>:
  400790:	f3 c3                	repz retq 

Disassembly of section .fini:

0000000000400794 <_fini>:
  400794:	48 83 ec 08          	sub    $0x8,%rsp
  400798:	48 83 c4 08          	add    $0x8,%rsp
  40079c:	c3                   	retq   

Disassembly of section .rodata:

00000000004007a0 <_IO_stdin_used>:
  4007a0:	01 00                	add    %eax,(%rax)
  4007a2:	02 00                	add    (%rax),%al
  4007a4:	00 00                	add    %al,(%rax)
	...

00000000004007a8 <__dso_handle>:
	...
  4007b0:	25 64 20 74 6f       	and    $0x6f742064,%eax
  4007b5:	20 74 68 65          	and    %dh,0x65(%rax,%rbp,2)
  4007b9:	20 70 6f             	and    %dh,0x6f(%rax)
  4007bc:	77 65                	ja     400823 <__GNU_EH_FRAME_HDR+0x57>
  4007be:	72 20                	jb     4007e0 <__GNU_EH_FRAME_HDR+0x14>
  4007c0:	25 64 20 3d 20       	and    $0x203d2064,%eax
  4007c5:	25                   	.byte 0x25
  4007c6:	64 0a 00             	or     %fs:(%rax),%al

Disassembly of section .eh_frame_hdr:

00000000004007cc <__GNU_EH_FRAME_HDR>:
  4007cc:	01 1b                	add    %ebx,(%rbx)
  4007ce:	03 3b                	add    (%rbx),%edi
  4007d0:	38 00                	cmp    %al,(%rax)
  4007d2:	00 00                	add    %al,(%rax)
  4007d4:	06                   	(bad)  
  4007d5:	00 00                	add    %al,(%rax)
  4007d7:	00 b4 fd ff ff 84 00 	add    %dh,0x84ffff(%rbp,%rdi,8)
  4007de:	00 00                	add    %al,(%rax)
  4007e0:	04 fe                	add    $0xfe,%al
  4007e2:	ff                   	(bad)  
  4007e3:	ff 54 00 00          	callq  *0x0(%rax,%rax,1)
  4007e7:	00 fa                	add    %bh,%dl
  4007e9:	fe                   	(bad)  
  4007ea:	ff                   	(bad)  
  4007eb:	ff ac 00 00 00 05 ff 	ljmp   *-0xfb0000(%rax,%rax,1)
  4007f2:	ff                   	(bad)  
  4007f3:	ff cc                	dec    %esp
  4007f5:	00 00                	add    %al,(%rax)
  4007f7:	00 54 ff ff          	add    %dl,-0x1(%rdi,%rdi,8)
  4007fb:	ff                   	(bad)  
  4007fc:	ec                   	in     (%dx),%al
  4007fd:	00 00                	add    %al,(%rax)
  4007ff:	00 c4                	add    %al,%ah
  400801:	ff                   	(bad)  
  400802:	ff                   	(bad)  
  400803:	ff 34 01             	pushq  (%rcx,%rax,1)
	...

Disassembly of section .eh_frame:

0000000000400808 <__FRAME_END__-0x110>:
  400808:	14 00                	adc    $0x0,%al
  40080a:	00 00                	add    %al,(%rax)
  40080c:	00 00                	add    %al,(%rax)
  40080e:	00 00                	add    %al,(%rax)
  400810:	01 7a 52             	add    %edi,0x52(%rdx)
  400813:	00 01                	add    %al,(%rcx)
  400815:	78 10                	js     400827 <__GNU_EH_FRAME_HDR+0x5b>
  400817:	01 1b                	add    %ebx,(%rbx)
  400819:	0c 07                	or     $0x7,%al
  40081b:	08 90 01 07 10 14    	or     %dl,0x14100701(%rax)
  400821:	00 00                	add    %al,(%rax)
  400823:	00 1c 00             	add    %bl,(%rax,%rax,1)
  400826:	00 00                	add    %al,(%rax)
  400828:	a8 fd                	test   $0xfd,%al
  40082a:	ff                   	(bad)  
  40082b:	ff 2a                	ljmp   *(%rdx)
	...
  400835:	00 00                	add    %al,(%rax)
  400837:	00 14 00             	add    %dl,(%rax,%rax,1)
  40083a:	00 00                	add    %al,(%rax)
  40083c:	00 00                	add    %al,(%rax)
  40083e:	00 00                	add    %al,(%rax)
  400840:	01 7a 52             	add    %edi,0x52(%rdx)
  400843:	00 01                	add    %al,(%rcx)
  400845:	78 10                	js     400857 <__GNU_EH_FRAME_HDR+0x8b>
  400847:	01 1b                	add    %ebx,(%rbx)
  400849:	0c 07                	or     $0x7,%al
  40084b:	08 90 01 00 00 24    	or     %dl,0x24000001(%rax)
  400851:	00 00                	add    %al,(%rax)
  400853:	00 1c 00             	add    %bl,(%rax,%rax,1)
  400856:	00 00                	add    %al,(%rax)
  400858:	28 fd                	sub    %bh,%ch
  40085a:	ff                   	(bad)  
  40085b:	ff 40 00             	incl   0x0(%rax)
  40085e:	00 00                	add    %al,(%rax)
  400860:	00 0e                	add    %cl,(%rsi)
  400862:	10 46 0e             	adc    %al,0xe(%rsi)
  400865:	18 4a 0f             	sbb    %cl,0xf(%rdx)
  400868:	0b 77 08             	or     0x8(%rdi),%esi
  40086b:	80 00 3f             	addb   $0x3f,(%rax)
  40086e:	1a 3b                	sbb    (%rbx),%bh
  400870:	2a 33                	sub    (%rbx),%dh
  400872:	24 22                	and    $0x22,%al
  400874:	00 00                	add    %al,(%rax)
  400876:	00 00                	add    %al,(%rax)
  400878:	1c 00                	sbb    $0x0,%al
  40087a:	00 00                	add    %al,(%rax)
  40087c:	44 00 00             	add    %r8b,(%rax)
  40087f:	00 46 fe             	add    %al,-0x2(%rsi)
  400882:	ff                   	(bad)  
  400883:	ff 0b                	decl   (%rbx)
  400885:	00 00                	add    %al,(%rax)
  400887:	00 00                	add    %al,(%rax)
  400889:	41 0e                	rex.B (bad) 
  40088b:	10 86 02 43 0d 06    	adc    %al,0x60d4302(%rsi)
  400891:	46 0c 07             	rex.RX or $0x7,%al
  400894:	08 00                	or     %al,(%rax)
  400896:	00 00                	add    %al,(%rax)
  400898:	1c 00                	sbb    $0x0,%al
  40089a:	00 00                	add    %al,(%rax)
  40089c:	64 00 00             	add    %al,%fs:(%rax)
  40089f:	00 31                	add    %dh,(%rcx)
  4008a1:	fe                   	(bad)  
  4008a2:	ff                   	(bad)  
  4008a3:	ff 4f 00             	decl   0x0(%rdi)
  4008a6:	00 00                	add    %al,(%rax)
  4008a8:	00 41 0e             	add    %al,0xe(%rcx)
  4008ab:	10 86 02 43 0d 06    	adc    %al,0x60d4302(%rsi)
  4008b1:	02 4a 0c             	add    0xc(%rdx),%cl
  4008b4:	07                   	(bad)  
  4008b5:	08 00                	or     %al,(%rax)
  4008b7:	00 44 00 00          	add    %al,0x0(%rax,%rax,1)
  4008bb:	00 84 00 00 00 60 fe 	add    %al,-0x1a00000(%rax,%rax,1)
  4008c2:	ff                   	(bad)  
  4008c3:	ff 65 00             	jmpq   *0x0(%rbp)
  4008c6:	00 00                	add    %al,(%rax)
  4008c8:	00 42 0e             	add    %al,0xe(%rdx)
  4008cb:	10 8f 02 42 0e 18    	adc    %cl,0x180e4202(%rdi)
  4008d1:	8e 03                	mov    (%rbx),%es
  4008d3:	45 0e                	rex.RB (bad) 
  4008d5:	20 8d 04 42 0e 28    	and    %cl,0x280e4204(%rbp)
  4008db:	8c 05 48 0e 30 86    	mov    %es,-0x79cff1b8(%rip)        # ffffffff86701729 <__TMC_END__+0xffffffff861006f1>
  4008e1:	06                   	(bad)  
  4008e2:	48 0e                	rex.W (bad) 
  4008e4:	38 83 07 4d 0e 40    	cmp    %al,0x400e4d07(%rbx)
  4008ea:	72 0e                	jb     4008fa <__GNU_EH_FRAME_HDR+0x12e>
  4008ec:	38 41 0e             	cmp    %al,0xe(%rcx)
  4008ef:	30 41 0e             	xor    %al,0xe(%rcx)
  4008f2:	28 42 0e             	sub    %al,0xe(%rdx)
  4008f5:	20 42 0e             	and    %al,0xe(%rdx)
  4008f8:	18 42 0e             	sbb    %al,0xe(%rdx)
  4008fb:	10 42 0e             	adc    %al,0xe(%rdx)
  4008fe:	08 00                	or     %al,(%rax)
  400900:	14 00                	adc    $0x0,%al
  400902:	00 00                	add    %al,(%rax)
  400904:	cc                   	int3   
  400905:	00 00                	add    %al,(%rax)
  400907:	00 88 fe ff ff 02    	add    %cl,0x2fffffe(%rax)
	...

0000000000400918 <__FRAME_END__>:
  400918:	00 00                	add    %al,(%rax)
	...

Disassembly of section .init_array:

0000000000600e00 <__frame_dummy_init_array_entry>:
  600e00:	a0                   	.byte 0xa0
  600e01:	06                   	(bad)  
  600e02:	40 00 00             	add    %al,(%rax)
  600e05:	00 00                	add    %al,(%rax)
	...

Disassembly of section .fini_array:

0000000000600e08 <__do_global_dtors_aux_fini_array_entry>:
  600e08:	80 06 40             	addb   $0x40,(%rsi)
  600e0b:	00 00                	add    %al,(%rax)
  600e0d:	00 00                	add    %al,(%rax)
	...

Disassembly of section .jcr:

0000000000600e10 <__JCR_END__>:
	...

Disassembly of section .dynamic:

0000000000600e18 <_DYNAMIC>:
  600e18:	01 00                	add    %eax,(%rax)
  600e1a:	00 00                	add    %al,(%rax)
  600e1c:	00 00                	add    %al,(%rax)
  600e1e:	00 00                	add    %al,(%rax)
  600e20:	01 00                	add    %eax,(%rax)
  600e22:	00 00                	add    %al,(%rax)
  600e24:	00 00                	add    %al,(%rax)
  600e26:	00 00                	add    %al,(%rax)
  600e28:	01 00                	add    %eax,(%rax)
  600e2a:	00 00                	add    %al,(%rax)
  600e2c:	00 00                	add    %al,(%rax)
  600e2e:	00 00                	add    %al,(%rax)
  600e30:	77 00                	ja     600e32 <_DYNAMIC+0x1a>
  600e32:	00 00                	add    %al,(%rax)
  600e34:	00 00                	add    %al,(%rax)
  600e36:	00 00                	add    %al,(%rax)
  600e38:	0c 00                	or     $0x0,%al
  600e3a:	00 00                	add    %al,(%rax)
  600e3c:	00 00                	add    %al,(%rax)
  600e3e:	00 00                	add    %al,(%rax)
  600e40:	60                   	(bad)  
  600e41:	05 40 00 00 00       	add    $0x40,%eax
  600e46:	00 00                	add    %al,(%rax)
  600e48:	0d 00 00 00 00       	or     $0x0,%eax
  600e4d:	00 00                	add    %al,(%rax)
  600e4f:	00 94 07 40 00 00 00 	add    %dl,0x40(%rdi,%rax,1)
  600e56:	00 00                	add    %al,(%rax)
  600e58:	19 00                	sbb    %eax,(%rax)
  600e5a:	00 00                	add    %al,(%rax)
  600e5c:	00 00                	add    %al,(%rax)
  600e5e:	00 00                	add    %al,(%rax)
  600e60:	00 0e                	add    %cl,(%rsi)
  600e62:	60                   	(bad)  
  600e63:	00 00                	add    %al,(%rax)
  600e65:	00 00                	add    %al,(%rax)
  600e67:	00 1b                	add    %bl,(%rbx)
  600e69:	00 00                	add    %al,(%rax)
  600e6b:	00 00                	add    %al,(%rax)
  600e6d:	00 00                	add    %al,(%rax)
  600e6f:	00 08                	add    %cl,(%rax)
  600e71:	00 00                	add    %al,(%rax)
  600e73:	00 00                	add    %al,(%rax)
  600e75:	00 00                	add    %al,(%rax)
  600e77:	00 1a                	add    %bl,(%rdx)
  600e79:	00 00                	add    %al,(%rax)
  600e7b:	00 00                	add    %al,(%rax)
  600e7d:	00 00                	add    %al,(%rax)
  600e7f:	00 08                	add    %cl,(%rax)
  600e81:	0e                   	(bad)  
  600e82:	60                   	(bad)  
  600e83:	00 00                	add    %al,(%rax)
  600e85:	00 00                	add    %al,(%rax)
  600e87:	00 1c 00             	add    %bl,(%rax,%rax,1)
  600e8a:	00 00                	add    %al,(%rax)
  600e8c:	00 00                	add    %al,(%rax)
  600e8e:	00 00                	add    %al,(%rax)
  600e90:	08 00                	or     %al,(%rax)
  600e92:	00 00                	add    %al,(%rax)
  600e94:	00 00                	add    %al,(%rax)
  600e96:	00 00                	add    %al,(%rax)
  600e98:	f5                   	cmc    
  600e99:	fe                   	(bad)  
  600e9a:	ff 6f 00             	ljmp   *0x0(%rdi)
  600e9d:	00 00                	add    %al,(%rax)
  600e9f:	00 98 02 40 00 00    	add    %bl,0x4002(%rax)
  600ea5:	00 00                	add    %al,(%rax)
  600ea7:	00 05 00 00 00 00    	add    %al,0x0(%rip)        # 600ead <_DYNAMIC+0x95>
  600ead:	00 00                	add    %al,(%rax)
  600eaf:	00 08                	add    %cl,(%rax)
  600eb1:	04 40                	add    $0x40,%al
  600eb3:	00 00                	add    %al,(%rax)
  600eb5:	00 00                	add    %al,(%rax)
  600eb7:	00 06                	add    %al,(%rsi)
  600eb9:	00 00                	add    %al,(%rax)
  600ebb:	00 00                	add    %al,(%rax)
  600ebd:	00 00                	add    %al,(%rax)
  600ebf:	00 d0                	add    %dl,%al
  600ec1:	02 40 00             	add    0x0(%rax),%al
  600ec4:	00 00                	add    %al,(%rax)
  600ec6:	00 00                	add    %al,(%rax)
  600ec8:	0a 00                	or     (%rax),%al
  600eca:	00 00                	add    %al,(%rax)
  600ecc:	00 00                	add    %al,(%rax)
  600ece:	00 00                	add    %al,(%rax)
  600ed0:	be 00 00 00 00       	mov    $0x0,%esi
  600ed5:	00 00                	add    %al,(%rax)
  600ed7:	00 0b                	add    %cl,(%rbx)
  600ed9:	00 00                	add    %al,(%rax)
  600edb:	00 00                	add    %al,(%rax)
  600edd:	00 00                	add    %al,(%rax)
  600edf:	00 18                	add    %bl,(%rax)
  600ee1:	00 00                	add    %al,(%rax)
  600ee3:	00 00                	add    %al,(%rax)
  600ee5:	00 00                	add    %al,(%rax)
  600ee7:	00 15 00 00 00 00    	add    %dl,0x0(%rip)        # 600eed <_DYNAMIC+0xd5>
	...
  600ef5:	00 00                	add    %al,(%rax)
  600ef7:	00 03                	add    %al,(%rbx)
	...
  600f01:	10 60 00             	adc    %ah,0x0(%rax)
  600f04:	00 00                	add    %al,(%rax)
  600f06:	00 00                	add    %al,(%rax)
  600f08:	02 00                	add    (%rax),%al
  600f0a:	00 00                	add    %al,(%rax)
  600f0c:	00 00                	add    %al,(%rax)
  600f0e:	00 00                	add    %al,(%rax)
  600f10:	48 00 00             	rex.W add %al,(%rax)
  600f13:	00 00                	add    %al,(%rax)
  600f15:	00 00                	add    %al,(%rax)
  600f17:	00 14 00             	add    %dl,(%rax,%rax,1)
  600f1a:	00 00                	add    %al,(%rax)
  600f1c:	00 00                	add    %al,(%rax)
  600f1e:	00 00                	add    %al,(%rax)
  600f20:	07                   	(bad)  
  600f21:	00 00                	add    %al,(%rax)
  600f23:	00 00                	add    %al,(%rax)
  600f25:	00 00                	add    %al,(%rax)
  600f27:	00 17                	add    %dl,(%rdi)
  600f29:	00 00                	add    %al,(%rax)
  600f2b:	00 00                	add    %al,(%rax)
  600f2d:	00 00                	add    %al,(%rax)
  600f2f:	00 18                	add    %bl,(%rax)
  600f31:	05 40 00 00 00       	add    $0x40,%eax
  600f36:	00 00                	add    %al,(%rax)
  600f38:	07                   	(bad)  
	...
  600f41:	05 40 00 00 00       	add    $0x40,%eax
  600f46:	00 00                	add    %al,(%rax)
  600f48:	08 00                	or     %al,(%rax)
  600f4a:	00 00                	add    %al,(%rax)
  600f4c:	00 00                	add    %al,(%rax)
  600f4e:	00 00                	add    %al,(%rax)
  600f50:	18 00                	sbb    %al,(%rax)
  600f52:	00 00                	add    %al,(%rax)
  600f54:	00 00                	add    %al,(%rax)
  600f56:	00 00                	add    %al,(%rax)
  600f58:	09 00                	or     %eax,(%rax)
  600f5a:	00 00                	add    %al,(%rax)
  600f5c:	00 00                	add    %al,(%rax)
  600f5e:	00 00                	add    %al,(%rax)
  600f60:	18 00                	sbb    %al,(%rax)
  600f62:	00 00                	add    %al,(%rax)
  600f64:	00 00                	add    %al,(%rax)
  600f66:	00 00                	add    %al,(%rax)
  600f68:	fe                   	(bad)  
  600f69:	ff                   	(bad)  
  600f6a:	ff 6f 00             	ljmp   *0x0(%rdi)
  600f6d:	00 00                	add    %al,(%rax)
  600f6f:	00 e0                	add    %ah,%al
  600f71:	04 40                	add    $0x40,%al
  600f73:	00 00                	add    %al,(%rax)
  600f75:	00 00                	add    %al,(%rax)
  600f77:	00 ff                	add    %bh,%bh
  600f79:	ff                   	(bad)  
  600f7a:	ff 6f 00             	ljmp   *0x0(%rdi)
  600f7d:	00 00                	add    %al,(%rax)
  600f7f:	00 01                	add    %al,(%rcx)
  600f81:	00 00                	add    %al,(%rax)
  600f83:	00 00                	add    %al,(%rax)
  600f85:	00 00                	add    %al,(%rax)
  600f87:	00 f0                	add    %dh,%al
  600f89:	ff                   	(bad)  
  600f8a:	ff 6f 00             	ljmp   *0x0(%rdi)
  600f8d:	00 00                	add    %al,(%rax)
  600f8f:	00 c6                	add    %al,%dh
  600f91:	04 40                	add    $0x40,%al
	...

Disassembly of section .got:

0000000000600ff8 <.got>:
	...

Disassembly of section .got.plt:

0000000000601000 <_GLOBAL_OFFSET_TABLE_>:
  601000:	18 0e                	sbb    %cl,(%rsi)
  601002:	60                   	(bad)  
	...
  601017:	00 96 05 40 00 00    	add    %dl,0x4005(%rsi)
  60101d:	00 00                	add    %al,(%rax)
  60101f:	00 a6 05 40 00 00    	add    %ah,0x4005(%rsi)
  601025:	00 00                	add    %al,(%rax)
  601027:	00 b6 05 40 00 00    	add    %dh,0x4005(%rsi)
  60102d:	00 00                	add    %al,(%rax)
	...

Disassembly of section .data:

0000000000601030 <__data_start>:
  601030:	00 00                	add    %al,(%rax)
	...

Disassembly of section .bss:

0000000000601034 <__bss_start>:
  601034:	00 00                	add    %al,(%rax)
	...

Disassembly of section .comment:

0000000000000000 <.comment>:
   0:	47                   	rex.RXB
   1:	43                   	rex.XB
   2:	43 3a 20             	rex.XB cmp (%r8),%spl
   5:	28 47 4e             	sub    %al,0x4e(%rdi)
   8:	55                   	push   %rbp
   9:	29 20                	sub    %esp,(%rax)
   b:	36 2e 32 2e          	ss xor %cs:(%rsi),%ch
   f:	31 20                	xor    %esp,(%rax)
  11:	32 30                	xor    (%rax),%dh
  13:	31 36                	xor    %esi,(%rsi)
  15:	30 39                	xor    %bh,(%rcx)
  17:	31 36                	xor    %esi,(%rsi)
  19:	20 28                	and    %ch,(%rax)
  1b:	52                   	push   %rdx
  1c:	65 64 20 48 61       	gs and %cl,%fs:0x61(%rax)
  21:	74 20                	je     43 <_init-0x40051d>
  23:	36 2e 32 2e          	ss xor %cs:(%rsi),%ch
  27:	31                   	.byte 0x31
  28:	2d                   	.byte 0x2d
  29:	32 29                	xor    (%rcx),%ch
	...
