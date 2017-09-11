
linker3.o:     file format elf64-x86-64


Disassembly of section .text:

0000000000000000 <main>:
   0:	55                   	push   %rbp
   1:	48 89 e5             	mov    %rsp,%rbp
   4:	48 83 ec 10          	sub    $0x10,%rsp
   8:	c7 45 f0 0a 00 00 00 	movl   $0xa,-0x10(%rbp)
   f:	b8 00 00 00 00       	mov    $0x0,%eax
  14:	e8 00 00 00 00       	callq  19 <main+0x19>
			15: R_X86_64_PC32	foo-0x4
  19:	89 45 f4             	mov    %eax,-0xc(%rbp)
  1c:	8b 55 f4             	mov    -0xc(%rbp),%edx
  1f:	8b 45 f0             	mov    -0x10(%rbp),%eax
  22:	89 d6                	mov    %edx,%esi
  24:	89 c7                	mov    %eax,%edi
  26:	e8 00 00 00 00       	callq  2b <main+0x2b>
			27: R_X86_64_PC32	my_mul-0x4
  2b:	89 45 fc             	mov    %eax,-0x4(%rbp)
  2e:	8b 55 f4             	mov    -0xc(%rbp),%edx
  31:	8b 45 f0             	mov    -0x10(%rbp),%eax
  34:	8b 4d fc             	mov    -0x4(%rbp),%ecx
  37:	89 c6                	mov    %eax,%esi
  39:	bf 00 00 00 00       	mov    $0x0,%edi
			3a: R_X86_64_32	.rodata
  3e:	b8 00 00 00 00       	mov    $0x0,%eax
  43:	e8 00 00 00 00       	callq  48 <main+0x48>
			44: R_X86_64_PC32	printf-0x4
  48:	b8 00 00 00 00       	mov    $0x0,%eax
  4d:	c9                   	leaveq 
  4e:	c3                   	retq   

Disassembly of section .rodata:

0000000000000000 <.rodata>:
   0:	25 64 20 74 6f       	and    $0x6f742064,%eax
   5:	20 74 68 65          	and    %dh,0x65(%rax,%rbp,2)
   9:	20 70 6f             	and    %dh,0x6f(%rax)
   c:	77 65                	ja     73 <main+0x73>
   e:	72 20                	jb     30 <main+0x30>
  10:	25 64 20 3d 20       	and    $0x203d2064,%eax
  15:	25                   	.byte 0x25
  16:	64 0a 00             	or     %fs:(%rax),%al

Disassembly of section .comment:

0000000000000000 <.comment>:
   0:	00 47 43             	add    %al,0x43(%rdi)
   3:	43 3a 20             	rex.XB cmp (%r8),%spl
   6:	28 47 4e             	sub    %al,0x4e(%rdi)
   9:	55                   	push   %rbp
   a:	29 20                	sub    %esp,(%rax)
   c:	36 2e 32 2e          	ss xor %cs:(%rsi),%ch
  10:	31 20                	xor    %esp,(%rax)
  12:	32 30                	xor    (%rax),%dh
  14:	31 36                	xor    %esi,(%rsi)
  16:	30 39                	xor    %bh,(%rcx)
  18:	31 36                	xor    %esi,(%rsi)
  1a:	20 28                	and    %ch,(%rax)
  1c:	52                   	push   %rdx
  1d:	65 64 20 48 61       	gs and %cl,%fs:0x61(%rax)
  22:	74 20                	je     44 <main+0x44>
  24:	36 2e 32 2e          	ss xor %cs:(%rsi),%ch
  28:	31                   	.byte 0x31
  29:	2d                   	.byte 0x2d
  2a:	32 29                	xor    (%rcx),%ch
	...

Disassembly of section .eh_frame:

0000000000000000 <.eh_frame>:
   0:	14 00                	adc    $0x0,%al
   2:	00 00                	add    %al,(%rax)
   4:	00 00                	add    %al,(%rax)
   6:	00 00                	add    %al,(%rax)
   8:	01 7a 52             	add    %edi,0x52(%rdx)
   b:	00 01                	add    %al,(%rcx)
   d:	78 10                	js     1f <.eh_frame+0x1f>
   f:	01 1b                	add    %ebx,(%rbx)
  11:	0c 07                	or     $0x7,%al
  13:	08 90 01 00 00 1c    	or     %dl,0x1c000001(%rax)
  19:	00 00                	add    %al,(%rax)
  1b:	00 1c 00             	add    %bl,(%rax,%rax,1)
  1e:	00 00                	add    %al,(%rax)
  20:	00 00                	add    %al,(%rax)
			20: R_X86_64_PC32	.text
  22:	00 00                	add    %al,(%rax)
  24:	4f 00 00             	rex.WRXB add %r8b,(%r8)
  27:	00 00                	add    %al,(%rax)
  29:	41 0e                	rex.B (bad) 
  2b:	10 86 02 43 0d 06    	adc    %al,0x60d4302(%rsi)
  31:	02 4a 0c             	add    0xc(%rdx),%cl
  34:	07                   	(bad)  
  35:	08 00                	or     %al,(%rax)
	...
