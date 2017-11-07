func tstconst
bgnstmt 1
t1 := 10
reti t1
fend

func tstcall
bgnstmt 1
t1 := 10
t2 := 100
t3 := "Test: %d %d\n"
argi t1
argi t2
argi t3
t4 := global printf
t5 := fi t4 3
reti t1
fend

func tstadd
formal 4
formal 4
bgnstmt 1
t1 := param 1
t2 := param 2
t3 := t1 +i t2
reti t3
fend

func tstsub
formal 4
formal 4
bgnstmt 1
t1 := param 1
t2 := param 2
t3 := t1 -i t2
reti t3
fend

func tstmul
formal 4
formal 4
bgnstmt 1
t1 := param 1
t2 := param 2
t3 := t1 *i t2
reti t3
fend

func tstdiv
formal 4
formal 4
bgnstmt 1
t1 := param 1
t2 := param 2
t3 := t1 /i t2
reti t3
fend

func tstmod
formal 4
formal 4
bgnstmt 1
t1 := param 1
t2 := param 2
t3 := t1 %i t2
reti t3
fend

func tstshl
formal 4
formal 4
bgnstmt 1
t1 := param 1
t2 := param 2
t3 := t1 <<i t2
reti t3
fend

func tstshr
formal 4
formal 4
bgnstmt 1
t1 := param 1
t2 := param 2
t3 := t1 >>i t2
reti t3
fend
