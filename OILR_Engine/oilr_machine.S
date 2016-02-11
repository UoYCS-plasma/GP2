#include <asm/unistd.h>
// #include <bits/socket.h>
#include <asm/mman.h>
// #include "inc/sys_defs.h"

# Warnings: be very careful about using push 
# and pop in non-Forth functions -- there's a
# return address on top of the stack!

# Useful values ######################################
.set HEAP_SIZE, 32*1024*1024  # 32 meg
.set SCRATCH_SIZE, 16*1024     # 16k
.set BUFFER_SIZE,  16*1024     # 16k
.set WORD_BUFFER_SIZE, 256
.set FLAG_TABLE_SIZE, (127-32) # non-space ASCII chars
.set CELL_BITS, 2
.set CELL_SIZE, (1<<CELL_BITS)
.set DS_SIZE, (64*CELL_SIZE)
.set link, 0

.set BASE_4_YEAR, 1461
.set LILIAN_CORRECTION, 6345

.set GRAPH_ELEM_SHIFT, 6   #  1<<6 = 64 bytes per element
.set GRAPH_ELEM_SIZE, (1<<GRAPH_ELEM_SHIFT)

// #define CANTILEVER_COMPILATION_TRACE
//#define CANTILEVER_EXECUTION_TRACE
// #define CANTILEVER_STACK_TRACE


#ifdef CANTILEVER_EXECUTION_TRACE
#define TRACE_CODE call _trace
#else
#define TRACE_CODE
#endif


######################################################
# macros                                             #
######################################################

#define JUMP(tgt) (tgt - .)

# Structural macros ##################################

.set LFA_OFFS, (1*CELL_SIZE)
.set BFA_OFFS, (2*CELL_SIZE)
.set CFA_OFFS, (3*CELL_SIZE)
.set PFA_OFFS, (4*CELL_SIZE)

.macro header label, name, behav
	.section .data
	.align CELL_SIZE, 0
dict_label_\label:
	.long (100001f - 100000f - 1)
	100000:
	.ifeqs "\name", ""
		.asciz "\label"
	.else
		.asciz "\name"
	.endif
	100001:
	.align CELL_SIZE, 0
dict_\label:
	.long link
	.long dict_label_\label
	.long \behav
.set link, dict_\label
.endm

.macro prim label, name, behav=storeinc
	header \label, "\name", \behav
\label:
	.long prim_\label
	.section .text
	.align CELL_SIZE
prim_\label:
	TRACE_CODE
.endm

.macro word label, name, behav=storeinc, handler=do
	header \label, "\name", \behav
\label:
	.long \handler
word_\label:
.endm

.macro constant label, val, name
	word \label, "\name", , doconst
	# push $\val
	# next
	.int \val
.endm

.macro variable label, val=0, name
	word \label, "\name", , dovar
var_\label:
	.long \val
.endm

.macro string label, str, name
	constant \label, str_\label, "\name"
	.section .data
str_\label:
	.long (20001f - str_text_\label - 1)
str_text_\label:
	.asciz "\str"
	20001:
	.align CELL_SIZE, 0
.endm

# Code macros ########################################

.macro next
	lodsl
	jmp *(%eax)
	.align CELL_SIZE
.endm

.macro end
	.long return
.endm

.macro string_len_in_cells reg
	shr $CELL_BITS, \reg
	inc \reg
.endm

.macro pushrs reg
	lea -CELL_SIZE(%ebp), %ebp
	movl \reg, (%ebp)
.endm

.macro poprs, reg
	mov (%ebp), \reg
	lea CELL_SIZE(%ebp), %ebp
.endm

.macro align_dp_to bound
	add $(\bound-1), %edi
	and $(-\bound), %edi
.endm

.macro align_dp
	add $3, %edi
	and $0xfffffffc, %edi
.endm

.macro times_ten reg
	shl $1, \reg
	lea (\reg, \reg, 4), \reg
.endm

.macro digit from, to, err
	movb \from, \to
	sub $'0', \to
	cmp $9, \to
	ja \err
.endm

######################################################
# C-style functions                                  #
######################################################

# Initialisation #####################################

.section .data
constant HeapBaseAddr . "heap-base-addr"


.section .text

.globl _start
.align CELL_SIZE
_start:
	cld
	mov (%esp), %eax
	mov %eax, var_argc
	lea 4(%esp), %eax
	mov %eax, var_argv
	mov %eax, var_argv0
	push $0
	mov %esp, var_ds0
	mov %esp, %ebp
	sub $DS_SIZE, %ebp
	mov %ebp, var_rs0
	mov $cold_start, %esi
next

# Utility function ###################################

.align CELL_SIZE
_fill_buffer:
	push %ebx
	mov $__NR_read, %eax
	mov var_inChannel, %ebx
	mov var_ioBuffer, %ecx
	mov %ecx, var_bufpos  // reset buffer position
	mov $BUFFER_SIZE, %edx
	int $0x80
	test %eax, %eax
	jbe _eof
	add %eax, %ecx
	mov %ecx, var_bufend
	pop %ebx
	// fallthrough
.align CELL_SIZE
_key:
	mov var_bufpos, %edx
	cmp var_bufend, %edx
	jae _fill_buffer
	movzbl (%edx), %eax
	inc %edx
	mov %edx, var_bufpos
ret

.align CELL_SIZE
_eof:
	pop %ebx  # we're called from fill-buffer, which preserves %ebx
	mov var_ioBuffer, %eax
	mov %eax, var_bufend
	mov %eax, var_bufpos
	pop %eax   # _key return addr
	pop %eax   # _word return addr
	mov $_eof_wrap, %esi
next

#ifdef CANTILEVER_EXECUTION_TRACE
_space_buffer:
	.space 128, ' '
.align CELL_SIZE
_trace:
	# print spaces based on return stack depth
	push %eax
	mov var_rs0, %edx
	mov $2, %ebx
	mov $__NR_write, %eax
	mov $_space_buffer, %ecx
	sub %ebp, %edx
	int $0x80
	# print function name
	mov (%esp), %eax
	mov (LFA_OFFS-CFA_OFFS)(%eax), %ecx
	mov (%ecx), %edx
	add $CELL_SIZE, %ecx
	mov $__NR_write, %eax
	int $0x80
	# print return char
	mov $__NR_write, %eax
	push $10
	mov %esp, %ecx
	mov $1, %edx
	int $0x80
	pop %ecx
	pop %eax
ret
#endif

.align CELL_SIZE
_word:  // copy the next word into wordbuffer
	// skip whitespace
	call _key
	cmpb $' ', %al
	jbe _word 
	// copy word
	mov $wordbuffer_text, %edx
1:
	movb %al, (%edx)
	inc %edx
	cmpl $end_wordbuffer, %edx
	jae 2f // buffer overflow!
	pushrs %edx  // save our pointer
	call _key
	poprs %edx   // ...and restore it
	cmpb $' ', %al
	ja 1b
	movl $0, CELL_SIZE(%edx)  // add two words of nulls after word
	//mov $0, 8(%edx)
	// populate the length field of the buffer
	movl $0, (%edx)
	sub $wordbuffer_text, %edx
	mov %edx, wordbuffer
ret
2:
	// TODO: should skip the rest of the long word too...
	movl $0x202e2e2e, wordbuffer+12 // truncate the long word with "... "
	movl $12, wordbuffer
	movl $wordbuffer, (%esp)  // over-write our return address
	push $str_TooLong
	# handle error. Was function, but used only here
	mov $_error_wrap, %esi
next

# Wrappers for calling words from code ###############
.align CELL_SIZE
cold_start:
	.long initialiseVM

.align CELL_SIZE
_error_wrap:
	.long lit, 2, error

.align CELL_SIZE
_eof_wrap:
	.long EndOfFile, raise, tailcall, reset

######################################################
# Forth-style code words                             #
######################################################

# codewords ##########################################

.align CELL_SIZE
do:
TRACE_CODE
	pushrs %esi  // save return address
	lea 4(%eax), %esi
next

.align CELL_SIZE
doconst:
	push 4(%eax)
TRACE_CODE
next

.align CELL_SIZE
dovar:
	lea 4(%eax), %edx
	push %edx
TRACE_CODE
next


# The black-magic of forth: data with behaviour.
#
# ... | dodoes | ptr | data ... | ...  behav
#                  \_________________7

.align CELL_SIZE
dodoes:
	pushrs %esi
	lea 8(%eax), %edx
	mov 4(%eax), %esi
	push %edx
next

# System calls #######################################

# System call numbers
constant SysExit,  __NR_exit
constant SysRead,  __NR_read
constant SysWrite, __NR_write
constant SysOpen,  __NR_open
constant SysClose, __NR_close
constant SysStat,  __NR_fstat
constant SysMmap,  __NR_mmap
constant SysMunmap, __NR_munmap
constant SysBrk,   __NR_brk
constant SysIOCtl, __NR_ioctl
constant SysTime,  __NR_time
constant SysNanosleep, __NR_nanosleep

# prims
prim syscall0  # id -- result
	pop %eax
	int $0x80
	push %eax
next
prim syscall1  # arg id -- result
	pushrs %ebx
	pop %eax
	pop %ebx
	int $0x80
	push %eax
	poprs %ebx
next
prim syscall2  # arg2 arg1 id -- result
	pushrs %ebx
	pop %eax
	pop %ebx
	pop %ecx
	int $0x80
	push %eax
	poprs %ebx
next
prim syscall3  # arg3 arg2 arg1 id -- result
	pushrs %ebx
	pop %eax
	pop %ebx
	pop %ecx
	pop %edx
	int $0x80
	push %eax
	poprs %ebx
next

prim syscall6  # arg6 arg5 ... arg1 id -- result
	# This is slightly different because for >5 args
	# Linux expects args to be passed by pointer.
	# In this case we simply use the stack
	pushrs %ebx
	pop %eax
	mov %esp, %ebx
	int $0x80
	add $(6*CELL_SIZE), %esp
	push %eax
	poprs %ebx
next

# IO prims ###########################################

# basic IO
constant stdin,  0
constant stdout, 1
constant stderr, 2

variable inChannel, 0, "in-channel"
variable outChannel, 1, "out-channel"
variable errChannel, 2, "err-channel"

# NOTE: values are octal!
constant ReadOnly,   0
constant WriteOnly,  01101
constant ReadWrite,  02202

prim key
	call _key
	push %eax
next

prim word  # -- str
	call _word
	// push the address of temporary buffer
	push $wordbuffer
next

# Logical and bitwise prims ##########################

constant true, -1
constant false, 0

prim match "matches?" # n1 n2 -- n1 bool
	# semi-destructive equals for pattern matching
	pop %eax
	xor %ecx, %ecx
	cmpl (%esp), %eax
	setne %cl
	dec %ecx
	push %ecx
next

prim between "btw?"  # n lower upper -- bool
	pop %edx  # upper
	pop %ecx  # lower
	pop %eax  # n
	sub %ecx, %edx
	sub %ecx, %eax
	xor %ecx, %ecx
	cmp %edx, %eax
	seta %cl
	dec %ecx
	push %ecx
next

prim eq, "=" // a b -- bool
	pop %eax
	pop %edx
	xor %ecx, %ecx
	cmpl %eax, %edx
	setne %cl
	dec %ecx
	push %ecx
next
prim neq, "<>" // a b -- bool
	pop %eax
	pop %edx
	xor %ecx, %ecx
	cmpl %eax, %edx
	sete %cl
	dec %ecx
	push %ecx
next
prim ge, ">="
	pop %eax
	pop %edx
	xor %ecx, %ecx
	cmpl %eax, %edx
	setl %cl
	dec %ecx
	push %ecx
next
prim gt, ">"
	pop %eax
	pop %edx
	xor %ecx, %ecx
	cmpl %eax, %edx
	setle %cl
	dec %ecx
	push %ecx
next
prim le, "<="
	pop %eax
	pop %edx
	xor %ecx, %ecx
	cmpl %eax, %edx
	setg %cl
	dec %ecx
	push %ecx
next
prim lt, "<"
	pop %eax
	pop %edx
	xor %ecx, %ecx
	cmpl %eax, %edx
	setge %cl
	dec %ecx
	push %ecx
next

prim and
	pop %eax
	and %eax, (%esp)
next
prim or
	pop %eax
	or %eax, (%esp)
next
prim xor
	pop %eax
	xor %eax, (%esp)
next
prim not
	pop %eax
	not %eax
	push %eax
next

prim bool
	pop %eax
	xor %edx, %edx
	test %eax, %eax
	setz %dl
	dec %edx
	push %edx
next

prim lshift, "shift-up" // int n -- int
	pop %ecx
	pop %eax
	shl %cl, %eax
	push %eax
next
prim rshift, "shift-down" // int n -- int
	pop %ecx
	pop %eax
	sar %cl, %eax
	push %eax
next
prim urshift, "u-shift-down" // uint n -- uint
	pop %ecx
	pop %eax
	shr %cl, %eax
	push %eax
next

# Arithmetic prims ###################################

constant zero, 0, "0"
constant one,  1, "1"
constant minus_one, -1 "-1"

prim mul, "*"  // int int -- int
	pop %eax
	imull (%esp)
	mov %eax, (%esp)
	// TODO: check for overflow
next

prim mulDiv, "*/" // int int int -- int
	pop %ecx
	pop %eax
	imull (%esp)
	idiv %ecx
	mov %eax, (%esp)
next

prim udivmod, "/modu" # int int -- int int
	pushrs %ebx
	pop %ebx
	pop %eax
	xor %edx, %edx
	divl %ebx
	push %eax
	push %edx
	poprs %ebx
next
prim divmod  "/mod" # int int -- int int
	pushrs %ebx
	pop %ebx
	pop %eax
	cdq  // sign-extend %eax into %edx
	idivl %ebx
	push %eax
	push %edx
	poprs %ebx
next

prim sub, "-" // int int -- int
	pop %eax
	sub %eax, (%esp)
next
prim add, "+" // int int -- int
	pop %eax
	add %eax, (%esp)
next

prim neg
	negl (%esp)
next

prim inc, "1+"
	incl (%esp)
next
prim dec, "1-"
	decl (%esp)
next

prim double, "2*"
	pop %eax
	shl $1, %eax
	push %eax
next
prim halve, "2/"
	pop %eax
	sar $1, %eax
	push %eax
next

prim min // int int -- int
	pop %eax
	cmpl (%esp), %eax
	jge 1f
	mov %eax, (%esp)
1:
next
prim max
	pop %eax
	cmpl (%esp), %eax
	jle 1f
	mov %eax, (%esp)
1:
next

prim umin // uint uint -- uint
	pop %eax
	cmpl (%esp), %eax
	jae 1f
	mov %eax, (%esp)
1:
next
prim umax
	pop %eax
	cmpl (%esp), %eax
	jbe 1f
	mov %eax, (%esp)
1:
next

prim sumCells, "sum-cells"  # array count -- int
	pop %ecx
	pop %edx
	xor %eax, %eax
1:
	jcxz 2f
	dec %ecx
	add %edx, %eax
	jmp 1b
2:
next

# Data Stack manipulation prims ######################

prim dspGet, "dsp@"
	push %esp
next
prim dspSet, "dsp!"
	pop %esp
next
prim dsDepth, "ds-depth"
	mov var_ds0, %eax
	sub %esp, %eax
	sar $CELL_BITS, %eax
	push %eax
next

prim drop
	pop %eax
next
prim nip
	pop %eax
	pop %edx
	push %eax
next
prim swap  // a b -- b a
	pop %eax
	pop %edx
	push %eax
	push %edx
next
prim dup
	push (%esp)
next
prim over
	push CELL_SIZE(%esp)
next

# Return stack prims #################################

prim rspGet, "rsp@"
	push %ebp
next
prim rspSet, "rsp!"
	pop %ebp
next
prim rsDepth, "rs-depth"
	mov var_rs0, %eax
	sub %ebp, %eax
	sar $CELL_BITS, %eax
	push %eax
next

prim push
	pop %eax
	pushrs %eax
next
prim peek
	push (%ebp)
next
prim pop
	poprs %eax
	push %eax
next
prim stash
	mov (%esp), %eax
	pushrs %eax
next
prim trash
	poprs %eax
next

prim frame
	pushrs %ebx
	mov %ebp, %ebx
next
prim unframe
	mov %ebx, %ebp
	poprs %ebx
next
prim local, "$$"  # n -- addr
	# get cell n of current frame
	pop %eax
	not %eax
	lea (%ebx, %eax, 4), %edx
	push %edx
next
prim getLocal, "$$@" # n -- val
	pop %eax
	not %eax
	mov (%ebx, %eax, 4), %eax
	push %eax
next
prim setLocal "$$!"  # val n -- 
	pop %eax
	pop %edx
	mov %edx, (%ebx, %eax, 4)
next
prim locals  # n -- 
	# create a frame with n local variables
	pushrs %ebx
	mov %ebp, %ebx
	pop %ecx
1:
	test %ecx, %ecx
	jz 2f
	pop %eax
	dec %ecx
	pushrs %eax
	jmp 1b
2:
	pushrs $do_unframe
next
prim incVar "inc-var"
	pop %eax
	incl (%eax)
next
prim decVar "dec-var"
	pop %eax
	decl (%eax)
next

do_unframe:
	.long unframe, return

# Instruction pointer ################################

prim ipGet, "ip@"
	push %esi
next
prim take
	lodsl
	push %eax
next
prim inspect
	push (%esi)
next

# Memory access ######################################

prim get, "@"
	pop %eax
	push (%eax)
next
prim getByte, "@b"
	xor %eax, %eax
	pop %edx
	mov (%edx), %al
	push %eax
next

prim set "!" # int addr -- 
	pop %eax
	pop %edx
	mov %edx, (%eax)
next
prim setByte "!b" # int addr -- 
	pop %eax
	pop %edx
	mov %dl, (%eax)
next

prim dpGet, "dp@"
	push %edi
next
prim dpSet, "dp!"
	pop %edi
next

prim here
	push %edi
next

prim dpAlign, "align-dp"
	align_dp
next

prim storeinc, ","
	pop %eax
	stosl
next

prim storebinc, ",b"
	pop %eax
	stosb
next

prim cell
	shll $CELL_BITS, (%esp)
next

prim align // addr -- addr
	// align to cell boundary
	pop %eax
	add $3, %eax
	andb $0xfc, %al
	push %eax
next

prim isAnonymous "is-anon?" # addr -- bool
	# is addr in the anonymous area?
	mov var_anonCodeAreaAddr, %edx
	pop %eax
	sub %edx, %eax
	xor %edx, %edx
	cmp $SCRATCH_SIZE, %eax
	seta %dl
	dec %edx
	push %edx
next

# Flow control #######################################

prim return
	poprs %esi
next

prim data
	mov (%esi), %eax
	lea 4(%esi), %edx
	lea 4(%esi, %eax), %esi
	push %edx
next

prim branch
	add (%esi), %esi
next

prim zbranch 
	lodsl                  # distance to branch
	pop %edx               # boolean to test
	xor %ecx, %ecx
	sub $CELL_SIZE, %eax   # because lodsl incremented %esi
	test %edx, %edx        # bool is zero?
	setnz %cl
	dec %ecx
	and %ecx, %eax         # %ecx is 0 if %edx is non-zero
	add %eax, %esi
next

prim tailcall, "tail:"
	mov (%esi), %esi
	lea 4(%esi), %esi
next

prim tailcallTOS, "tailcall-tos"
	pop %esi
	lea 4(%esi), %esi
next

prim call
	pop %eax
	jmp *(%eax)
next

# Numeric literals ###################################

prim lit
	lodsl
	push %eax
next
# we give this a different name to make code easier to read
prim quote, "'"
	lodsl
	push %eax
next

# Memory copying prims ###############################

prim copyBytes, "copy-bytes"  # from nbytes to --
	mov %edi, %edx  # save DP
	pushrs %esi      # save IP
	pop %edi
	pop %ecx
	pop %esi
	rep movsb
	poprs %esi
	mov %edx, %edi
next
prim copy, "copy" # from ncells to --
	mov %edi, %edx # save DP
	pushrs %esi
	pop %edi
	pop %ecx
	pop %esi
	rep movsl
	poprs %esi
	mov %edx, %edi
next
prim keep  # addr len -- addr
	# move a temporary value len cells long from addr into the dictionary 
	align_dp
	mov %esi, %edx  # save IP
	pop %ecx        # length in cells
	pop %esi        # get source
	push %edi       # push permanent address
	rep movsl
	mov %edx, %esi  # restore IP
next
prim forget  # cfa -- 
	# take the next compiled word, and delete it and 
	# everything following it in the dictionary
	pop %eax
	sub $CFA_OFFS, %eax
	mov %eax, var_dictPtr
	mov (%eax), %eax
	mov %eax, var_dictionary
next

# String handling prims ##############################

prim strEq # str str -- bool
	xor %edx, %edx
	pushrs %esi
	pushrs %edi
	pop %esi
	pop %edi
	mov (%esi), %ecx
	string_len_in_cells %ecx
	inc %ecx  # extra one for the length field
	repe cmpsl
	setnz %dl
	dec %edx
	push %edx
	poprs %edi
	poprs %esi
next

prim lenz  // zstr -- int
	pushrs %edi
	xor %ecx, %ecx
	dec %ecx
	xor %eax, %eax
	pop %edi
	repne scasb
	inc %ecx
	not %ecx
	push %ecx
	poprs %edi
next


# Numnber and graph elem parser #########################

.section .data
.align CELL_SIZE
number_char_class:
###    0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
# 00: escape chars
.byte  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

# 10: 
.byte  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

# 20:     !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /
.byte  0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 6, 0, 0

# 30:  0  1  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?
.byte  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 5, 0, 0, 0, 0, 0

# 40:  @  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O
.byte  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

# 50:  P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _
.byte  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

# 60:  `  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o
.byte  0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0

# # 70:  p  q  r  s  t  u  v  w  x  y  z  {  |  }  ~ DEL
# .byte  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

### Handlers
# 0: error
# 1: terminator
# 2: natural
# 3: char literal 'x'
# 4: node  n123
# 5: edge  e4

.align CELL_SIZE
jump_table:
	.long _num_err, _num_done, _natural, _char_lit, _node_lit, _edge_lit

word number  #  str -- int bool
	.long one, cell, add, znumber
end

prim znumber  # zstr -- int bool
	# see if zstr conforms to one of the supported 
	# number formats: 123 -123 'a' 1n 1e
	xor %eax, %eax
	xor %edx, %edx
	pushrs %ebx
	pushrs %esi
	pop %esi  # zstr in %esi
	# check for negative number
	movb (%esi), %al
	cmpb $'-', %al
	sete %al
	addl %eax, %esi  # increment zstr if negative
	push %eax
	mov $jump_table, %ebx
	xor %eax, %eax  # %eax is the accumulator.

.align CELL_SIZE
_natural:
	xor %ecx, %ecx
1:
	digit (%esi), %dl, 2f
	times_ten %ecx
	add %edx, %ecx
	inc %esi
	jmp 1b
2:  # if we're here we have a non-digit in %al
	add %ecx, %eax

.align CELL_SIZE
_choose_handler:
	# select handler
	movb (%esi), %dl
	cmp $'n', %dl    # no valid handlers above 'n'
	ja _num_err
	mov $number_char_class, %ecx
	movzbl (%ecx, %edx), %ecx
	mov (%ebx, %ecx, 4), %ecx
	jmp *%ecx

.align CELL_SIZE
_char_lit:
	inc %esi
	test %eax, %eax
	jnz _num_err
	mov (%esi), %al
	jmp _num_done

.align CELL_SIZE
_node_lit:
_edge_lit:
	mov var_host, %ecx
	shl $GRAPH_ELEM_SHIFT, %eax
	lea (%ecx, %eax), %eax
	# TODO: add check for correct type
	jmp _num_done

.align CELL_SIZE
_num_err:
	pop %edx # discard sign
	poprs %esi
	poprs %ebx
	push $0
	push $0
next

.align CELL_SIZE
_num_done:
	# apply negative from earlier
	pop %edx
	test %edx, %edx
	jz 4f
	neg %eax
4:
	poprs %esi
	poprs %ebx
	push %eax
	push $-1
next


# List prims #########################################

prim length # list -- int 
	xor %ecx, %ecx
	pop %eax
1:
	mov (%eax), %eax
	test %eax, %eax
	loopnz 1b
2:
	not %ecx
	push %ecx
next




######################################################
# Forth-style non-primitive words                    #
######################################################

# Startup code #######################################

word initialiseVM
	# initialise the dictionary pointer
	.long getBrkAddr, dup, dictPtr, set, dpSet
	# allocate default heap
	.long lit, HEAP_SIZE, setHeapSize
	# create scratchpad
	.long lit, SCRATCH_SIZE, buffer, scratchpadAddr, set
	# create anon code area
	.long lit, SCRATCH_SIZE, buffer, anonCodeAreaAddr, set
	# set up an IO buffer
	.long lit, BUFFER_SIZE, buffer, ioBuffer, set
	# initialise commandline flag handler
	.long lit, FLAG_TABLE_SIZE, cell, buffer, flagTableAddr, set
	# are there any commandline args...
	.long argc, get, dec, zbranch, JUMP(1f)
	# ...yes, then open the first one
	#.long argv, get, one, cell, add, get
	.long handleArgs
1:
.long tailcall, reset

word reset
	.long rs0, get, rspSet 
	.long interpreter, get
.long tailcallTOS

# word initCommandlineFlags, "init-commandline-flags"
#	.long quote, singleStepper, lit, 'S', flag
# end

word handleArgs, "handle-args"
	# first set this function to be our EOF handler
	.long quote, handleNextArg, handles, EndOfFile
	.long tailcall, handleNextArg
end
word handleNextArg, "handle-next-arg"
	# .long semicolon  # any uncalled code from the previous file?
	# check if there's an argument left to handle
	.long semicolon
	.long nextArg, dup, zbranch, JUMP(1f)
	.long zopenIn, tailcall, reset
1:
	# no args left: read from stdin
	.long drop, stdin, inChannel, set
	.long quote, exit0, handles, EndOfFile
.long tailcall, reset
word nextArg, "next-arg"
	.long argv, get, one, cell, add
	.long dup, argv, set
	.long get
end

# Strings #############################################

constant spc, 32, "'spc'"

word stringCellLength, "string-cell-length"   #  str -- n
	.long get, lit, CELL_BITS, rshift, inc, inc  # byte length, plus null, plus length field
end
word keeps   # tmpStr -- str
	.long dup, stringCellLength, keep
end
word copys   # str addr -- 
	.long push, dup, stringCellLength, pop, copy
end

word scanz # zstr pred -- addr
	.long push, dec
1:
	.long inc, dup, getByte
	.long dup, zbranch, JUMP(2f)  # reached the NULL terminator at the end of the string
	.long peek, call, zbranch, JUMP(1b)
	.long dup
2:
	.long nip, trash
end
word scanIn "scan-in"  # pred -- c
	# read chars from in-channel until pred returns true
	.long push  # store the pred off the stack
1:
	.long key, peek, call
	.long zbranch, JUMP(1b)
	.long trash
end

word clearInputBuffer, "clear-input-buffer"
	.long bufend, get, bufpos, set
end

# Errors & Exception handling #################################

word handles
	# usage: ' fun handles Exception
	#    set fun as handler for Exception
	.long peek, get, call, set
	.long pop, one, cell, add
	.long push
end

word raise  # excp -- 
	.long dup, get                 #  excp hndl
	.long dup, zbranch, JUMP(1f)   #  excp hndl
	.long nip, call, return
1:
	.long drop, labelForVar        #  lbl
	.long ErrorUnhandled, puts
	.long WarningColour, colour, puts, NoColour, colour, nl, error
end

# Default handler for EOF is to exit. This is over-ridden
# when reading from files passed on the commandline.
variable EndOfFile, exit0

string SuccessColour, "[32m"
string ErrorColour,   "[31m"
string WarningColour, "[33m"
string NoColour,      "[0m"

word error
	.long inChannel, get, isATTY, zbranch, JUMP(1f)
	.long clearInputBuffer, reset
1:
.long one, exit


# Numeric Output #######################################

word digit_to_char, "digit-to-char" # n -- c
	.long dup 
	.long zero, lit, 9, between, zbranch, JUMP(1f)
	.long lit, '0', add
	.long branch, JUMP(2f)
1:
	.long lit, 'a'-10, add
2:
end
word decompose  # n base -- 0 c ... 
	.long push, zero, swap
1:
	.long peek, udivmod, digit_to_char, swap
	.long dup, zbranch, JUMP(2f), branch, JUMP(1b)
2:
	.long drop, trash
end
word putsign     # n -- u
	.long dup, lit, (1<<(CELL_SIZE*8-1)), and, zbranch, JUMP(1f)
	.long neg, lit, '-', putc
1:
end
word putnumber   # n base --
	.long decompose
3:
	.long putc, dup, zbranch, JUMP(4f), branch, JUMP(3b)
4:
	.long drop
end

# String output #########################################

word lens  # str -- int
	.long get
end

word warn  # zstr len -- 
	.long errChannel, get, write
end
word warns # nstr --
	.long stash, one, cell, add
	.long pop, get, warn
end

word put   # zstr len --
	.long outChannel, get, write
end
word putz # zstr --
	.long dup, lenz
	.long put
end
word puts #  nstr --  
	.long stash, one, cell, add
	.long pop, get, put
end

word putc  # c --
	.long dspGet, one, put
	.long drop # drop on-stack buffer
end

word putn // n --
	.long putsign
	.long lit, 10
	.long putnumber
end
word putx // n --
	.long lit, 16
	.long putnumber
end

word colour "output-colour"  #  colour -- 
	.long push
	.long outChannel, get, isATTY, zbranch, JUMP(1f)
	.long peek, puts
1:
	.long trash
end


# File IO ###############################################

word zopen   # zstr mode -- fh
	# syscall3 requires arg order: perms mode zstr callid
	.long swap, push, push
	.long lit, 0666, pop, pop
	.long SysOpen, syscall3
	.long dup, zero, lt, zbranch, JUMP(1f)
	.long drop, FileOpenFailed, raise
1:
end
word open  # str mode -- fh
	.long push
	.long one, cell, add
	.long pop, zopen
end

word zopenIn, "zopen-in"
	.long ReadOnly, zopen, inChannel, set
end
word openIn, "open-in"
	.long ReadOnly, open, inChannel, set
end
word openOut, "open-out"
	# TODO: doesn't check for errors!
	.long open, outChannel, set
end

word close  # fh -- err
	.long SysClose, syscall1
end
word closeOut, "close-out"
	.long outChannel, get, close
	.long stdout, outChannel, set
end
word closeIn, "close-in"
	.long inChannel, get, close
	.long stdin, inChannel, set
end

word write  # str len fh -- 
	.long push
	.long swap, pop
	.long SysWrite, syscall3
	.long drop # discard result
end
word read   # buf len fh -- n
	.long push, swap, pop
	.long SysRead, syscall3
end

variable FileOpenFailed
variable MmapFailed
variable FailedToMunmapFile
variable CouldNotStatFile

word statFD "stat-fd"
	.long here, stash, swap, SysStat, syscall2
	.long zero, lt, zbranch, JUMP(1f)
	.long CouldNotStatFile, raise
1:
	.long pop
end

# Get the st_size field of the C stat struct
word st_size ".st_size"
	.long lit, 5, cell, add
end

word fileLength  "file-length" # fd -- len
	.long statFD, st_size, get
end


# Memory management ######################################

word setBrkAddr "set-brk-addr"  # addr -- addr
	.long SysBrk, syscall1
end
word getBrkAddr, "get-brk-addr"  #  -- addr
	.long zero, setBrkAddr
end
word getHeapSize "get-heap-size"  # -- n
	.long getBrkAddr, HeapBaseAddr, sub
end

# Todo: error handling on the following two functions
word setHeapSize "set-heap-size"  # n --
	.long HeapBaseAddr, add, setBrkAddr, drop
end
word growHeap "grow-heap"  # n --
	.long getBrkAddr, add, setBrkAddr, drop
end


# System interface ########################################

word exit0, "0-exit"
	.long zero, exit
end

word exit #  n -- 
	.long SysExit, syscall1
end

word bye, , call
	.long byebye, puts, nl
	.long zero, exit
end

constant TCGETS, 0x00005401
word isATTY "is-a-tty?"  # fd -- bool
	.long push, scratchpadAddr, get, TCGETS, pop, SysIOCtl, syscall3
	.long zero, eq
end

# Memory mapping

constant MapShared,     MAP_SHARED
constant MapPrivate,    MAP_PRIVATE
constant MapAnonymous,  MAP_ANONYMOUS
constant MapStack,      MAP_STACK

constant ProtRead,      PROT_READ
constant ProtWrite,     PROT_WRITE
constant ProtExec,      PROT_EXEC
constant ProtGrowsDown, PROT_GROWSDOWN
constant ProtGrowsUp,   PROT_GROWSUP

word mmap  # fd flags prot len -- addr
	.long push, push, push, push
	#     offs  fd   flag prot len  addr
	.long zero, pop, pop, pop, pop, zero, SysMmap, syscall6
	.long dup, lit, -200, minus_one, between, zbranch, JUMP(1f)
	.long MmapFailed, raise
1:
end
word munmap  # len addr -- 
	.long SysMunmap, syscall2
	.long drop  # todo: error handling
end

# Create a mem-mapped buffer
word buffer  # len -- addr
	.long push
	.long minus_one                        # fd is ignored
	.long MapPrivate, MapAnonymous, or     # set map options
	.long ProtWrite, ProtRead, or          # set protections
	.long pop, mmap
end

word mmapFd "mmap-fd" # fd len -- addr
	.long push, MapShared, ProtRead, pop, mmap
end

word bufferZFile "buffer-zfile" # zstr -- addr len fd
	.long ReadOnly, zopen, stash
	.long dup, fileLength, stash, mmapFd
	.long pop, pop
end

word bufferFile "buffer-file"  # str -- addr len fd
	.long one, cell, add
	.long bufferZFile
end


# Dictionary lookup ###################################

.section .data
.align CELL_SIZE, 0
wordbuffer:
	.long 0
wordbuffer_text:
	.space WORD_BUFFER_SIZE
end_wordbuffer:
	.long 0 // space for padding

word notFoundHandler "not-found-handler"  # str
	.long WarningColour, colour
	.long puts, NoColour, colour, NotFound, puts, nl
.long error

word find // str -- xt behav
	.long push  # save str
	.long dictionary
1:
	.long get, dup, zbranch, JUMP(2f)
	.long dup, label, peek, strEq
	.long zbranch, JUMP(1b)
	.long dup, cfa, swap, behaviour, trash, return
2:
	.long drop, peek, number, zbranch, JUMP(3f)
	.long quote, compileLiteral, trash, return
3:
	.long drop, pop, NotFoundException, raise
end

word lfa, ".lfa"  # entry -- addr
	.long lit, LFA_OFFS, add
end
word cfa, ".cfa" // entry -- codeAddr
	.long lit, CFA_OFFS, add
end
word bfa, ".bfa"
	.long lit, BFA_OFFS, add
end
word pfa, ".pfa"
	.long lit, PFA_OFFS, add
end

word label, ".label"  // entry -- str
	.long lfa, get
end
word behaviour, ".behaviour" 
	.long bfa, get
end
word cfaToLabel, ".cfa->.label"
	.long lit, CFA_OFFS, sub
	.long label
end

word labelForVar, "label-for-var"
	.long one, cell, sub, cfaToLabel
end

word nl
	.long lit, '\n', putc
end

word compileLiteral, "compile-literal" // n --
	.long quote, lit, storeinc, storeinc
end

# Words that create words ############################

variable NotInADataBlock

word beginData, "begin-data-block"
	.long quote, data, storeinc
	.long here, zero, storeinc
end
word endData, "end-data-block"
	.long dpAlign, here, over, one, cell, add, sub, swap, set
end

word lambda, "[", call  # -- addr
	.long beginData, DoAddr, storeinc
end
word endLambda, "]", call  # addr --
	.long quote, return, storeinc, endData
end
word dataLength, "data-length"  # lambda -- len
	.long dup, lit, 2, cell, sub, get   # fetch the data instruction
	.long quote, data, eq, zbranch, JUMP(1f)
	.long one, cell, sub, get, return      # get the length in bytes and return
1:
	.long NotInADataBlock, raise
end

word as, , call # addr -- 
	# a word to compile a definition from a lambda in anonymous code space
	.long semicolon, useDict
	.long word, keeps, header
	.long dup, dataLength  # get length of data block
	.long lit, CELL_BITS, rshift, keep, drop  # convert to cell length and keep.
end

word header  # str --
	.long dpAlign
	.long here, push
	.long dictionary, get, storeinc  # compile link to current head of dict
	.long storeinc                   # compile label address
	.long quote, storeinc, storeinc  # store the default behaviour
	.long pop, dictionary, set       # store our new word in the dict
end

word define  #  str interp --
	.long swap, header, storeinc
end

word defdoes   # str --
	.long quote, dodoes, define, zero, storeinc  # behaviour field
end
word defword   # str -- 
	.long quote, do, define
end
word defconst  # val str -- 
	.long quote, doconst, define
end
word defvar    # val str --
	.long quote, dovar, define
end

word create
	.long semicolon, useDict
	.long word, keeps, defdoes
end
word createImm, "create-immed"
	.long create, immed
end
word does
	# This is subtle! pop gives us the address of the word after
	# does in the definition (and prevents subsequent code executing).
	# We put it in the link field of the word create has just made.
	.long pop, dictionary, get, pfa, set
	# Finally we switch back to anonymous mode
	.long useAnon
end
word createConstant, "create-constant"  # val --
	# constant: potentially needs to evaluate the preceding expression
	# so there's a value waiting
	.long semicolon, useDict
	.long word, keeps, defconst, storeinc
	.long useAnon
end
word createVariable, "create-variable"  # --
	.long semicolon, useDict
	.long word, keeps, defvar
	.long zero, storeinc
	.long useAnon
end
word createWord, "create-word"
	.long semicolon, useDict
	.long word, keeps, defword
end
word immed, "#immediate", call
	.long dictionary, get, bfa, quote, call, swap, set
end

word constantColon, "constant:", call
	.long createConstant
end
word variableColon, "variable:", call
	.long createVariable
end
word colon, ":", call
	.long createWord
end
word semicolon, ";", call
	.long quote, return, storeinc
	.long here, isAnonymous, zbranch, JUMP(1f)
	.long anonCodeAreaAddr, get, call
1:
.long tailcall, useAnon

word useDict, "use-dict"
	.long here, isAnonymous, zbranch, JUMP(1f)
	.long dictPtr, get, dpSet
1:
end
word useAnon, "use-anon"
	.long here, isAnonymous, not, zbranch, JUMP(1f)
	.long here, dictPtr, set
1:
	.long anonCodeAreaAddr, get, dpSet
	.long DoAddr, storeinc
end


variable DataStackUnderflow
variable DataStackOverflow

word interpret
	.long useAnon
1:
	.long word, find, call
	.long branch, JUMP(1b)
end

word suppress, "`", call
	.long word, find, drop, storeinc
end


# Graph manipulation ###############################


variable host
variable hostFree, , "host-free"
variable freelist

constant GraphElemSize GRAPH_ELEM_SIZE
constant GraphElemMask (-GRAPH_ELEM_SIZE)
constant GraphElemShift GRAPH_ELEM_SHIFT

string ColourUncoloured  ""
string ColourRed         " # red"
string ColourBlue        " # blue"
string ColourGreen       " # green"
string ColourGrey        " # grey"
string ColourInvalid     " # invalid"
string ColourDashed      " # dashed"

constant ColourTable _colour_names

_colour_names:
	.long str_ColourUncoloured
	.long str_ColourRed
	.long str_ColourBlue
	.long str_ColourGreen
	.long str_ColourGrey
	.long str_ColourInvalid
	.long str_ColourInvalid
	.long str_ColourDashed

string StrLeader  "\t("
string StrTrailer ")\n"
string StrEmpty "empty"
string StrComma ", "

.macro getReg rnum dst=%eax
	not \rnum
	mov (%ebx, \rnum, 4), \dst
.endm
.macro setReg rnum src
	not \rnum
	mov \src, (%ebx, \rnum, 4)
.endm

prim elemAlignDp, "elem-align-dp"
	align_dp_to GRAPH_ELEM_SIZE
next
prim getRegister "@r"  # reg -- elem
	pop %eax
	getReg %eax
	push %eax
next

prim setRegister "!r"  # val reg -- 
	pop %eax
	pop %edx
	setReg %eax %edx
next

prim allot  # size --
	pop %eax
	add %eax, %edi
next

word setAsNode, "set-as-node"
end



.section .rodata

constant DoAddr,      do
constant DoConstAddr, doconst
constant DoDoesAddr,  dodoes
constant WordBuffer, wordbuffer
constant CellBits, CELL_BITS
constant CellSize, CELL_SIZE

constant Base4Year, BASE_4_YEAR
constant LilianCorrection, LILIAN_CORRECTION

variable argc
variable argv
variable argv0

variable ioBuffer, , "io-buffer"
variable bufpos
variable bufend
variable scratchpadAddr, , "scratchpad-addr"
variable anonCodeAreaAddr, , "anon-code-area-addr"

variable ds0
variable rs0
variable dp0
variable flagTableAddr
variable dictPtr, ,"dict-ptr"

variable NotFoundException, notFoundHandler


string byebye, "\n-- Toodle-pip!\n"
string TooLong, "Word too long: "
string NotFound, " is neither a dictionary word nor a recognised number\n"
string InvalidEsc, "Invalid escape char: "
string UnbalancedRSInBranch, "Unbalanced use of RS in one branch of pattern match at "
string ErrorUnhandled, "Unhandled exception: "
string DefaultPrompt, " ok "

string Expected, "Expected "
string Got,      " but got "
string In,       " in test: "
string RootMarker " (R)"

variable decimal_places, 0, "decimal-places"

variable promptText, str_DefaultPrompt, "prompt-text"
variable interpreter, interpret
variable dictionary, link   # MUST BE LAST
