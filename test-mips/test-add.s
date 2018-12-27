.data
nl: .asciiz "\n"

.text
.globl main
main:
move $fp, $sp
li $v0, 36
move $t0, $v0
li $v0, 42
move $t0, $v0
li $v0, 5
#soustraction
sub $v0, $t0, $v0
#addition
add $v0, $t0, $v0
#vardef a
addi $sp, $sp, -4
sw $v0, 0($sp)
lw $v0, -4($fp)
move $t0, $v0
li $v0, 57
#addition
add $v0, $t0, $v0
#vardef b
addi $sp, $sp, -4
sw $v0, 0($sp)
lw $v0, -8($fp)
move $t0, $v0
lw $v0, -4($fp)
#soustraction
sub $v0, $t0, $v0
move $t0, $v0
lw $v0, -4($fp)
#modulo
div $t0, $v0
mfhi $v0
#vardef c
addi $sp, $sp, -4
sw $v0, 0($sp)
lw $v0, -16($fp)
move $t0, $v0
li $v0, 8
#multiplication
mult $t0, $v0
mflo $v0
move $t0, $v0
lw $v0, -16($fp)
move $t0, $v0
li $v0, 3
#soustraction
sub $v0, $t0, $v0
#addition
add $v0, $t0, $v0
#vardef c
addi $sp, $sp, -4
sw $v0, 0($sp)
move $a0, $v0
li $v0, 1
syscall
la $a0, nl
li $v0, 4
syscall
li $v0, 0
jr $ra
