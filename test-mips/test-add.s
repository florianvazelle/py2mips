.data
nl: .asciiz "\n"

.text
.globl main
main:
move $fp, $sp
li $v0, 36
addi $sp, $sp, -4
sw $v0, 0($sp)
li $v0, 42
addi $sp, $sp, -4
sw $v0, 0($sp)
lw $t0, 4($sp)
lw $t1, 0($sp)
add $v0, $t0, $t1
move $a0, $v0
li $v0, 4
syscall
la $a0, nl
syscall
li $v0, 0
jr $ra
