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
sub $v0, $t0, $v0
add $v0, $t0, $v0
addi $sp, $sp, -4
sw $v0, 0($sp)
lw $v0, -4($fp)
move $t0, $v0
li $v0, 57
add $v0, $t0, $v0
addi $sp, $sp, -4
sw $v0, 0($sp)
lw $v0, -4($fp)
move $t0, $v0
lw $v0, -8($fp)
sub $v0, $t0, $v0
move $t0, $v0
lw $v0, -4($fp)

modulo:
  bgt $t0, 0, sub_mod
  blt $t0, 0, add_mod
zero_mod:
  li $v0, 0              #resultat
sub_mod:
  sub $t0, $t0, $v0
  bge $t0, $v0, modulo
add_mod:
  add $t0, $t0, $v0
  ble  $t0, $v0, modulo
move $a0, $v0
li $v0, 1
syscall
la $a0, nl
li $v0, 4
syscall
li $v0, 0
jr $ra
