
* Find the Factorial of a large number like 100
* Why conventional way of computing factorial fails for large numbers?
* A factorial of 100 has 158 digits. It is not possible to store these many digits even if we use long int. 

REPORT z_large_factorial.

TYPES: BEGIN OF ty_bigint,
         number TYPE string,
       END OF ty_bigint.

DATA: lv_num TYPE i VALUE 100,
      lv_result TYPE ty_bigint,
      lv_carry TYPE i,
      lv_temp TYPE i,
      lv_len TYPE i,
      lv_digit TYPE string,
      lv_index TYPE i.

START-OF-SELECTION.

  " Initialize result as 1
  lv_result-number = '1'.

  " Loop from 2 to the desired number (100)
  DO lv_num TIMES.
    " Increment loop index (1-based, hence +1)
    lv_index = sy-index + 1.
    
    " Initialize carry to 0
    lv_carry = 0.
    
    " Multiply each digit of the result by lv_index
    lv_len = STRLEN( lv_result-number ).
    DO lv_len TIMES.
      lv_temp = lv_carry + ( lv_result-number+lv_len-sy-index(1) * lv_index ).
      lv_carry = lv_temp DIV 10.
      lv_result-number+lv_len-sy-index(1) = lv_temp MOD 10.
    ENDDO.
    
    " If there is any carry left, add it to the result
    WHILE lv_carry > 0.
      lv_result-number = |{ lv_carry MOD 10 }{ lv_result-number }|.
      lv_carry = lv_carry DIV 10.
    ENDWHILE.
  ENDDO.

  " Output the result
  WRITE: / 'Factorial of', lv_num, 'is:', lv_result-number.

