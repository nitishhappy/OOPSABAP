" Given an array of n elements that contains elements from 0 to n-1, with any of these numbers appearing any number of times. Find these repeating numbers in O(n) and use only constant memory space. ((EOL))

" Approach:To find the repeating numbers in an array that contains elements from 0 to n-1, with any of these numbers appearing any number of times, we can use a clever algorithm that leverages the properties of the array indices to achieve an O(n) time complexity and O(1) space complexity.

* Here's how we can achieve this in ABAP:

* Traverse the array.
* For each element arr[i], calculate the index arr[i] % n.
* Increment the value at this calculated index by n.
* Traverse the array again. If any value is greater than or equal to 2n, then the index of that value is a repeating element.

* Concept : If a no. is repeated, then that no as an index is also repeated. So for that index, we will Increment by array size(n) for each occurance. IF it is a single accurance then it will always be less than 2n.

REPORT zfind_repeating_numbers.

DATA: lt_array TYPE TABLE OF i WITH EMPTY KEY,
      lv_n TYPE i,
      lv_i TYPE i,
      lv_index TYPE i,
      lt_result TYPE TABLE OF i WITH EMPTY KEY.

START-OF-SELECTION.
  " Initialize the array
  lt_array = VALUE #( (0) (4) (3) (2) (7) (8) (2) (3) (1) (1) ).

  " Calculate the size of the array
  lv_n = lines( lt_array ).

  " Traverse the array
  LOOP AT lt_array INTO lv_i.
    lv_index = lv_i MOD lv_n.
    lt_array[ lv_index + 1 ] = lt_array[ lv_index + 1 ] + lv_n.
  ENDLOOP.

  " Check for repeating elements
  LOOP AT lt_array INTO lv_i.
    IF lv_i DIV lv_n >= 2.
      lv_index = sy-tabix - 1.
      APPEND lv_index TO lt_result.
    ENDIF.
  ENDLOOP.

  " Display the result
  IF lt_result IS INITIAL.
    WRITE: / 'No repeating numbers found.'.
  ELSE.
    WRITE: / 'Repeating numbers are:'.
    LOOP AT lt_result INTO lv_i.
      WRITE: / lv_i.
    ENDLOOP.
  ENDIF.

