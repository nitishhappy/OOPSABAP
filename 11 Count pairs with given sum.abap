" * Given an array of N integers, and an integer K, the task is to find the number of pairs of integers in the array whose sum is equal to K. ((EOL))

REPORT ZPAIRS_WITH_SUM.

DATA: it_array TYPE TABLE OF i,
      lv_sum TYPE i,
      lv_count TYPE i,
      lv_left TYPE i,
      lv_right TYPE i.

START-OF-SELECTION.
  " Sample array initialization
  it_array = VALUE #( (1) (2) (3) (4) (5) ).

  " Sample sum
  lv_sum = 7.

  " Display original array
  WRITE: / 'Original Array:'.
  LOOP AT it_array INTO DATA(lv_element).
    WRITE lv_element.
  ENDLOOP.

  " Find pairs with sum equal to lv_sum
  SORT it_array ASCENDING.

  lv_count = 0.
  lv_left = 1.
  lv_right = lines( it_array ).

  WHILE lv_left < lv_right.
    IF it_array[ lv_left ] + it_array[ lv_right ] = lv_sum.
      lv_count = lv_count + 1.
      lv_left = lv_left + 1.
      lv_right = lv_right - 1.
    ELSEIF it_array[ lv_left ] + it_array[ lv_right ] < lv_sum.
      lv_left = lv_left + 1.
    ELSE.
      lv_right = lv_right - 1.
    ENDIF.
  ENDWHILE.

  " Display result
  WRITE: / 'Number of pairs with sum', lv_sum, 'is', lv_count.

" This program initializes an array it_array with sample values and a target sum lv_sum. It then sorts the array in ascending order and uses a two-pointer technique to find pairs of integers whose sum is equal to lv_sum. The lv_left pointer starts from the beginning of the array, and the lv_right pointer starts from the end. Depending on the sum of the elements pointed by the two pointers, they are adjusted to move towards each other to find all possible pairs.

"this algorithm has a time complexity of O(n log n) due to the sorting operation, where n is the number of elements in the array. The pair finding operation has a linear time complexity of O(n), where n is the number of elements in the array. Therefore, the overall time complexity is dominated by the sorting operation. ((EOL)) ((EOL))