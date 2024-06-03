"Given a sorted array arr[] of size N and a number X, you need to 
"find the number of occurrences of X in given array.

* Parameters:

* it_array: This is the input sorted array.
* iv_x: The value of X that we want to find occurrences for.
* cv_count: This is an output parameter where the count of occurrences will be stored.
* Initialization:

* lv_first_index and lv_last_index: These variables will store the indices of the first and last occurrences of X, respectively.
* lv_left_index and lv_right_index: These variables represent the left and right indices of the current sub-array being processed.
* lv_mid_index: This variable stores the index of the middle element of the sub-array.
* Binary Search for First Occurrence:

* We perform a binary search on the array to find the first occurrence of X.
* If the middle element of the sub-array is equal to X, we update lv_first_index and move the lv_right_index to the left to search for earlier occurrences.
* If the middle element is less than X, we update lv_left_index to search in the right half of the sub-array.
* If the middle element is greater than X, we update lv_right_index to search in the left half of the sub-array.
* Binary Search for Last Occurrence:

* Similar to the first binary search, we perform another binary search to find the last occurrence of X.
* We update lv_last_index if the middle element is equal to X, and move lv_left_index to the right to search for later occurrences.
* Calculating the Count:

* After finding the first and last occurrences, we calculate the count of occurrences by subtracting lv_first_index from lv_last_index and adding 1.
* Result:

* The count of occurrences is stored in the output parameter cv_count.

* This algorithm utilizes binary search twice to find the first and last occurrences of 
* X in the sorted array, with a time complexity of O(log n), where n is the number of elements 
* in the array. The final count operation takes constant time, resulting in a overall time 
* complexity of O(log n).

FORM count_occurrences USING    it_array TYPE TABLE OF i
                               iv_x     TYPE i
                      CHANGING cv_count TYPE i.

  DATA: lv_first_index TYPE i,
        lv_last_index  TYPE i,
        lv_left_index  TYPE i,
        lv_right_index TYPE i,
        lv_mid_index   TYPE i.

  lv_first_index = 0.
  lv_last_index = lines( it_array ) - 1.
  lv_left_index = 0.
  lv_right_index = lines( it_array ) - 1.

  " Finding the first occurrence of X using binary search
  DO WHILE lv_left_index <= lv_right_index.
    lv_mid_index = lv_left_index + trunc( ( lv_right_index - lv_left_index ) / 2 ).

    IF it_array[ lv_mid_index ] = iv_x.
      lv_first_index = lv_mid_index.
      lv_right_index = lv_mid_index - 1.
    ELSEIF it_array[ lv_mid_index ] < iv_x.
      lv_left_index = lv_mid_index + 1.
    ELSE.
      lv_right_index = lv_mid_index - 1.
    ENDIF.
  ENDDO.

  lv_left_index = 0.
  lv_right_index = lines( it_array ) - 1.

  " Finding the last occurrence of X using binary search
  DO WHILE lv_left_index <= lv_right_index.
    lv_mid_index = lv_left_index + trunc( ( lv_right_index - lv_left_index ) / 2 ).

    IF it_array[ lv_mid_index ] = iv_x.
      lv_last_index = lv_mid_index.
      lv_left_index = lv_mid_index + 1.
    ELSEIF it_array[ lv_mid_index ] < iv_x.
      lv_left_index = lv_mid_index + 1.
    ELSE.
      lv_right_index = lv_mid_index - 1.
    ENDIF.
  ENDDO.

  " Calculating the count of occurrences
  cv_count = lv_last_index - lv_first_index + 1.

ENDFORM.
