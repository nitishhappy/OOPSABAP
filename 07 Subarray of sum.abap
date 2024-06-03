" Given an array arr[] of non-negative integers and an integer sum, find a subarray that adds to a given sum.

* Note: There may be more than one subarray with sum as the given sum, print first such subarray. 

* To find a subarray in an array that adds up to a given sum, you can use the sliding 
* window technique. This approach allows you to iterate through the array while maintaining 
* a window of elements that may potentially form the desired sum. 

FORM find_subarray_sum_matching_sum
  USING    it_array TYPE TABLE OF i
  CHANGING cv_start_index TYPE i
          cv_end_index TYPE i
          cv_found TYPE c.

  DATA: lv_sum          TYPE i,
        lv_current_sum  TYPE i,
        lv_start_index  TYPE i VALUE 1,
        lv_end_index    TYPE i VALUE 1.

  lv_sum = 15. " Replace with the desired sum

  " Initialize current sum and start index
  lv_current_sum = it_array[ 1 ].

  " Iterate through the array
  DO WHILE lv_end_index <= lines( it_array ).
    " Check if current sum equals the desired sum
    IF lv_current_sum = lv_sum.
      cv_start_index = lv_start_index.
      cv_end_index = lv_end_index.
      cv_found = abap_true.
      EXIT. " Exit loop if subarray found
    ENDIF.

    " If current sum is less than the desired sum, extend the window
    IF lv_current_sum < lv_sum.
      lv_end_index = lv_end_index + 1.
      lv_current_sum = lv_current_sum + it_array[ lv_end_index ].
    ELSE. " If current sum is greater than the desired sum, shrink the window
      lv_current_sum = lv_current_sum - it_array[ lv_start_index ].
      lv_start_index = lv_start_index + 1.
    ENDIF.
  ENDDO.

  IF cv_found IS INITIAL.
    cv_start_index = 0.
    cv_end_index = 0.
  ENDIF.

ENDFORM.

*You can call this form with the input array it_array and provide the desired sum. The form will populate the cv_start_index and cv_end_index parameters with the indices of the first subarray found that matches the given sum.

*This algorithm works by maintaining a sliding window represented by lv_start_index and lv_end_index. It iterates through the array, adjusting the window size and sum based on whether the current sum is less than or greater than the desired sum. If the current sum equals the desired sum, it indicates that a subarray with the given sum has been found.

*The time complexity of this algorithm is O(n), where n is the number of elements in the array, as it iterates through the array only once. Therefore, it efficiently finds the first subarray that matches the given sum.
