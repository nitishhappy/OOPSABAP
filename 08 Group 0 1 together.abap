" Problem: An array contains both positive and negative numbers in random order. Rearrange the array elements so that all negative numbers appear before all positive numbers.

* Approach:
* To rearrange the array elements so that all negative numbers appear before all positive numbers, you can use the two-pointer approach. This approach involves maintaining two pointers, one starting from the beginning of the array and moving forward, and the other starting from the end of the array and moving backward. Here's the ABAP implementation:

FORM rearrange_array
  USING    it_array TYPE TABLE OF i
  CHANGING ct_rearranged_array TYPE TABLE OF i.

  DATA: lv_left_index  TYPE i VALUE 1,
        lv_right_index TYPE i VALUE lines( it_array ),
        lv_temp        TYPE i.

  " Copy the input array to rearranged array
  ct_rearranged_array = it_array[].

  DO WHILE lv_left_index <= lv_right_index.
    " If element at left index is negative, move to next element
    IF ct_rearranged_array[ lv_left_index ] < 0.
      lv_left_index = lv_left_index + 1.
    ELSE.
      " If element at right index is positive, move to previous element
      IF ct_rearranged_array[ lv_right_index ] > 0.
        lv_right_index = lv_right_index - 1.
      ELSE.
        " Swap elements at left and right indices
        lv_temp = ct_rearranged_array[ lv_left_index ].
        ct_rearranged_array[ lv_left_index ] = ct_rearranged_array[ lv_right_index ].
        ct_rearranged_array[ lv_right_index ] = lv_temp.
        lv_left_index = lv_left_index + 1.
        lv_right_index = lv_right_index - 1.
      ENDIF.
    ENDIF.
  ENDDO.

ENDFORM.

* You can call this form with the input array it_array and an empty table ct_rearranged_array to store the rearranged elements.

* This algorithm works by maintaining two pointers (lv_left_index and lv_right_index) that traverse the array from both ends. It swaps elements if the element at the left index is positive and the element at the right index is negative, ensuring that all negative numbers appear before all positive numbers in the rearranged array.

* The time complexity of this algorithm is O(n), where n is the number of elements in the array, as it iterates through the array only once. Therefore, it efficiently rearranges the array elements as required.
