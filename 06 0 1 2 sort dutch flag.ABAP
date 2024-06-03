
* Given an array A[] consisting of only 0s, 1s, and 2s. The task is to write a 
* function that sorts the given array. The functions should put all 0s first, 
* then all 1s and all 2s in last.

FORM sort_array USING    it_array TYPE TABLE OF i
                  CHANGING ct_sorted_array TYPE TABLE OF i.

  DATA: lv_low  TYPE i VALUE 0,
        lv_mid  TYPE i VALUE 0,
        lv_high TYPE i VALUE lines( it_array ) - 1,
        lv_temp TYPE i.

  DO WHILE lv_mid <= lv_high.
    CASE it_array[ lv_mid ].
      WHEN 0.
        " Swap lv_low and lv_mid elements
        lv_temp = it_array[ lv_low ].
        it_array[ lv_low ] = it_array[ lv_mid ].
        it_array[ lv_mid ] = lv_temp.
        lv_low = lv_low + 1.
        lv_mid = lv_mid + 1.
      WHEN 1.
        lv_mid = lv_mid + 1.
      WHEN 2.
        " Swap lv_mid and lv_high elements
        lv_temp = it_array[ lv_mid ].
        it_array[ lv_mid ] = it_array[ lv_high ].
        it_array[ lv_high ] = lv_temp.
        lv_high = lv_high - 1.
    ENDCASE.
  ENDDO.

  " Copy sorted array to output parameter
  ct_sorted_array = it_array[].

ENDFORM.

* This algorithm maintains three pointers lv_low, lv_mid, and lv_high to partition the 
* array into three sections: 0s, 1s, and 2s. It iterates through the array once and 
* swaps elements accordingly based on their values. This approach ensures that all 
* 0s are placed before 1s, and all 1s are placed before 2s, resulting in a sorted array.

* The time complexity of this algorithm is O(n), where n is the number of elements in the 
* array, as it iterates through the array only once. Therefore, it's efficient for sorting 
* arrays with 0s, 1s, and 2s.
