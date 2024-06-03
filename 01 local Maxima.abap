" Write SAP ABAP Code for below: Given an array arr of n elements that is first strictly increasing and then maybe strictly decreasing, find the maximum element in the array.
* " 
* Note: If the array is increasing then just print the last element will be the maximum value.

* Example:

* Input: array[]= {5, 10, 20, 15}
* Output: 20
* Explanation: The element 20 has neighbors 10 and 15, both of them are less than 20.Input: array[] = {10, 20, 15, 2, 23, 90, 67}
* Output: 20 or 90
* Explanation: The element 20 has neighbors 10 and 15, both of them are less than 20, similarly 90 has neighbors 23 and 67. ((EOL)) ((EOL))

REPORT zfind_peak_element.

TYPES: BEGIN OF ty_array,
         value TYPE i,
       END OF ty_array.

DATA: lt_array TYPE TABLE OF ty_array,
      ls_array TYPE ty_array,
      lv_max   TYPE i.

* Sample input array
lt_array = VALUE #( ( value = 5 ) ( value = 10 ) ( value = 20 ) ( value = 15 ) ).
* lt_array = VALUE #( ( value = 10 ) ( value = 20 ) ( value = 15 ) ( value = 2 ) ( value = 23 ) ( value = 90 ) ( value = 67 ) ).

* Function to find peak element
FORM find_peak_element USING lt_array TYPE TABLE OF ty_array
                       CHANGING lv_max TYPE i.
  DATA: lv_left  TYPE i,
        lv_right TYPE i,
        lv_mid   TYPE i.

  lv_left  = 1.
  lv_right = lines( lt_array ).

  WHILE lv_left <= lv_right.
    lv_mid = ( lv_left + lv_right ) DIV 2.
    
    READ TABLE lt_array INDEX lv_mid INTO DATA(ls_mid).
    IF lv_mid = 1.
      READ TABLE lt_array INDEX ( lv_mid + 1 ) INTO DATA(ls_mid_next).
      IF ls_mid-value > ls_mid_next-value.
        lv_max = ls_mid-value.
        RETURN.
      ELSE.
        lv_left = lv_mid + 1.
      ENDIF.
    ELSEIF lv_mid = lines( lt_array ).
      READ TABLE lt_array INDEX ( lv_mid - 1 ) INTO DATA(ls_mid_prev).
      IF ls_mid-value > ls_mid_prev-value.
        lv_max = ls_mid-value.
        RETURN.
      ELSE.
        lv_right = lv_mid - 1.
      ENDIF.
    ELSE.
      READ TABLE lt_array INDEX ( lv_mid - 1 ) INTO ls_mid_prev.
      READ TABLE lt_array INDEX ( lv_mid + 1 ) INTO ls_mid_next.
      IF ls_mid-value > ls_mid_prev-value AND ls_mid-value > ls_mid_next-value.
        lv_max = ls_mid-value.
        RETURN.
      ELSEIF ls_mid_prev-value < ls_mid-value AND ls_mid-value < ls_mid_next-value.
        lv_left = lv_mid + 1.
      ELSE.
        lv_right = lv_mid - 1.
      ENDIF.
    ENDIF.
  ENDWHILE.
ENDFORM.

* Call the form to find the peak element
PERFORM find_peak_element USING lt_array CHANGING lv_max.

* Print the maximum element
WRITE: / 'Maximum Element:', lv_max.
