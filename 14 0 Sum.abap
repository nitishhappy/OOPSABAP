*&---------------------------------------------------------------------*
*& Report YNITS_0SUM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*REPORT ynits_0sum..

" Given an array of positive and negative numbers, the task is to find if there is a subarray (of size at least one) with 0 sum.

* Examples:

* Input: {4, 2, -3, 1, 6}
* Output: true
* Explanation:
* There is a subarray with zero sum from index 1 to 3.

* Input: {4, 2, 0, 1, 6}
* Output: true
* Explanation: The third element is zero. A single element is also a sub-array.

* Input: {-3, 2, 3, 1, 6}
* Output: false0

* To find a subarray with a sum of 0 in an array, we can use a hash map to keep track of the cumulative sum at each index. If the cumulative sum at two different indices is the same, it means the subarray between these indices has a sum of 0.

* Explanation:
* Cumulative Sum: As we traverse the array, we keep a running total of the elements seen so far. This running total is called the cumulative sum.

* Hash Map: We use a hash map to store the cumulative sum and its corresponding index. If the cumulative sum at a particular index is seen before, it means the elements between the previous index and the current index sum to 0.

* Subarray Detection: If the cumulative sum is zero at any point, it means there is a subarray from the start of the array to the current index that sums to 0. Additionally, if any cumulative sum repeats, the elements between the first occurrence of
"this sum and the current index sum to 0.

REPORT z_find_zero_sum_subarray.

TYPES: BEGIN OF ty_element,
         index TYPE i,
         sum   TYPE i,
       END OF ty_element.

DATA: lt_array   TYPE TABLE OF i WITH EMPTY KEY,
      lt_hashmap TYPE TABLE OF ty_element WITH EMPTY KEY,
      ls_element TYPE ty_element,
      lv_sum     TYPE i,
      lv_start   TYPE i,
      lv_end     TYPE i,
      found      TYPE abap_bool.

START-OF-SELECTION.

  " Example input
  lt_array = VALUE #( ( 3 ) ( 4 ) ( -7 ) ( 3 ) ( 1 ) ( 3 ) ( 1 ) ( -4 ) ).

  " Initialize variables
  CLEAR: lv_sum, found.

  " Initialize hash map with a cumulative sum of 0 at index -1
  ls_element-index = 1.
  ls_element-sum   = 0.
  APPEND ls_element TO lt_hashmap.

  " Traverse the array
  LOOP AT lt_array INTO DATA(lv_elem).
    WRITE: lv_elem, ' ,'.
  ENDLOOP.
  LOOP AT lt_array INTO lv_elem.
    " Update cumulative sum
    DATA(lv_index) = sy-tabix.
    lv_sum = lv_sum + lv_elem.


    " Check if the cumulative sum has been seen before
    READ TABLE lt_hashmap WITH KEY sum = lv_sum INTO ls_element.
    IF sy-subrc = 0.
      " Subarray with sum 0 found
*      lv_start = ls_element-index + 1.
      lv_start = ls_element-index.
      lv_end   = lv_index.
      WRITE: / 'Subarray with 0 sum found ', ls_element-sum,  'from index', lv_start, 'to', lv_end.
      found = abap_true.
*      EXIT.
    ELSE.
      " Add cumulative sum to hash map
      ls_element-index = lv_index.
      ls_element-sum   = lv_sum.
      APPEND ls_element TO lt_hashmap.
    ENDIF.
  ENDLOOP.
*
*  " Output result
*  IF found = abap_true.
*    WRITE: / 'Subarray with 0 sum found from index', lv_start, 'to', lv_end.
*  ELSE.
*    WRITE: / 'No subarray with 0 sum found'.
*  ENDIF.

* Explanation of the Code:
* Initialization:

* We define a table lt_array to store the input array.
* We define a table lt_hashmap to store the cumulative sums and their corresponding indices.
* We initialize lv_sum to store the cumulative sum.
* Starting the Hash Map:

* We initialize the hash map with a cumulative sum of 0 at index -1 to handle cases where a subarray starts from the beginning.
* Traversing the Array:

* We loop through each element of the array.
* For each element, we update the cumulative sum.
* We check if this cumulative sum has been seen before using the hash map.
* If the cumulative sum has been seen, we determine the start and end indices of the subarray with sum 0.
* If the cumulative sum is new, we add it to the hash map with the current index.
* Output:

* If a subarray with sum 0 is found, we output the start and end indices.
* If no such subarray is found, we output that no subarray with sum 0 was found.
