Problem:
We have a detabase table called Family with field parend_id and child_id. Given a parent_id as input, provide a recursive solution to retreive its grand child and great-grandchilds by fetching data from Family. Family is a data heavy table so also optimize the performance.

Solution:
To retrieve the grandchild and great-grandchild records for a given parent_id from a heavy data table named Family, we need a recursive solution that is optimized for performance. This can be done using ABAP code that leverages the capabilities of internal tables and efficient querying.

Here is a detailed approach to achieve this:

Define the Recursive Method: Create a method that recursively retrieves child IDs.
Use Internal Tables Efficiently: Store intermediate results in internal tables to minimize database calls.
Optimize Database Access: Fetch data in bulk where possible to reduce the number of database accesses.

Step-by-Step Solution
1. Define a Structure and Table Type
First, define a structure to represent the Family table and a corresponding table type.

TYPES: BEGIN OF ty_family,
         parent_id TYPE i,
         child_id  TYPE i,
       END OF ty_family.

TYPES: tt_family TYPE TABLE OF ty_family WITH EMPTY KEY.

2. Create the Recursive Class
Define a class with a recursive method to fetch the descendants.

CLASS lcl_family DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: get_descendants
      IMPORTING iv_parent_id TYPE i
      RETURNING VALUE(rt_descendants) TYPE tt_family.
  PRIVATE SECTION.
    CLASS-METHODS: fetch_children
      IMPORTING iv_parent_id TYPE i
      CHANGING ct_family TYPE tt_family.
ENDCLASS.

CLASS lcl_family IMPLEMENTATION.

  METHOD get_descendants.
    DATA: lt_family TYPE tt_family,
          lt_descendants TYPE tt_family.

    " Fetch direct children
    fetch_children( iv_parent_id = iv_parent_id
                    CHANGING ct_family = lt_family ).

    " Recursive call for each child to fetch their children
    LOOP AT lt_family INTO DATA(ls_family).
      lt_descendants = get_descendants( iv_parent_id = ls_family-child_id ).
      APPEND LINES OF lt_descendants TO lt_family.
    ENDLOOP.

    rt_descendants = lt_family.
  ENDMETHOD.

  METHOD fetch_children.
    " Fetch children for the given parent_id
    SELECT parent_id, child_id
      FROM family
      INTO TABLE @ct_family
      WHERE parent_id = @iv_parent_id.
  ENDMETHOD.

ENDCLASS.

3. Use the Class to Retrieve Descendants
Finally, call the method and display the results.

START-OF-SELECTION.
  DATA: lt_descendants TYPE tt_family,
        lv_parent_id TYPE i.

  " Input parent_id
  lv_parent_id = 100.

  " Get descendants
  lt_descendants = lcl_family=>get_descendants( iv_parent_id = lv_parent_id ).

  " Display the results
  LOOP AT lt_descendants INTO DATA(ls_descendant).
    WRITE: / 'Parent ID:', ls_descendant-parent_id, 'Child ID:', ls_descendant-child_id.
  ENDLOOP.

Optimizing Performance
Bulk Fetching: Fetch children in bulk to minimize database calls.
Indexing: Ensure the parent_id field is indexed for faster querying.
Limiting Depth: If the depth of recursion is known and limited (e.g., to 3 levels for grand and great-grandchildren), optimize the recursion by terminating after reaching the required depth.
Enhanced Recursive Method with Depth Control

CLASS lcl_family DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: get_descendants
      IMPORTING iv_parent_id TYPE i
                iv_depth TYPE i DEFAULT 2
      RETURNING VALUE(rt_descendants) TYPE tt_family.
  PRIVATE SECTION.
    CLASS-METHODS: fetch_children
      IMPORTING iv_parent_id TYPE i
      CHANGING ct_family TYPE tt_family.
ENDCLASS.

CLASS lcl_family IMPLEMENTATION.

  METHOD get_descendants.
    DATA: lt_family TYPE tt_family,
          lt_descendants TYPE tt_family.

    " Fetch direct children
    fetch_children( iv_parent_id = iv_parent_id
                    CHANGING ct_family = lt_family ).

    " Recursive call for each child to fetch their children
    IF iv_depth > 1.
      LOOP AT lt_family INTO DATA(ls_family).
        lt_descendants = get_descendants( iv_parent_id = ls_family-child_id iv_depth = iv_depth - 1 ).
        APPEND LINES OF lt_descendants TO lt_family.
      ENDLOOP.
    ENDIF.

    rt_descendants = lt_family.
  ENDMETHOD.

  METHOD fetch_children.
    " Fetch children for the given parent_id
    SELECT parent_id, child_id
      FROM family
      INTO TABLE @ct_family
      WHERE parent_id = @iv_parent_id.
  ENDMETHOD.

ENDCLASS.

Usage with Depth Control

START-OF-SELECTION.
  DATA: lt_descendants TYPE tt_family,
        lv_parent_id TYPE i.

  " Input parent_id
  lv_parent_id = 100.

  " Get descendants up to 2 levels deep (grand and great-grandchildren)
  lt_descendants = lcl_family=>get_descendants( iv_parent_id = lv_parent_id iv_depth = 2 ).

  " Display the results
  LOOP AT lt_descendants INTO DATA(ls_descendant).
    WRITE: / 'Parent ID:', ls_descendant-parent_id, 'Child ID:', ls_descendant-child_id.
  ENDLOOP.
