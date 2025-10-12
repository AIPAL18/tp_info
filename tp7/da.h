/*
Dynamic ordered array.
*/

#ifndef DA_H_
#define DA_H_

/*-----------------------------------------------------------------------------
        parameters & settings
-----------------------------------------------------------------------------*/

#ifndef DA_ASSERT
#include <assert.h>
#define DA_ASSERT assert
#endif /* DA_ASSERT */

#ifndef DA_REALLOC
#include <stdlib.h>
#define DA_REALLOC realloc
#endif /* DA_REALLOC */

#ifndef DA_CALLOC
#include <stdlib.h>
#define DA_CALLOC calloc
#endif /* DA_MALLOC */

#ifndef DA_FREE
#include <stdlib.h>
#define DA_FREE free
#endif /* DA_FREE */

#ifndef DA_PRINT
#include <stdio.h>
#define DA_PRINT printf
#endif /* DA_PRINT */

#ifndef DA_FREE_VALUE
// Define it if values need to be freed
#define DA_FREE_VALUE(value) ((void)0)
#endif /* DA_FREE_VALUE */

#ifndef DA_INIT_CAP
#define DA_INIT_CAP 256
#endif

/*-----------------------------------------------------------------------------
        constructor
-----------------------------------------------------------------------------*/

#define da_define(name, item_type) \
    typedef struct name##_da { \
        item_type *items; \
        size_t count; \
        size_t capacity; \
    } name

#define da_new() \
    {0}

#define da_init(da) \
    do { \
        DA_ASSERT((da) == NULL); \
        (da) = DA_CALLOC(1, sizeof(typeof(*(da)))); \
        DA_ASSERT((da) != NULL && "Buy more RAM lol"); /* Tsoding */ \
        (da)->count = 0; \
        (da)->capacity = 0; \
    } while(0)

/*-----------------------------------------------------------------------------
        memory related
-----------------------------------------------------------------------------*/

#define da_reserve(da, expected_capacity) \
    do { \
        if ((expected_capacity) > (da)->capacity) { \
            if ((da)->capacity == 0) { \
                (da)->capacity = DA_INIT_CAP; \
            } \
            while ((expected_capacity) > (da)->capacity) { \
                (da)->capacity *= 2; \
            } \
            /*TODO: is it memory safe ?*/ \
            (da)->items = DA_REALLOC((da)->items, (da)->capacity * sizeof(*(da)->items)); \
            DA_ASSERT((da)->items != NULL && "Buy more RAM lol"); \
        } \
    } while (0)

#define da_resize(da, new_size) \
    do { \
        da_reserve((da), new_size); \
        (da)->count = (new_size); \
    } while (0)

// memory unsafe
#define da_free(da) \
    do { \
        if ((da) == NULL) break; \
        while ((da)->count-- > 0) { \
            DA_FREE_VALUE((da)->items[(da)->count]); \
        } \
        DA_FREE((da)->items); \
        DA_FREE((da)); \
    } while(0)

/*-----------------------------------------------------------------------------
        access
-----------------------------------------------------------------------------*/

// get the last element
#define da_last(da) \
    (da)->items[(DA_ASSERT((da)->count > 0), (da)->count-1)]

// get the i-th element
#define da_get(da, i) \
    (da)->items[(DA_ASSERT(i >= 0 && (size_t)i < (da)->count), i)]

// return true if da is empty
#define da_empty(da) \
    ((da)->count == 0)

/*-----------------------------------------------------------------------------
        editing
-----------------------------------------------------------------------------*/

// Append an item to a dynamic array
#define da_append(da, item) \
    do { \
        da_reserve((da), (da)->count + 1); \
        (da)->items[(da)->count++] = (item); \
    } while (0)

// Append several items to a dynamic array
#define da_append_several(da, new_items, new_items_count) \
    do { \
        da_reserve((da), (da)->count + (new_items_count)); \
        memcpy((da)->items + (da)->count, (new_items), (new_items_count)*sizeof(*(da)->items)); \
        (da)->count += (new_items_count); \
    } while (0)

#define da_pop(da) \
    (da)->items[(DA_ASSERT((da)->count > 0), --(da)->count)]

// remove the i-th element without caring for the order
#define da_remove_unordered(da, i) \
    do { \
        size_t j = (i); \
        DA_ASSERT(j < (da)->count); \
        (da)->items[j] = (da)->items[--(da)->count]; \
    } while(0)

// remove the i-th element
#define da_remove(da, i) \
    do { \
        size_t j = (i); \
        DA_ASSERT(j < (da)->count); \
        --(da)->count; \
        for (size_t k = j; k < (da)->count; ++k) { \
            (da)->items[k] = (da)->items[k + 1]; \
        } \
    } while(0)

#define da_pop_index(da, i) \
    da_get(da, i); \
    da_remove(da, i)

/*-----------------------------------------------------------------------------
        loop
-----------------------------------------------------------------------------*/

#define da_foreach(da, item_type, item_name) \
    for (item_type *item_name = (da)->items; item_name < (da)->items + (da)->count; ++item_name)

#define da_foreach_auto(da, item_name) \
    for (typeof((da)->items) item_name = (da)->items; item_name < (da)->items + (da)->count; ++item_name)

/*-----------------------------------------------------------------------------
        display
-----------------------------------------------------------------------------*/

// display 1D dynamic array
#define da_display(da, item_type, format) \
    do { \
        if ((da)->count == 0) DA_PRINT("[]"); \
        else { \
        DA_PRINT("["); \
        da_foreach(da, item_type, item) { \
            DA_PRINT(format, *item);\
            DA_PRINT(", "); \
        } \
        DA_PRINT("]\n"); \
    } } while(0)

#endif // DA_H_

/*
TODO: rendre l'API conforme (da_display, types en paramètre, etc.)
*/

/*
FROM https://github.com/tsoding/nob.h

   Public Domain (www.unlicense.org)
   This is free and unencumbered software released into the public domain.
   Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
   software, either in source code form or as a compiled binary, for any purpose,
   commercial or non-commercial, and by any means.
   In jurisdictions that recognize copyright laws, the author or authors of this
   software dedicate any and all copyright interest in the software to the public
   domain. We make this dedication for the benefit of the public at large and to
   the detriment of our heirs and successors. We intend this dedication to be an
   overt act of relinquishment in perpetuity of all present and future rights to
   this software under copyright law.
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
   WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/
