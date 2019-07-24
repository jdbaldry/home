local util = import 'util.libsonnet';

local not_array = 0;
local empty_array = [];
local simple_array = [1, 2, 3];
local two_dimensional = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
local mismatched_lengths = [[1, 2], [4, 5, 6], [7, 8], [10, 11, 12]];
{
    // RUNTIME ERROR: argument must be an array
    // not: util.transpose(not_array)
    test_empty: util.transpose(empty_array),
    // TODO: should this be null or [ ],
    test_simple: util.transpose(simple_array),
    test_two_dimensional: util.transpose(two_dimensional),
    test_mismatched_lengths: util.transpose(mismatched_lengths),
}  