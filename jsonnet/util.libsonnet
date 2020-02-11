{
    // errorEmpty returns an error message with the provided function name func.
    local errorEmpty(func) = error 'must not be an empty array: %s' % func,
    local errorNotArray(func) = error 'must be an array: %s' % func,

    // head is the first element of an array.
    // For example,
    // head([1, 2, 3, 4])
    // 1
    head(x)::
        if !(std.isArray(x)) then errorNotArray('head') else
        if std.length(x) == 0 then errorEmpty('head') else x[0],

    // tail is all elements apart from the head of the array.
    // For example,
    // tail([1, 2, 3, 4])
    // [2, 3, 4],
    tail(x)::
        if !(std.isArray(x)) then errorNotArray('tail') else
        if std.length(x) == 0 then errorEmpty('tail') else x[1:],


    // last is the last element of an array.
    // For example,
    // last([1, 2, 3, 4])
    // 4
    last(x)::
        if !(std.isArray(x)) then errorNotArray('last') else
        if std.length(x) == 0 then errorEmpty('last') else std.reverse(x)[0],

    // transpose transposes the rows and columns of its argument.
    // For example,
    // transpose([[1, 2, 3], [4, 5, 6]])
    // [[1, 4], [2, 5], [3, 6]]
    transpose(a)::
        if !(std.isArray(a)) then 
            errorNotArray('transpose')
        else if std.length(a) == 0 then 
            []
        else if std.isArray($.head(a)) then
            if std.length($.head(a)) == 0 then
                $.transpose($.tail(a))
        else
            local x = $.head($.head(a));
            local xs = $.tail($.head(a));
            local xss = $.tail(a);
            [[x] + [$.head(h) for h in xss if std.length(h) != 0]] + $.transpose([xs] + [$.tail(t) for t in xss if std.length(t) != 0]),
}