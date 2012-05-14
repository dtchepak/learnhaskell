using System;
using System.Collections.Generic;

namespace MyProg {
public class Program {
    public static void Main() {
        var list = new [] {1,2,3,4,5};
        var result = FoldLeft((a,x) => a+1, 0, list);
        Console.WriteLine(result);
    }

    public static TResult FoldLeft<TItem, TResult>(
        Func<TResult, TItem, TResult> combine, 
        TResult initialValue, 
        IEnumerable<TItem> items) 
    {
        var accumulator = initialValue;
        foreach (var item in items) {
            accumulator = combine(accumulator, item);
        }
        return accumulator;
    }
}
}
