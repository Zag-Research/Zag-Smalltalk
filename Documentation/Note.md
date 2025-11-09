if an object points to a ContextData and gets moved to the heap, the ContextData has to be moved to the same heap, the Context has to know that the data is there.

So can't depend on the CD being adjacent to the Context

Maybe a forwarding point for the CD?