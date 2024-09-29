module SimChooch.Common

// Define an interface for types with Time
type IHasTime =
    abstract member Time : uint64
    
// Define an interface for types with an Id
type IHasId =
    abstract member Id : uint
