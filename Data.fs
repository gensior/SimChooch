module SimChooch.Data

open System.Collections.Generic
open SimChooch.Common
open SimChooch.Domain

type InMemoryArray<'T when 'T : equality>(capacity: int) =
    let data: 'T array = Array.zeroCreate capacity
    let mutable count = 0
    
    member this.Insert (record: 'T) =
        data[count] <- record
        count <- count + 1
        count
        
    member this.Item
        with get(index: int) : 'T =
            if index < 0 || index >= data.Length then
                raise (System.IndexOutOfRangeException("Index is out of range."))
            data[index]
    
    member this.Count = count
    
    member this.AsArray() = data
        
type InMemoryMap<'T when 'T:> IHasId>() =
    let data = Dictionary<uint, 'T>()
    
    member this.Add(item: 'T) =
        data.Add(item.Id, item)

    member this.Get (id: uint) =
        data[id]

    member this.Count() = data.Count

type EventQueue<'T when 'T:> IHasTime>() =
    let queue = PriorityQueue<'T, uint64>()
    
    member this.Enqueue(item: 'T) =
        queue.Enqueue(item, item.Time)
        
    member this.Dequeue() =
        queue.Dequeue()
        
    member this.Peek() =
        queue.Peek()
        
    member this.Count =
        queue.Count