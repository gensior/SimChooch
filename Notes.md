# Initial Train Positions

Legend

| Symbol | Meaning          |
|:------:|:-----------------|
| `[1]`  | station id       |
|  `o`   | station          |
|  `x`   | train            |
|  `-`   | one mile of rail |

```
[1] [2]  [3]   [4] [5]
 o---o----o-----o---o
 x
```

Calculate the `total_distance` between all stations (and multiply by `2` if the line contains trains travelling in two directions).

Initialize the following values:

|               Key | Value                         |
|------------------:|:------------------------------|
|  `total_distance` | distance b/w all stations * 2 |
| `current_station` | 0                             |
|   `current_train` | 0                             |
|     `travel_time` | `time_between_trains`         |

We start at the first station. Be default, we place a train there. Next, we need to determine where the next train will be.

1. Take the `travel_time` value and subtract `station_wait_time`. What's left over is the time spent travelling towards the next station.

    ```
    travel_time = travel_time - station_wait_time
    ```
2. Next, multiply the `travel_time` by `train_speed` to determine the distance the train would cover if it were allowed to use up all of its travel time.

    ```
    travel_distance = travel_time * train_speed
    ```
3. Now we need to see how far this travel distance will take us. Look up the `station_distance` to the next station and see if it is greater than or less than the `travel_distance`.
    * If `station_distance` is greater than `travel_distance`, subtract `travel_distance` from `station_distance` to get `remaining_distance`. 
      * Create train arrival event based on `remaining_distance` and `train_speed`. 
      * Create a new train and add the remaining 
    * If `station_distance` is less than `travel_distance`, subtract `station_distance` from both `travel_distance` and `