---
title: Sorting Algorithms In Haskell
published: true
---

## Insertion sort

```haskell
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []
  where
    insert x (y : ys)
      | x < y = x : y : ys
      | otherwise = y : insert x ys
    insert x [] = [x]
```

Since the [`insert`](https://downloads.haskell.org/ghc/latest/docs/libraries/base-4.17.0.0/src/Data.OldList.html#insert) function is in the standard library, it can be rewritten more simply.

```haskell
import Data.List (insert)

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []
```

## Selection sort

```haskell
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = x : selectionSort (delete x xs)
  where
    x = minimum xs
    delete _ [] = []
    delete v (x : xs)
      | v == x = xs
      | otherwise = x : delete v xs
```

use [`delete`](https://downloads.haskell.org/ghc/latest/docs/libraries/base-4.17.0.0/src/Data.OldList.html#delete) to simplify

```haskell
import Data.List (delete)
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = x : selectionSort (delete x xs) where x = minimum xs
```

## Bubble sort

```haskell
bubbleSort :: Ord a => [a] -> [a]
bubbleSort = foldr swap []
  where
    swap x [] = [x]
    swap x (y : xs) = min x y : swap (max x y) xs
```

## Quick sort

```haskell
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
  quickSort (filter (< x) xs)
    ++ [x]
    ++ quickSort (filter (>= x) xs)
```

## Merge sort

```haskell
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    merge [] ys = ys
    merge xs [] = xs
    merge (x : xs) (y : ys)
      | x < y = x : merge xs (y : ys)
      | otherwise = y : merge ys (x : xs)
    (left, right) = splitAt (length xs `div` 2) xs
```

## Shell sort

```haskell
import Data.List (insert, transpose, unfoldr)

shellSort :: (Ord a) => [a] -> [a]
shellSort xs = foldr multiInsertionsort xs gaps
  where
    gaps = takeWhile (< length xs) $ iterate (\h -> 3 * h + 1) 1
    multiInsertionsort h =
      concat
        . transpose
        . map (foldl (flip insert) [])
        . transpose
        . takeWhile (not . null)
        . unfoldr (Just . splitAt h)
        . reverse
```

## References

- [手続き脳による Haskell -- Sorting Algorithms](http://wwwa.pikara.ne.jp/okojisan/haskell-sort/index.html)
- [Sorting Algorithms @ wikipedia](https://en.wikipedia.org/wiki/Sorting_algorithm)
- [Sorting Algorithms @ geeksforgeeks](https://www.geeksforgeeks.org/sorting-algorithms)
