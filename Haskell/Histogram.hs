import Data.List

area xs = length xs * minimum xs

largest_area = maximum . map area . segments

isNotEmpty x = not ([] == x)

segments xs = filter isNotEmpty _segments
       where
               _segments = concat . map tails . inits $ xs
               --_segments = concat . map inits . tails $ xs
