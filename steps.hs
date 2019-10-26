step :: Configuration -> Transition -> [Configuration]
-- 0      0       0  0      0
step (a, b, "") ((d, "", ""), (g, ""))                    -- NONE NONE
    | a == d = [(g, b, "")]
    | otherwise = []

-- 1      0       0  0       1
step (a, b, "") ((d, "", ""), (g, h))                  -- NONE PUSH
    | a == d = [(g, b, h)]
    | otherwise = []

-- 4           0        1  0      0
step (a, (b:bs), "") ((d, [e], ""), (g, ""))             -- RIGHT NONE
    | a == d && b == e = [(g, bs, "")]
    | otherwise = []

-- 5           0        1  0       1
step (a, (b:bs), "") ((d, [e], ""), (g, h))           -- RIGHT PUSH
    | a == d && b == e = [(g, bs, h)]
    | otherwise = []
-- 8      1       0   0       0
step (a, b, c) ((d, "", ""), (g, ""))                 -- NONE NONE
    | a == d = [(g, b, c)]
    | otherwise = []

-- 9      1      0  0       1
step (a, b, c) ((d, "", ""), (g, [h]))                  -- NONE PUSH
    | a == d = [(g, b, h:c)]
    | otherwise = []

-- 10        1        0   1      0
step (a, b, (c:cs)) ((d, "", [f]), (g, ""))             -- NONE POP
    | a == d && c == f = [(g, b, cs)]
    | otherwise = []

-- 11        1        0   1       1
step (a, b, (c:cs)) ((d, "", [f]), (g, [h]))           -- NONE REPLACE
    | a == d && c == f = [(g, b, (h:cs))]
    | otherwise = []

-- 12          1       1  0      0
step (a, (b:bs), c) ((d, [e], ""), (g, ""))             -- RIGHT NONE
    | a == d && b == e = [(g, bs, c)]
    | otherwise = []

-- 13            1          1  0       1
step (a, (b:bs), c) ((d, [e], ""), (g, [h]))      -- RIGHT PUSH
    | a == d && b == e = [(g, bs, h:c)]
    | otherwise = []

-- 14            1          1   1      0
step (a, (b:bs), (c:cs)) ((d, [e], [f]), (g, ""))      -- RIGHT POP
    | a == d && b == e && c == f = [(g, bs, cs)]
    | otherwise = []

-- 15            1          1   1       1
step (a, (b:bs), (c:cs)) ((d, [e], [f]), (g, [h]))    -- RIGHT REPLACE    
    | a == d && b == e && c == f = [(g, bs, (h:cs))]
    | otherwise = []  

-- catch al
step _ _ = []