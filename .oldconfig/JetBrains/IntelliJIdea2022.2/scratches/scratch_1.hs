--
--spawnSelected' :: [(String, String)] -> X ()
--spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
--    where conf = def
--                   { gs_cellheight   = 40
--                   , gs_cellwidth    = 180
--                   , gs_cellpadding  = 6
--                   , gs_originFractX = 0.5
--                   , gs_originFractY = 0.5
--                   , gs_font         = myFont
--                   }
--
--runSelectedAction' :: GSConfig (X ()) -> [(String, X ())] -> X ()
--runSelectedAction' conf actions = do
--    selectedActionM <- gridselect conf actions
--    case selectedActionM of
--        Just selectedAction -> selectedAction
--        Nothing -> return ()
--





--myScratchPads :: [NamedScratchpad]
--myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
--                , NS "mocp" spawnMocp findMocp manageMocp
--                , NS "calculator" spawnCalc findCalc manageCalc
--                , NS "browser" spawnBrow findBrow manageBrow
--                ]
