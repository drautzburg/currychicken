-- | The final result is a 'Move'. It is computed by refining
-- 'Intermediate's.

{-
data Move = Mov {
            movTime :: Instant,
            movItem :: Item,
            movPos  :: Position
}deriving (Eq, Show)


-- | execute a 'Move' modifying the 'System'
movExec :: Move -> State System ()
movExec (Mov t itm pos) = do
    sys <- get
    itemDb <- gets sysItems
    let itemDb' = itmMove t (itmId itm) pos itemDb
    modify $ (\s -> s{sysItems=itemDb'})
    return ()

-}





{-
-- | Shorthand
itrWait waits events = Intermediate (Left waits) events
-- | Shorthand
itrOkay a events = Intermediate (Right a) events
        

-- | Create Waits for all Source locations
waitAnySrc :: Process -> [Wait]
waitAnySrc prc = map wait (prcSrcLocs prc)
        where
            wait loc = Wait Adding (At loc) (prcId prc)


-- | Get Items at src ports or wait for one to arrive
chkSrcItems :: Process -> ItemDb -> Intermediate [Item]
chkSrcItems prc itemDb = 
        case items of
            [] -> itrWait (waitAnySrc prc) []
            _  -> itrOkay items []
        where
            items = prcSrcItems prc itemDb

-- | Get Items in buffer or wait for one to arrive
chkBuffItems :: Process -> ItemDb -> Intermediate [Item]
chkBuffItems prc itemDb = 
        case items of
            [] -> itrWait [Wait Adding  (Processing pid) pid] []
            _  -> itrOkay items []
        where
            pid = prcId prc
            items = itemsAt 1 (Processing $ prcId prc) itemDb


-- | Check wheter target item has enough space

chkSpace:: PrcId -> Item -> ItemDb -> Item -> Intermediate Item
chkSpace prcId itm db dest
        | itmVolume itm < itmSpace dest db = itrOkay itm [] 
        | otherwise = itrWait [(Wait Removing (top dest) prcId)] []
        where
            top i = case (itmPosition i) of
                        (Inside i') -> top (lookupId i' db)
                        _           -> itmPosition i

type Runner = Process -> Instant -> Reader System (Intermediate Move)



runBeltIn :: Runner
runBeltIn prc@(Prc prcId srcs snks (PrpBelt latency)) t = 
        do -- Reader monad
            itemDb    <- asks sysItems
            processDb <- asks sysProcesses
            myBuffer  <- return $ itmBuffer (Processing prcId) itemDb
            return $ do -- Intermediate monad
                        srcItems <- chkSrcItems prc itemDb
                        chkSpace prcId (head srcItems) itemDb myBuffer
                        return $ Mov t (head srcItems) (Inside $ itmId myBuffer)

runBeltOut :: Runner
runBeltOut prc@(Prc prcId srcs snks (PrpBelt latency)) t = 
        do -- Reader monad
            itemDb    <- asks sysItems
            processDb <- asks sysProcesses
            myBuffer  <- return $ itmBuffer (Processing prcId) itemDb
            myOut     <- return $ itmBuffer (At $ prtLoc $ head snks) itemDb
            return $ do -- Intermediate monad
                -- xxx check latency
                        srcItems <- chkBuffItems prc itemDb
                        chkSpace prcId (head srcItems) itemDb myOut
                        return $ Mov t (head srcItems) (Inside $ itmId myBuffer)

-}


-- ** Misc

safeHead :: [a] -> Maybe a -- xxx needed
safeHead xs = case xs of
                  [] -> Nothing 
                  (x:xs) -> Just x

-- | Randomly shuffle a List xxx - needed?

-- | A lookup which raises an exception when key was not found instead
-- of returning Nothing.
lookupId  id map = fromJust $ M.lookup id map
