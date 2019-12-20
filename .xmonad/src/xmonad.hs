{-# LANGUAGE FlexibleContexts #-}
import Data.Foldable
import Data.List
import Data.Maybe
import Graphics.X11.Xlib.Extras
import XMonad hiding (WindowClass)
import XMonad.Actions.CycleRecentWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import qualified Data.Map as M
import qualified XMonad.StackSet as W

startScript :: String -> X ()
startScript script_name = spawn $ "bash $HOME/.config/scripts/" ++ script_name

main :: IO ()
main = do
    safeSpawn "mkfifo" ["/tmp/xmonad"]
    xmonad
        $                 docks def
                              { terminal           = myTerminal
                              , modMask            = myModMask
                              , workspaces         = myWorkspaces
                              , normalBorderColor  = myNormalBorderColor
                              , focusedBorderColor = myFocusedBorderColor
                              , manageHook         = myManageHook
                              , layoutHook         = myLayoutHook
                              , handleEventHook    = docksEventHook <+> def handleEventHook
                              , startupHook        = myStartupHook
                              , logHook            = myLogHook
                              , borderWidth        = myBorderWidth
                              }
        `additionalKeysP` myKeys

myBorderWidth :: Dimension
myBorderWidth = 2

myScratchpads :: [NamedScratchpad]
myScratchpads =
    [ NS "htop"            "kitty htop"      (title =? "htop")                defaultFloating
    , NS "Blueman-manager" "blueman-manager" (className =? "Blueman-manager") defaultFloating
    ]

myTerminal :: String
myTerminal = "kitty"

myWorkspaces :: [String]
myWorkspaces = map show [1 .. 9] ++ ["NSP"]

myNormalBorderColor :: String
myNormalBorderColor = "#3E3D32"

myFocusedBorderColor :: String
myFocusedBorderColor = "#bdbdbd"

myModMask :: KeyMask
myModMask = mod4Mask

myManageHook :: ManageHook
myManageHook = composeAll [manageDocks, def manageHook]

myLogHook :: X ()
myLogHook = do
    sendWorkspaceNames "/tmp/xmonad"

sendWorkspaceNames :: String -> X ()
sendWorkspaceNames file = do
    st             <- get
    workspaceNames <- workspaces . config <$> ask
    workspaceIcons <- foldrM storeIcon M.empty $ W.workspaces $ windowset st
    let curWs   = W.tag $ W.workspace $ W.current $ windowset st
        tags    = sortOn (\t -> fromMaybe 99 $ t `elemIndex` myWorkspaces) workspaceNames
        outTags = foldr1 (\a b -> a <> "      " <> b) $ filter (/= "") $ mapMaybe
            (parseWorkspaceId workspaceIcons curWs)
            tags
    io $ appendFile file $ outTags ++ "\n"

myLayoutHook =
    avoidStruts
        $ spacingRaw False (Border 0 10 10 10) True (Border 10 10 10 10) True (smartBorders (Tall 1 (3 / 100) (1 / 2)))
        ||| noBorders Full

myStartupHook :: X ()
myStartupHook = do
    setWMName "XMonad"
    setWallpaper
    mapM_
        spawn
        [ "pkill polybar; polybar xmonad"
        , "pkill redshift; sleep 5s && redshift -l 53:-6 -t 6500:2500"
        , "pgrep nm-applet || nm-applet"
        , "pgrep blueman-applet || blueman-applet"
        , "pgrep picom || picom -f -D 3 --experimental-backends --backend glx"
        , "pgrep xautolock || xautolock -locker \"sh /home/ronan/.config/scripts/lock; systemctl suspend\" -detectsleep -time 30 -notify 30 -notifier \"notify-send -u critical -t 10000 -- 'Suspending in 30 seconds'\""
        , "light -N 1"
        ]
    startScript "xsettings"

setWallpaper :: X ()
setWallpaper = spawn "feh -z --bg-fill ~/.config/images"

myKeys :: [(String, X ())]
myKeys =
    [ ("<XF86MonBrightnessUp>"  , startScript "brightness UP")
    , ("<XF86MonBrightnessDown>", startScript "brightness DOWN")
    , ("<XF86AudioRaiseVolume>" , startScript "volume UP")
    , ("<XF86AudioLowerVolume>" , startScript "volume DOWN")
    , ("<XF86AudioMute>"        , startScript "volume MUTE")
    , ("M-p"                    , spawn "rofi -z -show run -opacity \"86\" ")
    , ("M-b"                    , namedScratchpadAction myScratchpads "Blueman-manager")
    , ("M-<Tab>"                , cycleRecentWS [xK_Super_L] xK_Tab xK_BackSpace)
    , ("M-S-t"                  , sendMessage ToggleStruts >> spawn "polybar-msg cmd toggle")
    , ("M-S-p"                  , spawn "polybar-msg cmd toggle")
    , ("M-S-n"                  , namedScratchpadAction myScratchpads "htop")
    , ("M-C-S-j"                , decScreenSpacing 10)
    , ("M-C-S-k"                , incScreenSpacing 10)
    , ("M-C-j"                  , decWindowSpacing 10)
    , ("M-C-k"                  , incWindowSpacing 10)
    , ("M-g"                    , toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled)
    , ("M-f"                    , spawn "firefox")
    , ("M-S-r"                  , withFocused $ \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    , ("M-s"                    , startScript "screen EDPI" >> setWallpaper)
    , ("M-S-s"                  , startScript "screen HDMI" >> setWallpaper)
    , ("M-C-s"                  , startScript "screen HDMIABOVE" >> setWallpaper)
    , ("M-i"                    , startScript "lock")
    , ("M-S-b"                  , withFocused toggleBorder)
    , ("M-n"                    , spawn "kill -s USR1 $(pidof deadd-notification-center)")
    ]

-- ######################################################################################
-- This section is used to convert a workspace ID to a string containing an icon for the focused window in that workspace
-- Can be used for XMobar (Or any status bar desired)

parseWorkspaceId :: WorkspaceIcons -> WorkspaceId -> WorkspaceId -> Maybe String
parseWorkspaceId icons cur i = case M.lookup i icons of
    Nothing | i == cur   -> Just $ "  " <> i <> "  "
    Nothing              -> Nothing
    Just _ | i == "NSP"  -> Nothing
    Just icon | i == cur -> Just $ "%{F#FFFFFF}" ++ i ++ "  " ++ icon ++ "%{F-}"
    Just icon            -> Just $ "%{F#777777}" ++ i ++ "  " ++ icon ++ "%{F-}"

type WindowClass = String
type WorkspaceIcons = M.Map WorkspaceId String

myWindowIcons = M.fromList
    [ ("kitty"                  , "\xf120")
    , ("firefox"                , "\xf269")
    , ("Blueman-manager"        , "\xf294")
    , ("libreoffice-startcenter", "\xf15c")
    , ("libreoffice-draw"       , "\xf15c")
    , ("Steam"                  , "\xf1b6")
    , ("Zathura"                , "\xf1c1")
    , ("okular"                 , "\xf1c1")
    , ("Spotify"                , "\xf1bc")
    , ("Inkscape"               , "\xf6fc")
    , ("Kodi"                   , "\xf03d")
    , ("transmission"           , "\xf019")
    , ("Zotero"                 , "\xf02d")
    ]

-- | Store the given workspace's name in given and return.
-- This is where to change if the desired format is not <Workspace tag><Window Icon>
storeIcon :: W.Workspace WorkspaceId l Window -> WorkspaceIcons -> X WorkspaceIcons
storeIcon ws aliases = do
    dis <- display <$> ask
    case W.stack ws of
        Nothing -> return aliases
        Just a  -> do
            focused <- io $ resClass <$> getClassHint dis (W.focus a)        -- Get focused window in given workspace
            let wsName = fromMaybe "\xf2d0" (M.lookup focused myWindowIcons) -- Create name to give workspace (i.e. Append icon to tag)
            return $ M.insert (W.tag ws) wsName aliases                    -- Store name in Map

-- ######################################################################################

toggleBorder :: Window -> X ()
toggleBorder w = do
    bw <- asks (borderWidth . config)
    withDisplay $ \d -> io $ do
        cw <- wa_border_width `fmap` getWindowAttributes d w
        if cw == 0 then setWindowBorderWidth d w bw else setWindowBorderWidth d w 0

