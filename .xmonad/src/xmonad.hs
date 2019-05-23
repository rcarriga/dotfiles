{-# LANGUAGE FlexibleContexts #-}
import XMonad.Layout.SimpleFloat
import XMonad hiding (WindowClass)
import XMonad.Config.Prime (WindowSpace)
import XMonad.Hooks.SetWMName
import XMonad.Actions.CycleRecentWS
import Data.Monoid
import Control.Exception
import System.Exit
import XMonad.Hooks.DynamicLog
import qualified Data.Map as M
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Util.SpawnOnce
import XMonad.Layout.Accordion
import XMonad.Layout.AutoMaster
import XMonad.Util.NamedScratchpad
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt
import GHC.IO.Handle.Types
import qualified XMonad.StackSet as W
import Graphics.X11.Xlib.Extras (getClassHint, resClass)
import Data.Foldable
import Data.Maybe
import Util
import XMonad.Layout.Spacing
import Data.List
import Data.Ord

startScript :: String -> X ()
startScript script_name = spawn $ "bash $HOME/.config/scripts/" ++ script_name


main :: IO ()
main = do
    safeSpawn "mkfifo" ["/tmp/xmonad"]
    xmonad $ docks myConfig

myConfig =
    def
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
            , borderWidth        = 0
            }
        `additionalKeysP` myKeys

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
myNormalBorderColor = "#333333"

myFocusedBorderColor :: String
myFocusedBorderColor = "#888888"

myModMask :: KeyMask
myModMask = mod4Mask

myManageHook :: ManageHook
myManageHook = composeAll [manageDocks, def manageHook]

myLogHook :: X ()
myLogHook = do
    st               <- get
    workspaceAliases <- foldrM storeAlias M.empty $ W.workspaces $ windowset st
    let curWs   = W.tag $ W.workspace $ W.current $ windowset st
        visWs   = map (W.tag . W.workspace) . W.visible $ windowset st
        hidWs   = map W.tag $ filter (isJust . W.stack) $ W.hidden $ windowset st
        tags    = sortOn (\t -> fromMaybe 99 $ t `elemIndex` myWorkspaces) $ curWs : visWs ++ hidWs
        outTags = (++) "  " $ foldr1 (\a b -> a ++ "  " ++ b) $ filter (/= "") $ map
            (parseWorkspaceId workspaceAliases curWs)
            tags
    io $ appendFile "/tmp/xmonad" $ outTags ++ "\n"

-- ######################################################################################
-- This section is used to convert a workspace ID to a string containing an icon for the focused window in that workspace
-- Can be used for XMobar (Or any status bar desired)

-- | Given the mappings of workspace IDs to their strings aliases
-- an ID return a string to represent workspace in XMobar.
-- Also returns empty string for NamedScratchpad workspace.
parseWorkspaceId :: WorkspaceAliases -> WorkspaceId -> WorkspaceId -> String
parseWorkspaceId m cur i =
    let x = fromMaybe (" " ++ i ++ " ") $ M.lookup i m
    in
        if i == "NSP"
            then ""
            else if i == cur then "%{u#FF0033}%{F#FFFFFF}" ++ x ++ "%{F-}%{-u}" else "%{F#777777}" ++ x ++ "%{F-}"

type WindowClass = String
type WindowIcons = M.Map WindowClass String
type WorkspaceAliases = M.Map WorkspaceId String

myWindowIcons = M.fromList
    [ ("kitty"                  , "\xf120")
    , ("Firefox"                , "\xf269")
    , ("Blueman-manager"        , "\xf294")
    , ("libreoffice-startcenter", "\xf15c")
    , ("libreoffice-draw"       , "\xf15c")
    ]

-- | Store the given workspace's name in given and return.
-- This is where to change if the desired format is not <Workspace tag><Window Icon>
storeAlias :: W.Workspace WorkspaceId l Window -> WorkspaceAliases -> X WorkspaceAliases
storeAlias ws aliases = do
    dis <- display <$> ask
    case W.stack ws of
        Nothing -> return aliases
        Just a  -> do
            focus <- io $ resClass <$> getClassHint dis (W.focus a)          -- Get focused window in given workspace
            let wsName = "  " <> (fromMaybe (W.tag ws) (M.lookup focus myWindowIcons)) <> "  "   -- Create name to give workspace (i.e. Append icon to tag)
            return $ M.insert (W.tag ws) wsName aliases                                -- Store name in Map

-- ######################################################################################

myLayoutHook =
    avoidStruts
        $   spacingRaw False (Border 0 10 10 10) True (Border 10 10 10 10) True
        $   Tall 1 (3 / 100) (1 / 2)
        ||| noBorders Full
        ||| simpleFloat

myStartupHook :: X ()
myStartupHook = do
    setWMName "XMonad"
    mapM_
        spawn
        [ "pkill polybar; polybar xmonad"
        , "pkill redshift; sleep 5s && redshift -l 53:-6 -t 6500:2500"
        , "pgrep nm-applet || nm-applet"
        , "pgrep blueman-applet || blueman-applet"
        , "pgrep compton || compton -f -D 3"
        , "pgrep xautolock || xautolock -locker \"systemctl suspend\" -detectsleep -time 30 -notify 30 -notifier \"notify-send -u critical -t 10000 -- 'Suspending in 30 seconds'\""
        , "xinput set-prop \"DLL07BE:01 06CB:7A13 Touchpad\" \"libinput Tapping Enabled\" 1"
        , "xinput set-prop \"DLL07BE:01 06CB:7A13 Touchpad\" \"libinput Natural Scrolling Enabled\" 1"
        , "xset r rate 150 40"
        , "setxkbmap -layout gb"
        , "xsetroot -cursor_name left_ptr"
        , "light -N 1"
        , "feh --bg-fill ~/.config/images/city.jpg"
        , "wal -s --theme monokai"
        ]

myKeys :: [(String, X ())]
myKeys =
    [ ("<XF86MonBrightnessUp>"  , startScript "brightness UP")
    , ("<XF86MonBrightnessDown>", startScript "brightness DOWN")
    , ("<XF86AudioRaiseVolume>" , startScript "volume UP")
    , ("<XF86AudioLowerVolume>" , startScript "volume DOWN")
    , ("<XF86AudioMute>"        , startScript "volume MUTE")
    , ("M-p"                    , spawn "rofi -show run -opacity \"85\" ")
    , ("M-b"                    , namedScratchpadAction myScratchpads "Blueman-manager")
    , ("M-<Tab>"                , cycleRecentWS [xK_Super_L] xK_Tab xK_BackSpace)
    , ("M-S-t"                    , sendMessage ToggleStruts >> spawn "polybar-msg cmd toggle")
    , ("M-n"                    , namedScratchpadAction myScratchpads "htop")
    -- , ("M-c"                    , confirmPrompt myPromptConfig "close window?" kill)
    , ("M-S-q"                  , confirmPrompt myPromptConfig "exit" $ io exitSuccess)
    , ("M-C-S-j"                , decScreenSpacing 10)
    , ("M-C-S-k"                , incScreenSpacing 10)
    , ("M-C-j"                  , decWindowSpacing 10)
    , ("M-C-k"                  , incWindowSpacing 10)
    , ("M-g"                    , toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled)
    , ("M-f"                    , spawn "firefox")
    ]

myPromptConfig :: XPConfig
myPromptConfig = def { font = "FiraCode-Regular", bgColor = "black", fgColor = "white", height = 25 }
