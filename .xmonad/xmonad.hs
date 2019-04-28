{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
import XMonad hiding (WindowClass)
import XMonad.Config.Prime (WindowSpace)
import XMonad.Hooks.SetWMName
import XMonad.Actions.CycleRecentWS
import Data.Monoid
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

startScript :: String -> X ()
startScript script_name = spawn $ "bash $HOME/.config/scripts/" ++ script_name

main :: IO ()
main = spawnPipe "xmobar" >>= xmonad . docks . myConfig

myConfig pipe =
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
            , logHook            = myLogHook pipe
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
myWorkspaces = map show [1 .. 9]

myNormalBorderColor :: String
myNormalBorderColor = "#333333"

myFocusedBorderColor :: String
myFocusedBorderColor = "#888888"

myModMask :: KeyMask
myModMask = mod4Mask

myManageHook :: ManageHook
myManageHook = composeAll [manageDocks, def manageHook]

myLogHook :: Handle -> X ()
myLogHook pipe = do
    m <- buildIconMap
    dynamicLogWithPP def
        { ppOutput  = hPutStrLn pipe . pad
        , ppCurrent = xmobarColor "white" "black" . parseWorkspaceId m
        , ppHidden  = xmobarColor "#555555" "black" . parseWorkspaceId m
        , ppSep     = " | "
        , ppWsSep   = xmobarColor "#555555" "black" "  "
        , ppLayout  = const ""
        , ppTitle   = const ""
        }


-- ######################################################################################
-- This section is used to convert a workspace ID to a string containing an icon for the focused window in that workspace
-- Can be used for XMobar (Or any status bar desired)

-- | Given the mappings of workspace IDs to their strings aliases
-- an ID return a string to represent workspace in XMobar.
-- Also returns empty string for NamedScratchpad workspace.
parseWorkspaceId :: WorkspaceAliases -> WorkspaceId -> String
parseWorkspaceId m i = if i == "NSP" then "" else fromMaybe i $ M.lookup i m

type WindowClass = String
type WindowIcons = M.Map WindowClass String

-- | Icons to represent a specified window class
windowIcons :: WindowIcons
windowIcons =
    let wrapIcon icon = " <fn=1>" <> icon <> "</fn>"
    in
        M.fromList $ map
            (liftSnd wrapIcon)
            [ ("kitty"                  , "\xf120")
            , ("Firefox"                , "\xf269")
            , ("Blueman-manager"        , "\xf294")
            , ("libreoffice-startcenter", "\xf15c")
            , ("libreoffice-draw"       , "\xf15c")
            ]

type WorkspaceAliases = M.Map WorkspaceId String

-- | Cycle through workspaces and store workspace aliases
buildIconMap :: X WorkspaceAliases
buildIconMap = foldrM storeAlias M.empty =<< W.workspaces . windowset <$> get

-- | Store the given workspace's name in given and return.
-- This is where to change if the desired format is not <Workspace tag><Window Icon>
storeAlias :: W.Workspace WorkspaceId l Window -> WorkspaceAliases -> X WorkspaceAliases
storeAlias ws m = do
    dis <- display <$> ask
    case W.stack ws of
        Nothing -> return m
        Just a  -> do
            focus <- liftIO $ resClass <$> getClassHint dis (W.focus a)          -- Get focused window in given workspace
            let wsName = W.tag ws <> fromMaybe "" (M.lookup focus windowIcons)   -- Create name to give workspace (i.e. Append icon to tag)
            return $ M.insert (W.tag ws) wsName m                                -- Store name in Map

-- ######################################################################################

myLayoutHook = avoidStruts $ Tall 1 (3 / 100) (1 / 2) ||| noBorders Full

myStartupHook :: X ()
myStartupHook = do
    setWMName "XMonad"
    mapM_
        spawn
        [ "pkill trayer; trayer --expand true --transparent true --margin 5 --iconspacing 5 --edge bottom --align right --widthtype request  --height 20 --tint 0x000000 --SetDockType true --SetPartialStrut true --padding 5"
        , "pkill redshift; sleep 5s && redshift -l 53:-6 -t 6500:2500"
        , "pgrep nm-applet || nm-applet"
        , "pgrep blueman-applet || blueman-applet"
        , "pgrep compton || compton -f -D 3 -i 0.8"
        , "pgrep xautolock || xautolock -locker \"systemctl suspend\" -detectsleep -time 30 -notify 30 -notifier \"notify-send -u critical -t 10000 -- 'Suspending in 30 seconds'\""
        , "xinput set-prop \"DLL07BE:01 06CB:7A13 Touchpad\" \"libinput Tapping Enabled\" 1"
        , "xinput set-prop \"DLL07BE:01 06CB:7A13 Touchpad\" \"libinput Natural Scrolling Enabled\" 1"
        , "xset r rate 150 40"
        , "xsetroot -cursor_name left_ptr"
        , "light -N 1"
        , "feh --bg-fill ~/.config/images/city.jpg"
        , "wal --theme monokai"
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
    , ("M-t"                    , sendMessage ToggleStruts)
    , ("M-n"                    , namedScratchpadAction myScratchpads "htop")
    , ("M-c"                    , confirmPrompt myPromptConfig "close window?" kill)
    , ("M-S-q"                  , confirmPrompt myPromptConfig "exit" $ io exitSuccess)
    ]

myPromptConfig :: XPConfig
myPromptConfig = def { font = "FiraCode-Regular", bgColor = "black", fgColor = "white", height = 25 }
