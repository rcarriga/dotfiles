{-# LANGUAGE FlexibleContexts #-}
import           XMonad
import           XMonad.Hooks.SetWMName
import           XMonad.Actions.CycleRecentWS
import           Data.Monoid
import           System.Exit
import           XMonad.Hooks.DynamicLog
import qualified Data.Map                      as M
import           XMonad.Util.EZConfig
import           XMonad.Hooks.ManageDocks
import           XMonad.Actions.WorkspaceNames
import           XMonad.Util.Run
import           XMonad.Layout.Tabbed
import           XMonad.Layout.Grid
import           XMonad.Layout.NoBorders
import           XMonad.Util.SpawnOnce
import           XMonad.Layout.Accordion
import           XMonad.Layout.AutoMaster
import           XMonad.Util.NamedScratchpad

myScratchpads = [NS "htop" "kitty htop" (title =? "htop") defaultFloating]
myTerminal = "kitty"
myWorkspaces = map show [1 .. 9]
myNormalBorderColor = "#333333"
myFocusedBorderColor = "#888888"
myModMask = mod4Mask

startScript :: String -> X ()
startScript script_name = spawn $ "bash $HOME/.config/scripts/" ++ script_name

main :: IO ()
main = spawnPipe "xmobar" >>= xmonad . docks . myConfig

myConfig pipe =
    def { terminal           = myTerminal
        , modMask            = myModMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , manageHook         = manageDocks <+> def manageHook
        , layoutHook         = myLayoutHook
        , handleEventHook    = docksEventHook <+> def handleEventHook
        , startupHook        = myStartupHook
        , logHook            = myLogHook pipe
        , borderWidth        = 1
        }
        `additionalKeysP` myKeys

myLogHook pipe = dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn pipe }

myLayoutHook =
    avoidStruts
        $   Tall 1 (3 / 100) (1 / 2)
        ||| noBorders Full
        ||| Accordion
        ||| autoMaster 1 (1 / 100) Grid

myStartupHook :: X ()
myStartupHook = do
    setWMName "XMonad"
    mapM_
        spawn
        [ "pkill trayer; trayer --expand true --transparent true --margin 5 --iconspacing 5 --edge top --align right --width 2 --height 19 --tint 0x000000 --SetPartialStrut true --padding 5"
        , "pgrep redshift || redshift -l 53:-6 -t 6500:2500"
        , "pgrep nm-applet || nm-applet"
        , "pgrep blueman-applet || blueman-applet"
        , "pgrep compton || compton"
        , "pgrep xautolock || xautolock -locker \"systemctl suspend\" -detectsleep -time 30 -notify 30 -notifier \"notify-send -u critical -t 10000 -- 'Suspending in 30 seconds'\""
        , "xinput set-prop \"DLL07BE:01 06CB:7A13 Touchpad\" \"libinput Tapping Enabled\" 1"
        , "xinput set-prop \"DLL07BE:01 06CB:7A13 Touchpad\" \"libinput Natural Scrolling Enabled\" 1"
        , "xset r rate 150 40"
        , "xsetroot -cursor_name left_ptr"
        , "light -N 1"
        , "feh --bg-fill ~/.config/images/moon.jpg"
        ]

myKeys :: [(String, X ())]
myKeys =
    [ ("<XF86MonBrightnessUp>"  , startScript "brightness UP")
    , ("<XF86MonBrightnessDown>", startScript "brightness DOWN")
    , ("<XF86AudioRaiseVolume>" , startScript "volume UP")
    , ("<XF86AudioLowerVolume>" , startScript "volume DOWN")
    , ("<XF86AudioMute>"        , startScript "volume MUTE")
    , ("M-p"                    , spawn "rofi -show run -opacity \"85\" ")
    , ("M-<Tab>", cycleRecentWS [xK_Super_L] xK_Tab xK_BackSpace)
    , ("M-t"                    , sendMessage ToggleStruts)
    , ("M-h"                    , namedScratchpadAction myScratchpads "htop")
    ]

