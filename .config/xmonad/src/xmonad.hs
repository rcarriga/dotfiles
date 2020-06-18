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
import System.Environment
import XMonad.Layout.MouseResizableTile
import XMonad.Hooks.EwmhDesktops
import Data.Ratio ((%))

startScript :: String -> X ()
startScript script_name = spawn $ "zsh $HOME/.config/scripts/" ++ script_name

main :: IO ()
main = do
    mode <- lookupEnv "XMONAD_MODE"
    let gameMode = case mode of
            Just "game" -> True
            _           -> False
        workspaceNameFile = if gameMode then "/tmp/xmonadGameMode" else "/tmp/xmonad"
    safeSpawn "mkfifo" [workspaceNameFile]
    xmonad
        $                 ewmh
        $                 ewmhFullscreen
        $                 docks def
                              { terminal           = "kitty"
                              , modMask            = mod4Mask
                              , workspaces         = myWorkspaces
                              , normalBorderColor  = "#3E3D32"
                              , focusedBorderColor = "#bdbdbd"
                              , handleEventHook    = def handleEventHook
                              , layoutHook         = myLayoutHook
                              , startupHook        = myStartupHook gameMode
                              , logHook            = sendWorkspaceNames workspaceNameFile
                              , borderWidth        = 2
                              }
        `additionalKeysP` myKeys gameMode

myScratchpads :: [NamedScratchpad]
myScratchpads =
    [ NS "bashtop"         "kitty bashtop"   (title =? "bashtop")             defaultFloating
    , NS "Blueman-manager" "blueman-manager" (className =? "Blueman-manager") defaultFloating
    ]

myWorkspaces :: [String]
myWorkspaces = map show ([1 .. 9] :: [Int]) ++ ["NSP"]

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
    io $ appendFile file $ outTags ++ "  \n"

newtype NoFullscreenBorders = NoFullscreenBorders Ambiguity deriving (Read, Show)

instance SetsAmbiguous NoFullscreenBorders where
    hiddens (NoFullscreenBorders amb) ws parentRect maybeStack wrs =
        (fst <$> fullFloats) ++ hiddens amb ws parentRect maybeStack wrs
      where
        floats     = M.toList $ W.floating ws
        fullRect   = W.RationalRect (0 % 1) (0 % 1) (1 % 1) (1 % 1)
        fullFloats = filter (\(_, r) -> r == fullRect) floats

myBordersMod = lessBorders (NoFullscreenBorders Never)
myLayoutHook =
    myBordersMod
        $   avoidStruts
        $   spacingRaw False (Border 0 10 10 10) True (Border 10 10 10 10) True mouseResizableTile --(Tall 1 (3 / 100) (1 / 2)))
        ||| noBorders Full

myStartupHook :: Bool -> X ()
myStartupHook gamingMode = do
    setWMName "XMonad"
    setWallpaper
    startCompositor False
    mapM_
        spawn
        (if gamingMode
            then
                [ "pkill polybar; sleep 1; polybar xmonadGameMode"
                , "pgrep nm-applet || nm-applet"
                , "pgrep blueman-applet || blueman-applet"
                , "light -N 1"
                ]
            else
                [ "pkill polybar; sleep 1; polybar xmonad"
                , "/usr/lib/notification-daemon-1.0/notification-daemon"
                , "pgrep redshift-gtk || redshift-gtk -l 53:-6 -t 6500:2500"
                , "pgrep nm-applet || nm-applet"
                , "(pgrep kdeconnectd || /usr/lib/kdeconnectd) && pkill kdeconnect-indic && kdeconnect-indicator"
                , "pgrep blueman-applet || blueman-applet"
                , "pgrep xautolock || xautolock -locker \"sh /home/ronan/.config/scripts/lock; systemctl suspend\" -detectsleep -time 30 -notify 30 -notifier \"notify-send -u critical -t 10000 -- 'Suspending in 30 seconds'\""
                , "light -N 1"
                ]
        )

setWallpaper :: X ()
setWallpaper = spawn "feh -z --bg-fill ~/.config/images/"

compositorCommand :: String
compositorCommand = "picom --experimental-backends"

startCompositor :: Bool -> X ()
startCompositor force =
    spawn $ if force then "pkill picom; " <> compositorCommand else "pgrep picom || " <> compositorCommand

myKeys :: Bool -> [(String, X ())]
myKeys gameMode =
    [ ("<XF86MonBrightnessUp>"  , startScript "brightness UP")
    , ("<XF86MonBrightnessDown>", startScript "brightness DOWN")
    , ("<XF86AudioRaiseVolume>" , startScript "volume UP")
    , ("<XF86AudioLowerVolume>" , startScript "volume DOWN")
    , ("<XF86AudioMute>"        , startScript "volume MUTE")
    , ("C-S-<Space>"            , spawn "rofi -show drun")
    , ("M-b"                    , namedScratchpadAction myScratchpads "Blueman-manager")
    , ("M-<Tab>"                , toggleRecentNonEmptyWS)
    , ( "M-S-t"
      , spawn $ "pkill polybar || (sleep 1 && polybar " ++ (if gameMode then "xmonadGameMode" else "xmonad") ++ ")"
      )
    , ("M-S-p"  , spawn "polybar-msg cmd toggle")
    , ("M-S-n"  , namedScratchpadAction myScratchpads "bashtop")
    , ("M-C-j"  , sendMessage ShrinkSlave)
    , ("M-C-k"  , sendMessage ExpandSlave)
    , ("M-C-S-j", decWindowSpacing 10)
    , ("M-C-S-k", incWindowSpacing 10)
    , ("M-g"    , toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled)
    , ("M-f"    , spawn "firefox")
    , ("M-S-r"  , withFocused $ \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    , ("M-s"    , startScript "screen EDPI" >> startCompositor True >> setWallpaper)
    , ("M-S-s"  , startScript "screen HDMI" >> startCompositor True >> setWallpaper)
    , ("M-C-s"  , startScript "screen HDMIRIGHT" >> startCompositor True >> setWallpaper)
    , ("M-i"    , startScript "lock")
    , ("M-S-b"  , withFocused toggleBorder)
    , ("M-u"    , setWallpaper)
    ]

-- ######################################################################################
-- This section is used to convert a workspace ID to a string containing an icon for the focused window in that workspace
-- Can be used for XMobar (Or any status bar desired)

type WindowClass = String
type WorkspaceIcons = M.Map WorkspaceId [String]

myWindowIcons :: M.Map String String
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
    , ("Transmission-gtk"       , "\xf019")
    , ("Zotero"                 , "\xf02d")
    , ("Signal"                 , "\xfceb")
    , ("Thunderbird"            , "\xf6ed")
    , ("minecraft-launcher"     , "\xf872")
    , ("discord"                , "\xfb6e")
    , ("Postman"                , "\xf1d8")
    ]

parseWorkspaceId :: WorkspaceIcons -> WorkspaceId -> WorkspaceId -> Maybe String
parseWorkspaceId icons cur i =
    let joinIcons is = foldr (\a b -> a <> "   " <> b) (head is) (tail is)
    in
        case M.lookup i icons of
            Nothing | i == cur      -> Just $ "  " <> i <> "  "
            Nothing                 -> Nothing
            Just _ | i == "NSP"     -> Nothing
            Just wsIcons | i == cur -> Just $ polybarColour "#FFFFFF" $ i ++ "  " ++ joinIcons wsIcons
            Just wsIcons ->
                Just $ polybarColour "#777777" $ "%{A:xdotool key Super+" ++ i ++ ":}" ++ i ++ "  " ++ joinIcons wsIcons ++ "%{A}"

polybarColour :: String -> String -> String
polybarColour colour str = "%{F" <> colour <> "}" <> str <> "%{F-}"

storeIcon :: W.Workspace WorkspaceId l Window -> WorkspaceIcons -> X WorkspaceIcons
storeIcon ws aliases = do
    dis <- display <$> ask
    case W.stack ws of
        Nothing -> return aliases
        Just a  -> do
            wsWindows <- io $ mapM (fmap resClass . getClassHint dis) (W.up a <>(W.focus a : W.down a)) -- Get window classes for workspace
            let wsIcons = map (\win -> fromMaybe "\xf2d0" (M.lookup win myWindowIcons)) wsWindows        -- Get icons for all windows
            return $ M.insert (W.tag ws) wsIcons aliases                                                 -- Store icons in Map



-- ######################################################################################

toggleBorder :: Window -> X ()
toggleBorder w = do
    bw <- asks (borderWidth . config)
    withDisplay $ \d -> io $ do
        cw <- wa_border_width `fmap` getWindowAttributes d w
        if cw == 0 then setWindowBorderWidth d w bw else setWindowBorderWidth d w 0


