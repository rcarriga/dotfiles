{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.List (elemIndex, sortOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Graphics.X11.Xlib.Extras
  ( ClassHint (resClass),
    WindowAttributes (wa_border_width),
    getClassHint,
    getWindowAttributes,
  )
import System.Environment (lookupEnv)
import XMonad
import XMonad.Actions.CycleRecentWS (toggleRecentNonEmptyWS)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (AvoidStruts, avoidStruts, docks)
import XMonad.Hooks.ManageHelpers ()
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.LayoutModifier (ModifiedLayout ())
import XMonad.Layout.MouseResizableTile
  ( MRTMessage (ExpandSlave, ShrinkSlave),
    MouseResizableTile,
    mouseResizableTile,
  )
import XMonad.Layout.NoBorders
  ( Ambiguity (Never),
    ConfigurableBorder,
    SetsAmbiguous (..),
    WithBorder,
    lessBorders,
    noBorders,
  )
import XMonad.Layout.Spacing
  ( Border (Border),
    Spacing,
    decWindowSpacing,
    incWindowSpacing,
    spacingRaw,
    toggleScreenSpacingEnabled,
    toggleWindowSpacingEnabled,
  )
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS),
    defaultFloating,
    namedScratchpadAction,
  )
import XMonad.Util.Run (safeSpawn)

startScript :: String -> X ()
startScript script_name = spawn $ "zsh $HOME/.config/scripts/" ++ script_name

main :: IO ()
main = do
  mode <- fromMaybe "xmonad" <$> lookupEnv "XMONAD_MODE"
  let workspaceNameFile = "/tmp/" <> mode
  safeSpawn "mkfifo" [workspaceNameFile]
  xmonad $
    ewmh $
      ewmhFullscreen $
        docks
          def
            { terminal = "kitty",
              modMask = mod4Mask,
              workspaces = myWorkspaces,
              normalBorderColor = "#3E3D32",
              focusedBorderColor = "#bdbdbd",
              handleEventHook = def handleEventHook,
              layoutHook = myLayoutHook,
              startupHook = myStartupHook mode,
              logHook = sendWorkspaceNames workspaceNameFile,
              borderWidth = 2
            }
          `additionalKeysP` myKeys mode

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "bashtop" "kitty bashtop" (title =? "bashtop") defaultFloating,
    NS "Blueman-manager" "blueman-manager" (className =? "Blueman-manager") defaultFloating
  ]

myWorkspaces :: [String]
myWorkspaces = map show ([1 .. 9] :: [Int]) ++ ["NSP"]

myStartupHook :: String -> X ()
myStartupHook mode = do
  setWMName "XMonad"
  setWallpaper
  startCompositor False
  mapM_
    spawn
    [ "pkill polybar; sleep 1; polybar " <> mode,
      "copyq",
      "pkill flameshot; flameshot",
      "/usr/lib/notification-daemon-1.0/notification-daemon",
      "pgrep redshift-gtk || redshift-gtk -l 53:-6 -t 6500:2500",
      "pgrep nm-applet || nm-applet",
      "(pgrep kdeconnectd || /usr/lib/kdeconnectd) && pkill kdeconnect-indic && kdeconnect-indicator",
      "pgrep blueman-applet || blueman-applet",
      "light -N 1"
    ]

setWallpaper :: X ()
setWallpaper = spawn "feh -z --bg-fill ~/.config/images/"

compositorCommand :: String
compositorCommand = "picom --experimental-backends"

startCompositor :: Bool -> X ()
startCompositor force =
  spawn $ if force then "pkill picom; " <> compositorCommand else "pgrep picom || " <> compositorCommand

myKeys :: String -> [(String, X ())]
myKeys mode =
  [ ("<XF86MonBrightnessUp>", startScript "brightness UP"),
    ("<XF86MonBrightnessDown>", startScript "brightness DOWN"),
    ("<XF86AudioRaiseVolume>", startScript "volume UP"),
    ("<XF86AudioLowerVolume>", startScript "volume DOWN"),
    ("<XF86AudioMute>", startScript "volume MUTE"),
    ("C-S-<Space>", spawn "rofi -show drun"),
    ("M-b", namedScratchpadAction myScratchpads "Blueman-manager"),
    ("M-<Tab>", toggleRecentNonEmptyWS),
    ( "M-S-t",
      spawn $ "pkill polybar || (sleep 1 && polybar " <> mode <> ")"
    ),
    ("M-S-p", spawn "polybar-msg cmd toggle"),
    ("M-S-n", namedScratchpadAction myScratchpads "bashtop"),
    ("M-C-j", sendMessage ShrinkSlave),
    ("M-C-k", sendMessage ExpandSlave),
    ("M-C-S-j", decWindowSpacing 10),
    ("M-C-S-k", incWindowSpacing 10),
    ("M-g", toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled),
    ("M-f", spawn "firefox"),
    ("M-d", spawn "GDK_DPI_SCALE=0.5 GDK_SCALE=2 dbeaver"),
    ("M-S-r", withFocused $ \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster),
    ("M-s", startScript "screen EDPI" >> startCompositor True >> setWallpaper),
    ("M-S-s", startScript "screen HDMI" >> startCompositor True >> setWallpaper),
    ("M-C-s", startScript "screen HDMIRIGHT" >> startCompositor True >> setWallpaper),
    ("M-i", startScript "lock"),
    ("M-S-b", withFocused toggleBorder),
    ("M-u", setWallpaper),
    ("M-p", spawn "xset r rate 150 40 && setxkbmap -layout gb"),
    ("M-v", startScript "session"),
    ("M-m", spawn "kitty -e nvim -c 'startinsert' -c 'set buftype=nofile' -c 'autocmd VimLeave * norm \"+zyie'")
  ]

-- ######################################################################################
-- Creates and stores a string in FIFO for displaying in polybar
-- Focused workspace and windows are highlighted
-- Can be used for XMobar (Or any status bar desired)

myWindowIcons :: M.Map String String
myWindowIcons =
  M.fromList
    [ ("kitty", "\xf120"),
      ("firefox", "\xf269"),
      ("Blueman-manager", "\xf294"),
      ("libreoffice-startcenter", "\xf15c"),
      ("libreoffice-draw", "\xf15c"),
      ("Steam", "\xf1b6"),
      ("Zathura", "\xf1c1"),
      ("okular", "\xf1c1"),
      ("Spotify", "\xf1bc"),
      ("Inkscape", "\xf6fc"),
      ("Kodi", "\xf03d"),
      ("Transmission-gtk", "\xf019"),
      ("Zotero", "\xf02d"),
      ("Signal", "\xfceb"),
      ("Thunderbird", "\xf6ed"),
      ("minecraft-launcher", "\xf872"),
      ("discord", "\xfb6e"),
      ("Postman", "\xf1d8"),
      ("Slack", "\xf9b0"),
      ("Keybase", "\xf084"),
      ("vlc", "\xfa7b")
    ]

highlight :: String -> String
highlight = fg "#FFFFFF"

normal :: String -> String
normal = fg "#777777"

fg :: String -> String -> String
fg colour str = "%{F" <> colour <> "}" <> str <> "%{F-}"

bg :: String -> String -> String
bg colour str = "%{B" <> colour <> "}" <> str <> "%{B-}"

clickable :: String -> String -> String
clickable winId str =
  "%{A:xdotool key Super+"
    <> winId
    <> ":}"
    <> str
    <> "%{A}"

sendWorkspaceNames :: String -> X ()
sendWorkspaceNames file = do
  workspacesString <- joinWithSpaces 1 <$> prettyWorkspaceList
  io $ appendFile file $ workspacesString ++ "  \n"

prettyWorkspaceList :: X [String]
prettyWorkspaceList = do
  curWorkspaces <- sortOn W.tag . filter (\wspace -> W.tag wspace /= "NSP") . W.workspaces . windowset <$> get
  workspaceIcons <- filter (\(_, icons) -> not $ null icons) . zip (map W.tag curWorkspaces) <$> mapM prettyWindowIconList curWorkspaces
  focusedWspace <- W.tag . W.workspace . W.current . windowset <$> get
  let colour tag = (if tag == focusedWspace then highlight else normal)
  return $ map (\(tag, winIcons) -> colour tag $ clickable tag $ joinWithSpaces 4 (tag : winIcons)) workspaceIcons

joinWithSpaces :: Int -> [String] -> String
joinWithSpaces spaces = joinStrings $ replicate spaces ' '

joinStrings :: String -> [String] -> String
joinStrings joinWith = foldr (\a b -> a <> joinWith <> b) ""

prettyWindowIconList :: W.Workspace WorkspaceId l Window -> X [String]
prettyWindowIconList workspace = case W.stack workspace of
  Nothing -> return []
  Just curStack -> do
    let curWindows = W.integrate curStack
    winIcons <- windowIcons curWindows
    let focusedIndex = fromMaybe (-1) $ elemIndex (W.focus curStack) curWindows
    isWorkspaceFocused <- (==) (W.tag workspace) . W.tag . W.workspace . W.current . windowset <$> get
    return $ zipWith (\icon i -> if i == focusedIndex && isWorkspaceFocused then highlight icon else normal icon) winIcons [0 ..]

windowIcons :: [Window] -> X [String]
windowIcons winIds = do
  dis <- display <$> ask
  windowClasses <- io $ mapM (fmap resClass . getClassHint dis) winIds
  return $ map (\win -> fromMaybe "\xf2d0" (M.lookup win myWindowIcons)) windowClasses

-- ######################################################################################
-- Making XMonad play nice with fullscreen windows and borders.

toggleBorder :: Window -> X ()
toggleBorder w = do
  bw <- asks (borderWidth . config)
  withDisplay $ \d -> io $ do
    cw <- wa_border_width `fmap` getWindowAttributes d w
    if cw == 0 then setWindowBorderWidth d w bw else setWindowBorderWidth d w 0

newtype NoFullscreenBorders = NoFullscreenBorders Ambiguity deriving (Read, Show)

instance SetsAmbiguous NoFullscreenBorders where
  hiddens (NoFullscreenBorders amb) ws parentRect maybeStack wrs =
    (fst <$> fullFloats) ++ hiddens amb ws parentRect maybeStack wrs
    where
      floats = M.toList $ W.floating ws
      fullRect = W.RationalRect (0 % 1) (0 % 1) (1 % 1) (1 % 1)
      fullFloats = filter (\(_, r) -> r == fullRect) floats

myBordersMod :: ModifiedLayout AvoidStruts (Choose (ModifiedLayout Spacing MouseResizableTile) (ModifiedLayout WithBorder Full)) Window -> ModifiedLayout (ConfigurableBorder NoFullscreenBorders) (ModifiedLayout AvoidStruts (Choose (ModifiedLayout Spacing MouseResizableTile) (ModifiedLayout WithBorder Full))) Window
myBordersMod = lessBorders (NoFullscreenBorders Never)

myLayoutHook :: ModifiedLayout (ConfigurableBorder NoFullscreenBorders) (ModifiedLayout AvoidStruts (Choose (ModifiedLayout Spacing MouseResizableTile) (ModifiedLayout WithBorder Full))) Window
myLayoutHook =
  myBordersMod $
    avoidStruts $
      spacingRaw False (Border 0 10 10 10) True (Border 10 10 10 10) True mouseResizableTile --(Tall 1 (3 / 100) (1 / 2)))
        ||| noBorders Full
