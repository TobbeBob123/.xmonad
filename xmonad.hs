import XMonad hiding ( (|||) )
import XMonad.Core
import qualified XMonad.StackSet as W

import Graphics.X11.ExtraTypes.XF86

import qualified Data.Map as M
import Data.Monoid

import System.IO
import System.Exit
import System.Directory

import XMonad.Actions.NoBorders
import XMonad.Actions.SpawnOn
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Navigation2D
import XMonad.Actions.Promote
import XMonad.Actions.WithAll 
import XMonad.Actions.CycleWS

import XMonad.Util.EZConfig
import XMonad.Util.WorkspaceCompare
import XMonad.Util.SpawnOnce
import XMonad.Util.Run

import XMonad.Hooks.ToggleHook
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks

import XMonad.Layout hiding ( (|||) ) 
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Renamed
import XMonad.Layout.Spiral
import XMonad.Layout.Grid
import XMonad.Layout.SimplestFloat
import XMonad.Layout.SimpleFloat
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiColumns
import XMonad.Layout.Gaps
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import qualified XMonad.Layout.ToggleLayouts as T

myTerminal = "kitty"
myLauncher = "rofi -show drun"

myFocusFollowsMouse  :: Bool
myFocusFollowsMouse  = True

myBorderWidth = 1
myFocusColor = "#ff79c6"
myNormColor   = "#282a36"

myModMask = mod4Mask

--- WS ---
xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x    = [x]

myWorkspaces = clickable . (map xmobarEscape) $  ["1 Term",
                                                  "2 Sos",
                                                  "3 FilS",
                                                  "4 Nett",
                                                  "5 Jobb",
                                                  "6 Mail",
                                                  "7 Spill",
                                                  "8",
                                                  "9"]
    where                                                                       
              clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                             (i,ws) <- zip [1..9] l,                                        
                            let n = i ]

myManageHook = composeAll
    [ className =? "confirm"         --> doFloat
    , className =? "file_progress"   --> doFloat
    , className =? "dialog"          --> doFloat
    , className =? "download"        --> doFloat
    , className =? "error"           --> doFloat
    , className =? "Nm-connection-editor" --> doFloat
    , className =? "Gtk2_prefs" --> doFloat
    , className =? "Steam" --> doFloat
    , className =? "lunarclient" --> doFloat
    , className =? "Yad" --> doCenterFloat
    , className =? "fim" --> doCenterFloat
    , className =? "Pavucontrol" --> doCenterFloat
    , className =? "CoreImage" --> doCenterFloat
    , className =? "Bitwarden" --> doCenterFloat
    , className =? "stacer" --> doCenterFloat
    , className =? "kitty" --> doShift (myWorkspaces !! 0)
    , className =? "Emacs" --> doShift (myWorkspaces !! 0)
    , className =? "Signal" --> doShift (myWorkspaces !! 1)
    , className =? "discord" --> doShift (myWorkspaces !! 1)
    , className =? "Pcmanfm" --> doShift (myWorkspaces !! 2)
    , className =? "librewolf" --> doShift (myWorkspaces !! 3)  
    , className =? "Surf" --> doShift (myWorkspaces !! 3)
    , className =? "qutebrowser" --> doShift (myWorkspaces !! 3)
    , className =? "tabbed" --> doShift (myWorkspaces !! 3)
    , className =? "Badwolf" --> doShift (myWorkspaces !! 3)
    , className =? "teams-for-linux" --> doShift (myWorkspaces !! 4)
    , title     =? "LibreOffice" --> doShift (myWorkspaces !! 4)
    , className =? "Soffice" --> doShift (myWorkspaces !! 4)
    , className =? "code-oss" --> doShift (myWorkspaces !! 4)
    , className =? "Thunderbird" --> doShift (myWorkspaces !! 5)
    , className =? "Geary" --> doShift (myWorkspaces !! 5)
    , className =? "Steam" --> doShift (myWorkspaces !! 6)
    , className =? "lunarclient" --> doShift (myWorkspaces !! 6)
    , className =? "GeForce NOW" --> doShift (myWorkspaces !! 6)
    ]

myLayouts = avoidStruts $
            gaps [(U,0), (R,0), (L,0), (D,0)] (
            layoutTall 
        ||| layoutSpiral 
        ||| layoutGrid 
        ||| layoutMirror 
        ||| layoutFloat
        ||| layoutFull
        ||| layoutTreeColumns
        ||| layoutMultiColumns)

    where
      layoutTall =
                 renamed [Replace "Tall"]
                 $ Tall 1 (3/100) (1/2)
      layoutSpiral = 
                 renamed [Replace "Sprial"]
                 $ spiral (6/7)
      layoutGrid =
                 renamed [Replace "Grid"]
                 $ Grid
      layoutMirror =
                 renamed [Replace "Mirror"]
                 $ Mirror (Tall 1 (3/100) (3/5))
      layoutFloat =
                 renamed [Replace "Float"]
                 $ smartBorders
                 $ limitWindows 20 simplestFloat
      layoutFull =
                 renamed [Replace "Full"]
                 $ smartBorders
                 $ limitWindows 20 simpleFloat
      layoutTreeColumns =
                 renamed [Replace "Treecolumns"]
                 $ ThreeCol 1 (3/100) (1/2)
      layoutMultiColumns =
                 renamed [Replace "Multicolumns"]
                 $ multiCol [1] 1 0.01 (-0.5)

myKeys conf@(XConfig {XMonad.modMask = mod}) = M.fromList $
-- Start_keys
-- Tips: <mod> = Win key/Super
      -- Start Terminal
      [ ((mod, xK_Return), spawn myTerminal)
      -- Se Xmonad config
      , ((mod .|. shiftMask, xK_Return), spawn "emacsclient -c -a 'emacs' ~/.xmonad/README.org")
      -- Vis Hotkeys
      , ((mod, xK_s), spawn "~/.xmonad/keys.sh")
      -- Vis alias for fish
      , ((mod .|. shiftMask, xK_s), spawn "~/.config/fish/alias.sh")
      -- Start Program Launcher
      , ((mod, xK_d), spawn myLauncher)
      -- Start Nett
      , ((mod .|. shiftMask, xK_Tab), spawn "librewolf")
      -- Start Emacs
      , ((mod, xK_e), spawn "emacsclient -c -a 'emacs'")
      -- Start FilManager
      , ((mod .|. shiftMask, xK_f), spawn "pcmanfm")
      -- lås PC
      , ((mod, xK_l), spawn "light-locker-command -l")
      -- Lyd instillinger
      , ((mod .|. shiftMask, xK_l), spawn "pavucontrol")
      -- Ta skjermbilde
      , ((mod, xK_p), spawn "~/Script/SkjermBilde.sh")
      -- Nett instillinger
      , ((mod .|. shiftMask, xK_n), spawn "nm-connection-editor")
      -- Start Libreoffice
      , ((mod .|. shiftMask, xK_t), spawn "libreoffice")
      -- AV/PÅ Border
      , ((mod, xK_Escape), withFocused toggleBorder)
      -- Gaps
      , ((mod .|. shiftMask, xK_f), sendMessage $ setGaps [(U,0), (R,0), (D,0),(L,0)])
      , ((mod .|. shiftMask, xK_a), sendMessage $ setGaps [(U,10), (R,10), (D,10),(L,10)])
      -- Lukk Vindu
      , ((mod .|. shiftMask, xK_q), kill)   
      -- Quit xmonad
      , ((mod .|. shiftMask, xK_e), io (exitWith ExitSuccess))
      -- Restart xmonad
      , ((mod .|. shiftMask, xK_r), spawn "xmonad --recompile; xmonad --restart")
      -- Lyd
      , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
      , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
      , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle")
      -- Lys
      , ((0, xF86XK_MonBrightnessUp), spawn "lux -a 5%")
      , ((0, xF86XK_MonBrightnessDown), spawn "lux -s 5%")
--- Layout Hotkeys
      , ((mod .|. controlMask, xK_1), sendMessage $ JumpToLayout "Tall")
      , ((mod1Mask, xK_w), sinkAll)
      , ((mod .|. controlMask, xK_2), sendMessage $ JumpToLayout "Sprial")
      , ((mod .|. controlMask, xK_3), sendMessage $ JumpToLayout "Grid")
      , ((mod .|. controlMask, xK_Tab), sendMessage NextLayout)
      , ((mod .|. controlMask, xK_4), sendMessage $ JumpToLayout "Mirror")
      , ((mod .|. controlMask, xK_5), sendMessage $ JumpToLayout "Float")
      , ((mod .|. controlMask, xK_6), sendMessage $ JumpToLayout "Full")
      , ((mod .|. controlMask, xK_7), sendMessage $ JumpToLayout "Treecolumns")
      , ((mod .|. controlMask, xK_8), sendMessage $ JumpToLayout "Multicolumns")
      , ((mod .|. controlMask, xK_u), withFocused $ windows . W.sink)
--- Windows
      , ((mod, xK_a), windows W.focusMaster) 
      , ((mod, xK_j), windows W.focusDown)  
      , ((mod, xK_k), windows W.focusUp)    
      , ((mod .|. shiftMask, xK_j), windows W.swapDown)
      , ((mod .|. shiftMask, xK_k), windows W.swapUp)
      , ((mod, xK_space), promote)
--- Juster Vindu
      , ((mod, xK_u), sendMessage Shrink)
      , ((mod, xK_i), sendMessage Expand)
      , ((mod, xK_Right), nextWS)     
      , ((mod, xK_Left), prevWS)
--- Skjermer
      , ((mod .|. shiftMask, xK_Right), shiftNextScreen)
      , ((mod .|. shiftMask, xK_Left), shiftPrevScreen)
      , ((mod, xK_Up), nextScreen)
      , ((mod, xK_Down), prevScreen)
-- End_keys

      ]
    ++ 
-- surf to use instead of LibreWolf under "Nett"
--, ((mod .|. shiftMask, xK_Tab), spawn "surf -SBdI https://startpage.com")

-- Workspaces Key binding

    [((m .|. mod, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)
       ]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    
    [((m .|. mod, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_y, xK_x, xK_g] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

--- Mus ---
myMouseBindings (XConfig {XMonad.modMask = mod}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((mod, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((mod, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((mod, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    ]

myEventHook = mempty
myLogHook = return ()

myStartupHook :: X ()
myStartupHook = do
                setWMName "X"
		        --spawnOnce "mpv ~/Privat/Frihetens_forpost.mp3"
                spawnOnce "~/.fehbg"
                spawnOnce "picom --experimental-backends"
                spawnOnce "lxsession"
                spawnOnce "dbus-update-activation-environment --systemd DISPLAY eval $(/usr/bin/gnome-keyring-deamon --start --components=pkcs11,secrets,ssh) export SSH_AUTH_SOCK &"
                spawnOnce "dunst"
                spawnOnce "nm-applet"
                spawnOnce "xautolock -time 30 -locker 'systemctl suspend'"
                spawnOnce "emacsclient -c -a 'emacs'"
                spawnOnce "signal-desktop"
                spawnOnce "teams-for-linux"
                spawnOnce "trayer --edge top --align right --distance 5 --width 3 --expand true --SetDockType true --SetPartialStrut True --transparent true --alpha 0 --tint 0x282A36 --expand true --height 15 --monitor 1 --padding 1"
                spawnOnce "~/Script/husk_oppdater.sh"
                spawnOnce "geary"
		spawnOnce "/usr/bin/emacs --daemon"

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
  xmonad $ docks
         $ withUrgencyHook NoUrgencyHook
         $ defaults { 
         logHook = dynamicLogWithPP $ xmobarPP            
              {
                 ppTitle = const ""
               , ppTitleSanitize = const ""  
               , ppWsSep = " | "
               , ppOutput = hPutStrLn xmproc
               , ppLayout = xmobarColor "#50fa7b" "#282a36"
               , ppCurrent = xmobarColor "#8be9fd" "#282a36"
               , ppHiddenNoWindows = xmobarColor "#ff76c6" "#282a36"
               , ppHidden = xmobarColor "#bd93f9" "#282a36"
               , ppUrgent = xmobarColor "#ff5555" "#282a36"
               }
       }

defaults = def { 
      -- simple stuff
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        terminal           = myTerminal,
        -- numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        -- hooks, layouts
        layoutHook         = myLayouts,
        normalBorderColor  = myNormColor,
        focusedBorderColor = myFocusColor,
        logHook            = myLogHook,
        startupHook        = myStartupHook,
        handleEventHook    = myEventHook,
        manageHook         = myManageHook 
    }
