--- Imports ---
import XMonad hiding ( (|||) )
import System.Directory
import XMonad.Core
import Data.Monoid
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import System.Exit
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import System.IO
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Actions.Navigation2D
import XMonad.Actions.UpdatePointer
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig
import Graphics.X11.ExtraTypes.XF86
import XMonad.Actions.Promote
import XMonad.Actions.WithAll 
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import qualified XMonad.Layout.ToggleLayouts as T
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.WorkspaceCompare
import XMonad.Util.NamedScratchpad 
-- Layouts
import XMonad.Layout hiding ( (|||) ) 
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Renamed
import XMonad.Layout.Spiral
import XMonad.Layout.Grid
import XMonad.Layout.SimpleFloat

--- Definer variable ---
myTerminal = "kitty"
myLauncher = "rofi -show run"

--- Regler for Xmonad ---
-- Fokus vindu der mus er
myFocusFollowsMouse  :: Bool
myFocusFollowsMouse  = True

-- Border
myBorderWidth = 0

-- Set super Key
myModMask = mod4Mask

-- WS
myWorkspaces = ["1","2 Term","3 Signal","4 Thunar","5 Nett","6 Discord","7 Teams","8 Lyd","9 Nett-Ins"]

myManageHook = composeAll
    [ className =? "confirm"         --> doFloat
    , className =? "file_progress"   --> doFloat
    , className =? "dialog"          --> doFloat
    , className =? "download"        --> doFloat
    , className =? "error"           --> doFloat
    , className =? "Steam" --> doShift "1"
    , className =? "code-oss" --> doShift "1"
    , className =? "libreoffice" --> doShift "1"
    , className =? "lunarclient" --> doShift "1"
    , className =? "kitty" --> doShift "2 Term"
    , className =? "Signal" --> doShift "3 Signal"
    , className =? "Thunar" --> doShift "4 Thunar"
    , className =? "librewolf" --> doShift "5 Nett" 
    , className =? "discord" --> doShift "6 Discord"
    , className =? "teams-for-linux" --> doShift "7 Teams"
    , className =? "Pavucontrol" --> doShift "8 Lyd"
    , className =? "Nm-connection-editor" --> doShift "9 Nett-Ins"]

--- Layouts ---
myLayouts = avoidStruts $
            layoutTall ||| layoutSpiral ||| layoutGrid ||| layoutMirror ||| layoutFloat
    where
      layoutTall =
                 renamed [Replace "tall"]
                 $ Tall 1 (3/100) (1/2)
      layoutSpiral = 
                 renamed [Replace "sprial"]
                 $ spiral (6/7)
      layoutGrid =
                 renamed [Replace "grid"]
                 $ Grid
      layoutMirror =
                 renamed [Replace "mirror"]
                 $ Mirror (Tall 1 (3/100) (3/5))
      layoutFloat =
                 renamed [Replace "float"]
                 $ smartBorders
                 $ limitWindows 20 simpleFloat
  
--- HotKeys ---
myKeys conf@(XConfig {XMonad.modMask = mod}) = M.fromList $
      -- Start Terminal
      [ ((mod, xK_Return), spawn myTerminal)
      -- Start Rofi
      , ((mod, xK_d), spawn myLauncher)
      -- Start Nett
      , ((mod .|. shiftMask, xK_Tab), spawn "librewolf")
      -- Start Thunar
      , ((mod .|. shiftMask, xK_f), spawn "thunar")
      -- Start Minecraft
      , ((mod, xK_m), spawn "lunarclient")
      -- Start Discord
      , ((mod .|. shiftMask, xK_d), spawn "discord-canary")
      -- lås PC
      , ((mod, xK_l), spawn "betterlockscreen -l")
      -- Start Code
      , ((mod, xK_o), spawn "code")
      -- Start Steam
      , ((mod .|. shiftMask, xK_o), spawn "steam")
      -- Start pavucontol
      , ((mod .|. shiftMask, xK_l), spawn "pavucontrol")
      -- Start Coreshot
      , ((mod, xK_p), spawn "coreshot") 
      -- Lukk Vindu
      , ((mod .|. shiftMask, xK_q), kill)   
      -- Quit xmonad
      , ((mod .|. shiftMask, xK_e), io (exitWith ExitSuccess))
      -- Restart xmonad
      , ((mod .|. shiftMask, xK_r), spawn "xmonad --recompile; xmonad --restart")
      -- Lyd
      , ((0 , xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
      , ((0 , xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
      , ((0 , xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle")
--- Layout Hotkeys
      , ((mod .|. shiftMask, xK_s), sendMessage $ JumpToLayout "tall")
      , ((mod, xK_w), sinkAll) 
      , ((mod, xK_s), sendMessage $ JumpToLayout "sprial")
      , ((mod, xK_b), sendMessage $ JumpToLayout "grid")
      , ((mod, xK_Tab), sendMessage NextLayout)
      , ((mod, xK_i), sendMessage $ JumpToLayout "mirror")
      , ((mod, xK_f), sendMessage $ JumpToLayout "float")
      , ((mod .|. shiftMask, xK_u), withFocused $ windows . W.sink)
--- Windows
      , ((mod, xK_a), windows W.focusMaster) 
      , ((mod, xK_Down), windows W.focusDown)  
      , ((mod, xK_Up), windows W.focusUp)    
      , ((mod .|. shiftMask, xK_w), windows W.swapMaster) 
      , ((mod .|. shiftMask, xK_Down), windows W.swapDown)
      , ((mod .|. shiftMask, xK_m), promote)
--- Juster Vindu
      , ((mod, xK_u), sendMessage Shrink)
      , ((mod, xK_v), sendMessage Expand)                  
      ]
    ++

-- Workspaces Key binding

    [((m .|. mod, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    
    [((m .|. mod, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_Up, xK_Left, xK_Right] [0..]
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

-- Kjør xmonad med alle konfig i denne filen (Ikke fjern)
myEventHook = mempty
myLogHook = return ()

--- Autostart ---
myStartupHook :: X ()
myStartupHook = do
                setWMName "X"
                spawnOnce "nm-applet"
                spawnOnce "volumeicon"
                spawnOnce "xautolock -time 5 -locker 'systemctl suspend'"
                spawnOnce myTerminal
                spawnOnce "thunar" 
                spawnOnce "signal-desktop"
                spawnOnce "discord-canary"
                spawnOnce "teams-for-linux"
                spawnOnce "librewolf"
                spawnOnce "pavucontrol"
                spawnOnce "nm-connection-editor"
                spawnOnce "trayer --edge top --align right --widthtype request --expand true --SetDockType true --SetPartialStrut true --transparent true --alpha 0 --tint 0x1A1918 --expand true --heighttype pixel --height 14 --monitor 1 --padding 1"
main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
  xmproc1 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
  xmproc2 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
  xmonad $ docks
         $ ewmh
         $ defaults { 
         logHook = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP  xmobarPP { 
                 ppSep = "   "
               , ppOutput = \x -> hPutStrLn xmproc0 x 
                               >> hPutStrLn xmproc1 x 
                               >> hPutStrLn xmproc2 x
               , ppCurrent = xmobarColor "white" "black" . wrap "[" "]"
               , ppUrgent = xmobarColor "red" "black" . wrap "!" "!"
               , ppVisible = xmobarColor "gray" "black" 
          }
       }
          
defaults = defaultConfig { 
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
        logHook            = myLogHook,
        startupHook        = myStartupHook,
        handleEventHook    = myEventHook,
        manageHook         = myManageHook 
    }
