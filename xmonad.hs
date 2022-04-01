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
import XMonad.Hooks.UrgencyHook
import XMonad.Util.WorkspaceCompare
import XMonad.Actions.CycleWS
import XMonad.Layout.Spacing
import XMonad.Hooks.ToggleHook
import XMonad.Actions.NoBorders
import XMonad.Layout.Gaps
-- Layouts
import XMonad.Layout hiding ( (|||) ) 
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Renamed
import XMonad.Layout.Spiral
import XMonad.Layout.Grid
import XMonad.Layout.SimpleFloat
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiColumns
--- Themes ---
--import XMonad.Util.Themes
--- Definer variable ---
myTerminal = "kitty"
myLauncher = "rofi -show run"

--- Regler for Xmonad ---
-- Fokus vindu der mus er
myFocusFollowsMouse  :: Bool
myFocusFollowsMouse  = True

-- Border
myBorderWidth = 2
myFocusColor = "#ff79c6"
myNormColor   = "#282a36"

-- Set super Key
myModMask = mod4Mask

--- WS ---
myWorkspaces = ["1 Term","2 Signal","3 Thunar","4 Nett","5 Discord","6 Teams","7 Lyd","8","9"]

myManageHook = composeAll
    [ className =? "confirm"         --> doFloat
    , className =? "file_progress"   --> doFloat
    , className =? "dialog"          --> doFloat
    , className =? "download"        --> doFloat
    , className =? "error"           --> doFloat
    , className =? "Nm-connection-editor" --> doFloat
    , className =? "Gtk2_prefs" --> doFloat
    , className =? "kitty" --> doShift "1 Term"
    , className =? "Signal" --> doShift "2 Signal"
    , className =? "Thunar" --> doShift "3 Thunar"
    , className =? "librewolf" --> doShift "4 Nett" 
    , className =? "discord" --> doShift "5 Discord"
    , className =? "teams-for-linux" --> doShift "6 Teams"
    , className =? "Pavucontrol" --> doShift "7 Lyd"
    , className =? "Steam" --> doShift "8"
    , className =? "code-oss" --> doShift "8"
    , className =? "libreoffice" --> doShift "8"
    , className =? "lunarclient" --> doShift "8"
    , className =? "Gimp" --> doShift "8"
    ]

--- Layouts ---
myLayouts = avoidStruts $
            gaps [(U,0), (R,0), (L,0), (D,0)] $
-- Uncomment hvis du vil ha gaps rundt vindu
            layoutTall 
        ||| layoutSpiral 
        ||| layoutGrid 
        ||| layoutMirror 
        ||| layoutFloat
        ||| layoutTreeColumns
        ||| layoutMultiColumns

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
      layoutTreeColumns =
                 renamed [Replace "3C"]
                 $ ThreeCol 1 (3/100) (1/2)
      layoutMultiColumns =
                 renamed [Replace "MC"]
                 $ multiCol [1] 1 0.01 (-0.5)
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
      -- Start Teams
      , ((mod, xK_t), spawn "teams-for-linux")
      -- Start Steam
      , ((mod .|. shiftMask, xK_o), spawn "steam")
      -- Start pavucontol
      , ((mod .|. shiftMask, xK_l), spawn "pavucontrol")
      -- Start Coreshot
      , ((mod, xK_p), spawn "coreshot") 
      -- Start Nett instillinger
      , ((mod .|. shiftMask, xK_n), spawn "nm-connection-editor")
      -- AV/PÅ Border
      , ((mod, xK_Escape), withFocused toggleBorder)
      -- Gaps
      , ((mod .|. controlMask, xK_s), sendMessage $ setGaps [(U,0), (R,0), (D,0),(L,0)]) 
      , ((mod .|. controlMask, xK_a), sendMessage $ setGaps [(U,5), (R,5), (D,5),(L,5)]) 
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
--- Layout Hotkeys
      , ((mod .|. shiftMask, xK_s), sendMessage $ JumpToLayout "tall")
      , ((mod, xK_w), sinkAll) 
      , ((mod, xK_s), sendMessage $ JumpToLayout "sprial")
      , ((mod, xK_b), sendMessage $ JumpToLayout "grid")
      , ((mod, xK_Tab), sendMessage NextLayout)
      , ((mod, xK_i), sendMessage $ JumpToLayout "mirror")
      , ((mod, xK_f), sendMessage $ JumpToLayout "float")
      , ((mod .|. shiftMask, xK_k), sendMessage $ JumpToLayout "3C")
      , ((mod, xK_c), sendMessage $ JumpToLayout "MC")
      , ((mod .|. shiftMask, xK_u), withFocused $ windows . W.sink)
--- Windows
      , ((mod, xK_a), windows W.focusMaster) 
      , ((mod, xK_Down), windows W.focusDown)  
      , ((mod, xK_Up), windows W.focusUp)    
      , ((mod .|. shiftMask, xK_Down), windows W.swapDown)
      , ((mod .|. shiftMask, xK_Up), windows W.swapUp)
      , ((mod, xK_space), promote)
--- Juster Vindu
      , ((mod, xK_u), sendMessage Shrink)
      , ((mod, xK_v), sendMessage Expand)
      , ((mod, xK_Right), nextWS)     
      , ((mod, xK_Left), prevWS)
--- Skjermer
      , ((mod .|. shiftMask, xK_Right), shiftNextScreen)
      , ((mod .|. shiftMask, xK_Left), shiftPrevScreen)
      , ((mod, xK_j), nextScreen)
      , ((mod, xK_k), prevScreen)
      ]
    ++

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

-- Kjør xmonad med alle konfig i denne filen (Ikke fjern)
myEventHook = mempty
myLogHook = return ()

--- Autostart ---
myStartupHook :: X ()
myStartupHook = do
                setWMName "X"
                spawnOnce "nm-applet"
                spawnOnce "xautolock -locker 'systemctl suspend'"
                spawnOnce myTerminal
                spawnOnce "thunar" 
                spawnOnce "signal-desktop"
                spawnOnce "discord-canary"
                spawnOnce "teams-for-linux"
                spawnOnce "librewolf"
                spawnOnce "pavucontrol"
                spawnOnce  "trayer --edge top --align right --widthtype request --expand true --SetDockType true --SetPartialStrut true --transparent false --alpha 0 --tint 282a36 --expand true --height 20 --monitor 1 --padding 1"
                spawnOnce "~/script/husk_oppdater.sh"
main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
  xmproc1 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
  xmproc2 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
  xmonad $ docks
         $ withUrgencyHook NoUrgencyHook
         $ defaults { 
         logHook = dynamicLogWithPP xmobarPP            
              {
                 ppTitle = const ""
               , ppTitleSanitize = const ""  
               , ppWsSep = " | "
               , ppOutput = \x -> hPutStrLn xmproc0 x 
                               >> hPutStrLn xmproc1 x 
                               >> hPutStrLn xmproc2 x
               , ppLayout = xmobarColor "#50fa7b" "#282a36"
               , ppCurrent = xmobarColor "#8be9fd" "#282a36"
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
