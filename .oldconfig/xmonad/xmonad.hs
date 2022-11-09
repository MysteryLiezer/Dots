  -- Base
import XMonad
import XMonad.ManageHook (title,appName)
import System.Directory
import System.IO (hClose, hPutStr, hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.GridSelect
import XMonad.Actions.GroupNavigation
import XMonad.Actions.DynamicProjects
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.TagWindows
import XMonad.Actions.EasyMotion (selectWindow, sKeys, EasyMotionConfig(..), textSize, ChordKeys(..), fixedSize)
import XMonad.Actions.Minimize
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (hiddenWS, ignoringWSs, wsTagGroup, prevWS, nextWS, shiftToPrev, shiftToNext, Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen, toggleWS, emptyWS)
--import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ShowWName (flashName)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WorkspaceHistory

    -- Layouts
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Minimize
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import XMonad.Layout.BoringWindows as BW hiding (Replace)
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
   -- Utilities
import XMonad.Util.ExclusiveScratchpads
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap)
import XMonad.Util.NamedActions
-- import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

   -- ColorScheme module (SET ONLY ONE!)
      -- Possible choice are:
      -- DoomOne
      -- Dracula
      -- GruvboxDark
      -- MonokaiPro
      -- Nord
      -- OceanicNext
      -- Palenight
      -- SolarizedDark
      -- SolarizedLight
      -- TomorrowNight
import Colors.DoomOne

myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=6:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask        -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty"    -- Sets default terminal

myBrowser :: String
myBrowser = "qutebrowser"  -- Sets qutebrowser as browser

myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "  -- Makes emacs keybindings easier to type

myEditor :: String
myEditor = "emacsclient -c -a 'emacs' "  -- Sets emacs as editor
-- myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor

myBorderWidth :: Dimension
myBorderWidth = 1           -- Sets border width for windows

myNormColor :: String       -- Border color of normal windows
myNormColor   = colorBack   -- This variable is imported from Colors.THEME

myFocusColor :: String      -- Border color of focused windows
myFocusColor  = color15     -- This variable is imported from Colors.THEME

mySoundPlayer :: String
mySoundPlayer = "ffplay -nodisp -autoexit " -- The program that will play system sounds

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
  spawnOnce (mySoundPlayer ++ startupSound)
  spawn "killall conky"   -- kill current conky on each restart
  spawn "killall trayer"  -- kill current trayer on each restart

  spawnOnce "lxsession"
  spawnOnce "picom"
  spawnOnce "nm-applet"
  spawnOnce "volumeicon"
  spawn "/usr/bin/emacs --daemon" -- emacs daemon for the emacsclient

  spawnOnce "xargs xwallpaper --stretch < ~/.cache/wall"
  -- spawnOnce "~/.fehbg &"  -- set last saved feh wallpaper
  -- spawnOnce "feh --randomize --bg-fill /usr/share/backgrounds/dtos-backgrounds/*"  -- feh set random wallpaper
  -- spawnOnce "nitrogen --restore &"   -- if you prefer nitrogen to feh
  setWMName "LG3D"

exclusiveSps = mkXScratchpads [ ("htop",   "urxvt -name htop -e htop", title =? "htop")
                              , ("xclock", "xclock", appName =? "xclock")
                              , ("browser", "qutebrowser --target=window https://www.google.com", title =? "browser")
                              , ("terminal", "alacritty", title =? "terminal")
                              , ("calculator", "qalculate-gtk", title =? "calculator")
                              ] $ customFloating $ W.RationalRect l t w h -- (1/4) (1/4) (1/2) (1/2)
                                                                   where
                                                                     h = 0.9
                                                                     w = 0.9
                                                                     t = 0.95 -h
                                                                     l = 0.95 -w

scratchpads = exclusiveSps

tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = 0xdd292d3e
                              , TS.ts_font         = "xft:Ubuntu:size=10"
                              , TS.ts_node         = (0xffd0d0d0, 0xff202331)
                              , TS.ts_nodealt      = (0xffd0d0d0, 0xff292d3e)
                              , TS.ts_highlight    = (0xffffffff, 0xff755999)
                              , TS.ts_extra        = 0xffd0d0d0
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 20
                              , TS.ts_originX      = 0
                              , TS.ts_originY      = 0
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = myTreeNavigation
                              }

treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
   [ Node (TS.TSNode "Hello"    "displays hello"      (sendMessage NextLayout)) []
   , Node (TS.TSNode "Shutdown" "Poweroff the system" (spawn "shutdown")) []
   , Node (TS.TSNode "Brightness" "Sets screen brightness using xbacklight" (return ()))
       [ Node (TS.TSNode "Bright" "FULL POWER!!"            (spawn "xbacklight -set 100")) []
       , Node (TS.TSNode "Normal" "Normal Brightness (50%)" (spawn "xbacklight -set 50"))  []
       , Node (TS.TSNode "Dim"    "Quite dark"              (spawn "xbacklight -set 10"))  []
       ]
   ]

myTreeNavigation = M.fromList
    [ ((0, xK_Escape),   TS.cancel)
    , ((0, xK_Return),   TS.select)
    , ((0, xK_space),    TS.select)
    , ((0, xK_Up),       TS.movePrev)
    , ((0, xK_Down),     TS.moveNext)
    , ((0, xK_Left),     TS.moveParent)
    , ((0, xK_Right),    TS.moveChild)
    , ((0, xK_k),        TS.movePrev)
    , ((0, xK_j),        TS.moveNext)
    , ((0, xK_h),        TS.moveParent)
    , ((0, xK_l),        TS.moveChild)
    , ((0, xK_o),        TS.moveHistBack)
    , ((0, xK_i),        TS.moveHistForward)
    , ((0, xK_1),        TS.moveTo ["{Hacking}"])
    , ((0, xK_2),        TS.moveTo ["{Programming}"])
    , ((0, xK_3),        TS.moveTo ["{Media}"])
    , ((0, xK_4),        TS.moveTo ["{Social}"])
    , ((0, xK_5),        TS.moveTo ["{Misc}"])
    , ((mod4Mask, xK_1), TS.moveTo ["{Hacking}", "$Terminals", "1>"])
    , ((mod4Mask, xK_q), TS.moveTo ["{Hacking}", "@Applications", "1>"])
    , ((mod4Mask, xK_a), TS.moveTo ["{Hacking}", "#Browsers", "1>"])
    , ((mod4Mask, xK_z), TS.moveTo ["{Hacking}", "|Scratch|", "1>"])
    , ((mod4Mask, xK_2), TS.moveTo ["{Programming}", "$Terminals", "<1"])
    , ((mod4Mask, xK_w), TS.moveTo ["{Programming}", "@Applications", "<1"])
    , ((mod4Mask, xK_s), TS.moveTo ["{Programming}", "#Browsers", "<1"])
    , ((mod4Mask, xK_x), TS.moveTo ["{Programming}", "|Scratch|", "<1"])
    , ((mod4Mask, xK_3), TS.moveTo ["{Media}", "$Terminals", "!1"])
    , ((mod4Mask, xK_e), TS.moveTo ["{Media}", "@Applications", "!1"])
    , ((mod4Mask, xK_d), TS.moveTo ["{Media}", "#Browsers", "!1"])
    , ((mod4Mask, xK_c), TS.moveTo ["{Media}", "|Scratch|", "!1"])
    , ((mod4Mask, xK_4), TS.moveTo ["{Social}", "$Terminals", "+1"])
    , ((mod4Mask, xK_r), TS.moveTo ["{Social}", "@Applications", "+1"])
    , ((mod4Mask, xK_f), TS.moveTo ["{Social}", "#Browsers", "+1"])
    , ((mod4Mask, xK_v), TS.moveTo ["{Social}", "|Scratch|", "+1"])
    , ((mod4Mask, xK_5), TS.moveTo ["{Misc}", "$Terminals", "~1"])
    , ((mod4Mask, xK_t), TS.moveTo ["{Misc}", "@Applications", "~1"])
    , ((mod4Mask, xK_g), TS.moveTo ["{Misc}", "#Browsers", "~1"])
    , ((mod4Mask, xK_b), TS.moveTo ["{Misc}", "|Scratch|", "~1"])
    ]

projects :: [Project]
projects =
  [ Project { projectName      = "{Hacking}"
            , projectDirectory = "/tmp"
            , projectStartHook = Just $ do spawn "qutebrowser --target=window https://ticktick.com/webapp/#p/6361982a8f0897c728093f89/tasks"
            }

  , Project { projectName      = "{Programming}"
            , projectDirectory = "/tmp"
            , projectStartHook = Just $ do spawn "qutebrowser --target=window https://ticktick.com/webapp/#p/6361983d8f0897c728094056/tasks"
            }

  , Project { projectName      = "{Media}"
            , projectDirectory = "/tmp"
            , projectStartHook = Just $ do spawn "qutebrowser --target=window https://ticktick.com/webapp/#p/6361984d8f0897c7280940ed/tasks"
            }

  , Project { projectName      = "{Social}"
            , projectDirectory = "/tmp"
            , projectStartHook = Just $ do spawn "qutebrowser --target=window https://ticktick.com/webapp/#p/636198708f0897c728094333/tasks"
            }

  , Project { projectName      = "{Misc}"
            , projectDirectory = "/tmp"
            , projectStartHook = Just $ do spawn "qutebrowser --target=window https://ticktick.com/webapp/#p/636198808f0897c7280943e0/tasks"
            }

  , Project { projectName      = "{Hacking}.|Scratch|"
            , projectDirectory = "/Documents"
            , projectStartHook = Just $ do spawn "emacsclient -c -a 'emacs' --eval '(dashboard-refresh-buffer)'"
            }

  , Project { projectName      = "{Programming}.|Scratch|"
            , projectDirectory = "/Documents"
            , projectStartHook = Just $ do spawn "emacsclient -c -a 'emacs' --eval '(dashboard-refresh-buffer)'"
            }

  , Project { projectName      = "{Media}.|Scratch|"
            , projectDirectory = "/Documents"
            , projectStartHook = Just $ do spawn "emacsclient -c -a 'emacs' --eval '(dashboard-refresh-buffer)'"
            }

  , Project { projectName      = "{Social}.|Scratch|"
            , projectDirectory = "/Documents"
            , projectStartHook = Just $ do spawn "emacsclient -c -a 'emacs' --eval '(dashboard-refresh-buffer)'"
            }

  , Project { projectName      = "{Misc}.|Scratch|"
            , projectDirectory = "/Documents"
            , projectStartHook = Just $ do spawn "emacsclient -c -a 'emacs' --eval '(dashboard-refresh-buffer)'"
            }

  , Project { projectName      = "{Hacking}.$Terminals>"
            , projectDirectory = "/tmp"
            , projectStartHook = Just $ do spawn myTerminal
            }

  , Project { projectName      = "{Programming}.$Terminals<"
            , projectDirectory = "/tmp"
            , projectStartHook = Just $ do spawn myTerminal
            }

  , Project { projectName      = "{Media}.$Terminals!"
            , projectDirectory = "~/Downloads"
            , projectStartHook = Just $ do spawn myTerminal
            }

  , Project { projectName      = "{Social}.$Terminals+"
            , projectDirectory = "/tmp"
            , projectStartHook = Just $ do spawn myTerminal
            }

  , Project { projectName      = "{Misc}.$Terminals~"
            , projectDirectory = "/tmp"
            , projectStartHook = Just $ do spawn myTerminal
            }

  , Project { projectName      = "{Hacking}.#Browsers>"
            , projectDirectory = "/tmp"
            , projectStartHook = Just $ do spawn "qutebrowser --target=window https://academy.hackthebox.com/dashboard"
            }

  , Project { projectName      = "{Programming}.#Browsers<"
            , projectDirectory = "/tmp"
            , projectStartHook = Just $ do spawn "qutebrowser --target=window https://stackoverflow.com"
            }

  , Project { projectName      = "{Media}.#Browsers!"
            , projectDirectory = "/Downloads"
            , projectStartHook = Just $ do spawn "qutebrowser --target=window https://www.youtube.com"
            }

  , Project { projectName      = "{Social}.#Browsers+"
            , projectDirectory = "/tmp"
            , projectStartHook = Just $ do spawn "qutebrowser --target=window https://reddit.com"
            }

  , Project { projectName      = "{Misc}.#Browsers~"
            , projectDirectory = "/tmp"
            , projectStartHook = Just $ do spawn "qutebrowser --target=window https://google.com"
            }

  , Project { projectName      = "{Programming}.@Applications<"
            , projectDirectory = "/tmp"
            , projectStartHook = Just $ do spawn "/home/farhan/.local/share/JetBrains/Toolbox/apps/IDEA-U/ch-0/222.4345.14/bin/idea.sh"
            }


  ]

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
 where navKeyMap = M.fromList [
          ((0,xK_Escape), cancel)
         ,((0,xK_Return), select)
         ,((0,xK_slash) , substringSearch myNavigation)
         ,((0,xK_Left)  , move (-1,0)  >> myNavigation)
         ,((0,xK_h)     , move (-1,0)  >> myNavigation)
         ,((0,xK_Right) , move (1,0)   >> myNavigation)
         ,((0,xK_l)     , move (1,0)   >> myNavigation)
         ,((0,xK_Down)  , move (0,1)   >> myNavigation)
         ,((0,xK_j)     , move (0,1)   >> myNavigation)
         ,((0,xK_Up)    , move (0,-1)  >> myNavigation)
         ,((0,xK_k)     , move (0,-1)  >> myNavigation)
         ,((0,xK_y)     , move (-1,-1) >> myNavigation)
         ,((0,xK_i)     , move (1,-1)  >> myNavigation)
         ,((0,xK_n)     , move (-1,1)  >> myNavigation)
         ,((0,xK_m)     , move (1,-1)  >> myNavigation)
         ,((0,xK_space) , setPos (0,0) >> myNavigation)
         ]
       navDefaultHandler = const myNavigation

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                (0x28,0x2c,0x34) -- lowest inactive bg
                (0x28,0x2c,0x34) -- highest inactive bg
                (0xc7,0x92,0xea) -- active bg
                (0xc0,0xa7,0x9a) -- inactive fg
                (0x28,0x2c,0x34) -- active fg

mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_navigate    = myNavigation
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnMocp  = myTerminal ++ " -t mocp -e mocp"
    findMocp   = title =? "mocp"
    manageMocp = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCalc  = "qalculate-gtk"
    findCalc   = className =? "Qalculate-gtk"
    manageCalc = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.4
                 t = 0.75 -h
                 l = 0.70 -w
    spawnBrow  = myBrowser ++ " -t qutebrowsers"
    findBrow   = title =? "qutebrowsers"
    manageBrow = customFloating $ W.RationalRect l t w h
               where
                 h = 0.7
                 w = 0.4
                 t = 0.75 -h
                 l = 0.70 -w

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ limitWindows 5
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ mySpacing 0
           $ ResizableTall 1 (3/100) (70/100) []
grid     = renamed [Replace "grid"]
           $ limitWindows 9
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ mySpacing 0
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
tabs     = renamed [Replace "tabs"]
           $ noBorders
           $ tabbed shrinkText myTabTheme

myTabTheme = def { fontName            = myFont
                 , activeColor         = color15
                 , inactiveColor       = color08
                 , activeBorderColor   = color15
                 , inactiveBorderColor = colorBack
                 , activeTextColor     = colorBack
                 , inactiveTextColor   = color16
                 }

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
  { swn_font              = "xft:Ubuntu:bold:size=45"
  , swn_bgcolor           = "#1c1f24"
  , swn_color             = "#ffffff"
  }

myLayoutHook = avoidStruts
               $ mouseResize
               $ windowArrange
--               $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout = tabs
                                             ||| tall
                                             ||| grid

myWorkspaces :: Forest String
myWorkspaces = [ Node "{Hacking}"       -- for everyday activity's
                   [ Node "$Terminals>"
                       [ Node "1>" []
                       , Node "2>" []
                       , Node "3>" []
                       , Node "4>" []
                       ]--  with 4 extra sub-workspaces, for even more activity's
                   , Node "@Applications>"
                       [ Node "1>" []
                       , Node "2>" []
                       , Node "3>" []
                       , Node "4>" []
                       ]
                   , Node "#Browsers>"
                       [ Node "1>" []
                       , Node "2>" []
                       , Node "3>" []
                       , Node "4>" []
                       ]
                   , Node "|Scratch|"
                       [ Node "1" []
                       , Node "2" []
                       , Node "3" []
                       , Node "4" []
                       ]
                   ]
               , Node "{Programming}" -- for all your programming needs
                   [ Node "$Terminals<"
                       [ Node "1<" []
                       , Node "2<" []
                       , Node "3<" []
                       , Node "4<" []
                       ]--  with 4 extra sub-workspaces, for even more activity's
                   , Node "@Applications<"
                       [ Node "1<" []
                       , Node "2<" []
                       , Node "3<" []
                       , Node "4<" []
                       ]
                   , Node "#Browsers<"
                       [ Node "1<" []
                       , Node "2<" []
                       , Node "3<" []
                       , Node "4<" []
                       ]
                   , Node "|Scratch|"
                       [ Node "1" []
                       , Node "2" []
                       , Node "3" []
                       , Node "4" []
                       ]
                   ]
               , Node "{Media}"
                   [ Node "$Terminals!"
                       [ Node "1!" []
                       , Node "2!" []
                       , Node "3!" []
                       , Node "4!" []
                       ]--  with 4 extra sub-workspaces, for even more activity's
                   , Node "@Applications!"
                       [ Node "1!" []
                       , Node "2!" []
                       , Node "3!" []
                       , Node "4!" []
                       ]
                   , Node "#Browsers!"
                       [ Node "1!" []
                       , Node "2!" []
                       , Node "3!" []
                       , Node "4!" []
                       ]
                   , Node "|Scratch|"
                       [ Node "1" []
                       , Node "2" []
                       , Node "3" []
                       , Node "4" []
                       ]
                   ]
               , Node "{Social}"
                   [ Node "$Terminals+"
                       [ Node "1+" []
                       , Node "2+" []
                       , Node "3+" []
                       , Node "4+" []
                       ]--  with 4 extra sub-workspaces, for even more activity's
                   , Node "@Applications+"
                       [ Node "1+" []
                       , Node "2+" []
                       , Node "3+" []
                       , Node "4+" []
                       ]
                   , Node "#Browsers+"
                       [ Node "1+" []
                       , Node "2+" []
                       , Node "3+" []
                       , Node "4+" []
                       ]
                   , Node "|Scratch|"
                       [ Node "1" []
                       , Node "2" []
                       , Node "3" []
                       , Node "4" []
                       ]
                   ]
               , Node "{Misc}"
                   [ Node "$Terminals~"
                       [ Node "1~" []
                       , Node "2~" []
                       , Node "3~" []
                       , Node "4~" []
                       ]--  with 4 extra sub-workspaces, for even more activity's
                   , Node "@Applications~"
                       [ Node "1~" []
                       , Node "2~" []
                       , Node "3~" []
                       , Node "4~" []
                       ]
                   , Node "#Browsers~"
                       [ Node "1~" []
                       , Node "2~" []
                       , Node "3~" []
                       , Node "4~" []
                       ]
                   , Node "|Scratch|"
                       [ Node "1" []
                       , Node "2" []
                       , Node "3" []
                       , Node "4" []
                       ]
                   ]
               ]

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  [ className =? "confirm"         --> doFloat
  , className =? "file_progress"   --> doFloat
  , className =? "dialog"          --> doFloat
  , className =? "download"        --> doFloat
  , className =? "error"           --> doFloat
  , className =? "Gimp"            --> doFloat
  , className =? "notification"    --> doFloat
  , className =? "pinentry-gtk-2"  --> doFloat
  , className =? "splash"          --> doFloat
  , className =? "toolbar"         --> doFloat
  , className =? "Yad"             --> doCenterFloat
  , title =? "Oracle VM VirtualBox Manager"  --> doFloat
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
  , isFullscreen -->  doFullFloat
  ] <+> xScratchpadsManageHook scratchpads

soundDir = "/opt/dtos-sounds/" -- The directory that has the sound files

startupSound  = soundDir ++ "startup-01.mp3"
shutdownSound = soundDir ++ "shutdown-01.mp3"
dmenuSound    = soundDir ++ "menu-01.mp3"

subtitle' ::  String -> ((KeyMask, KeySym), NamedAction)
subtitle' x = ((0,0), NamedAction $ map toUpper
                      $ sep ++ "\n-- " ++ x ++ " --\n" ++ sep)
  where
    sep = replicate (6 + length x) '-'

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe $ "yad --text-info --fontname=\"SauceCodePro Nerd Font Mono 12\" --fore=#46d9ff back=#282c36 --center --geometry=1200x800 --title \"XMonad keybindings\""
  --hPutStr h (unlines $ showKm x) -- showKM adds ">>" before subtitles
  hPutStr h (unlines $ showKmSimple x) -- showKmSimple doesn't add ">>" to subtitles
  hClose h
  return ()

myKeys :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
myKeys c =
--  (subtitle "Custom Keys":) $ mkNamedKeymap c $
  let subKeys str ks = subtitle' str : mkNamedKeymap c ks in
  subKeys "Xmonad Essentials"
  [ ("M-C-r", addName "Recompile XMonad"       $ spawn "xmonad --recompile")
  , ("M-S-r", addName "Restart XMonad"         $ spawn "xmonad --restart")
  , ("M-S-q", addName "Quit XMonad"            $ sequence_ [spawn (mySoundPlayer ++ shutdownSound), io exitSuccess])
  , ("M-S-c", addName "Kill focused window"    $ kill1)
  , ("M-S-a", addName "Kill all windows on WS" $ killAll)
  , ("M-S-<Return>", addName "Run prompt"      $ sequence_ [spawn (mySoundPlayer ++ dmenuSound), spawn "~/.local/bin/dm-run"])
  , ("M-/", addName "DTOS Help"                $ spawn "~/.local/bin/dtos-help")]

  ^++^ subKeys "Window navigation"
  [ ("M-j", addName "Move focus to next window"                $ windows W.focusDown)
  , ("M-k", addName "Move focus to prev window"                $ windows W.focusUp)
  , ("M-m", addName "Move focus to master window"              $ windows W.focusMaster)
--  , ("M-S-j", addName "Swap focused window with next window"   $ windows W.swapDown)
--  , ("M-S-k", addName "Swap focused window with prev window"   $ windows W.swapUp)
  , ("M-S-m", addName "Swap focused window with master window" $ windows W.swapMaster)
  , ("M-<Backspace>", addName "Move focused window to master"  $ promote)
  , ("M-S-,", addName "Rotate all windows except master"       $ rotSlavesDown)
  , ("M-S-.", addName "Rotate all windows current stack"       $ rotAllDown)]

  ^++^ subKeys "Dmenu scripts"
  [ ("M-p h", addName "List all dmscripts"     $ spawn "dm-hub")
  , ("M-p a", addName "Choose ambient sound"   $ spawn "dm-sounds")
  , ("M-p b", addName "Set background"         $ spawn "dm-setbg")
  , ("M-p c", addName "Choose color scheme"    $ spawn "~/.local/bin/dtos-colorscheme")
  , ("M-p C", addName "Pick color from scheme" $ spawn "dm-colpick")
  , ("M-p e", addName "Edit config files"      $ spawn "dm-confedit")
  , ("M-p i", addName "Take a screenshot"      $ spawn "dm-maim")
  , ("M-p k", addName "Kill processes"         $ spawn "dm-kill")
  , ("M-p m", addName "View manpages"          $ spawn "dm-man")
  , ("M-p n", addName "Store and copy notes"   $ spawn "dm-note")
  , ("M-p o", addName "Browser bookmarks"      $ spawn "dm-bookman")
  , ("M-p p", addName "Passmenu"               $ spawn "passmenu -p \"Pass: \"")
  , ("M-p q", addName "Logout Menu"            $ spawn "dm-logout")
  , ("M-p r", addName "Listen to online radio" $ spawn "dm-radio")
  , ("M-p s", addName "Search various engines" $ spawn "dm-websearch")
  , ("M-p t", addName "Translate text"         $ spawn "dm-translate")]

  ^++^ subKeys "Favorite programs"
  [ ("M-<Return>", addName "Launch terminal"   $ spawn (myTerminal))
  , ("M-b", addName "Launch web browser"       $ spawn (myBrowser))
  , ("M-M1-h", addName "Launch htop"           $ spawn (myTerminal ++ " -e htop"))]

  ^++^ subKeys "Monitors"
  [ ("M-.", addName "Switch focus to next monitor" $ nextScreen)
  , ("M-,", addName "Switch focus to prev monitor" $ prevScreen)]

  -- Switch layouts
  ^++^ subKeys "Switch layouts"
  [ ("M-<Tab>", addName "Switch to next layout"   $ sendMessage NextLayout)
  , ("M-'", addName "Toggle noborders/tabs" $ sendMessage (T.Toggle "tabs"))
  , ("M-<Space>", addName "Toggle noborders/tabs" $ sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)]

  -- Window resizing
  ^++^ subKeys "Window resizing"
  [ ("M-h", addName "Shrink window"               $ sendMessage Shrink)
  , ("M-l", addName "Expand window"               $ sendMessage Expand)
  , ("M-M1-j", addName "Shrink window vertically" $ sendMessage MirrorShrink)
  , ("M-M1-k", addName "Expand window vertically" $ sendMessage MirrorExpand)]

  -- Floating windows
  ^++^ subKeys "Floating windows"
  [ ("M-f", addName "Toggle float layout"        $ sendMessage (T.Toggle "floats"))
  , ("M-t", addName "Sink a floating window"     $ withFocused $ windows . W.sink)
  , ("M-S-t", addName "Sink all floated windows" $ sinkAll)]

  -- Increase/decrease spacing (gaps)
  ^++^ subKeys "Window spacing (gaps)"
  [ ("C-M1-j", addName "Decrease window spacing" $ decWindowSpacing 4)
  , ("C-M1-k", addName "Increase window spacing" $ incWindowSpacing 4)
  , ("C-M1-h", addName "Decrease screen spacing" $ decScreenSpacing 4)
  , ("C-M1-l", addName "Increase screen spacing" $ incScreenSpacing 4)]

  -- Increase/decrease windows in the master pane or the stack
  ^++^ subKeys "Increase/decrease windows in master pane or the stack"
  [ ("M-S-<Up>", addName "Increase clients in master pane"   $ sendMessage (IncMasterN 1))
  , ("M-S-<Down>", addName "Decrease clients in master pane" $ sendMessage (IncMasterN (-1)))
  , ("M-=", addName "Increase max # of windows for layout"   $ increaseLimit)
  , ("M--", addName "Decrease max # of windows for layout"   $ decreaseLimit)]

  -- Sublayouts
  ^++^ subKeys "Sublayouts"
  [ ("M-C-h", addName "pullGroup L"           $ sendMessage $ pullGroup L)
  , ("M-C-l", addName "pullGroup R"           $ sendMessage $ pullGroup R)
  , ("M-C-k", addName "pullGroup U"           $ sendMessage $ pullGroup U)
  , ("M-C-j", addName "pullGroup D"           $ sendMessage $ pullGroup D)
  , ("M-C-m", addName "MergeAll"              $ withFocused (sendMessage . MergeAll))
  -- , ("M-C-u", addName "UnMerge"               $ withFocused (sendMessage . UnMerge))
  , ("M-C-/", addName "UnMergeAll"            $  withFocused (sendMessage . UnMergeAll))
  , ("M-C-.", addName "Switch focus next tab" $  onGroup W.focusUp')
  , ("M-C-,", addName "Switch focus prev tab" $  onGroup W.focusDown')]

  -- Scratchpads
  ^++^ subKeys "Scratchpads"
  [ ("M-s s", addName "Toggle scratchpad terminal"   $ scratchpadAction scratchpads "terminal")
  , ("M-s h", addName "Toggle scratchpad htop"       $ scratchpadAction scratchpads "htop")
  , ("M-s c", addName "Toggle scratchpad calculator" $ scratchpadAction scratchpads "calculator")
  , ("M-s t", addName "Toggle scratchpad time"       $ scratchpadAction scratchpads "xclock")
  , ("M-s b", addName "Toggle scratchpad browser"    $ scratchpadAction scratchpads "browser")]

  -- Controls for mocp music player (SUPER-u followed by a key)
  ^++^ subKeys "Mocp music player"
  [ ("M-u p", addName "mocp play"                $ spawn "mocp --play")
  , ("M-u l", addName "mocp next"                $ spawn "mocp --next")
  , ("M-u h", addName "mocp prev"                $ spawn "mocp --previous")
  , ("M-u <Space>", addName "mocp toggle pause"  $ spawn "mocp --toggle-pause")]

  ^++^ subKeys "GridSelect"
  -- , ("C-g g", addName "Select favorite apps"     $ runSelectedAction' defaultGSConfig gsCategories)
  [ ("M-M1-t", addName "Goto selected window"    $ goToSelected $ mygridConfig myColorizer)
  , ("M-M1-b", addName "Bring selected window"   $ bringSelected $ mygridConfig myColorizer) ]
--
  -- Emacs (SUPER-e followed by a key)
  ^++^ subKeys "Emacs"
  [ ("M-e e", addName "Emacsclient Dashboard"    $ spawn (myEmacs ++ ("--eval '(dashboard-refresh-buffer)'")))
  , ("M-e a", addName "Emacsclient EMMS (music)" $ spawn (myEmacs ++ ("--eval '(emms)' --eval '(emms-play-directory-tree \"~/Music/\")'")))
  , ("M-e b", addName "Emacsclient Ibuffer"      $ spawn (myEmacs ++ ("--eval '(ibuffer)'")))
  , ("M-e d", addName "Emacsclient Dired"        $ spawn (myEmacs ++ ("--eval '(dired nil)'")))
  , ("M-e i", addName "Emacsclient ERC (IRC)"    $ spawn (myEmacs ++ ("--eval '(erc)'")))
  , ("M-e n", addName "Emacsclient Elfeed (RSS)" $ spawn (myEmacs ++ ("--eval '(elfeed)'")))
  , ("M-e s", addName "Emacsclient Eshell"       $ spawn (myEmacs ++ ("--eval '(eshell)'")))
  , ("M-e v", addName "Emacsclient Vterm"        $ spawn (myEmacs ++ ("--eval '(+vterm/here nil)'")))
  , ("M-e w", addName "Emacsclient EWW Browser"  $ spawn (myEmacs ++ ("--eval '(doom/window-maximize-buffer(eww \"distro.tube\"))'")))]

  -- Minimize Keys
  ^++^ subKeys "Minimize"
  [ ("M-M1-m", addName "Minimize" $ withFocused minimizeWindow)
  , ("M-M1-u", addName "Unminimize" $ withLastMinimized maximizeWindowAndFocus) ]

  -- Focus Windows
  ^++^ subKeys "Focus Windows"
  [ ("M-;", addName "Focus Window" $ (selectWindow def{sKeys = AnyKeys [xK_f, xK_j, xK_g, xK_h, xK_d, xK_k], overlayF = fixedSize 100 150}) >>= (`whenJust` windows . W.focusWindow))
  , ("M-a", addName "Close Window" $ (selectWindow def{sKeys = AnyKeys [xK_f, xK_j, xK_g, xK_h, xK_d, xK_k], overlayF = fixedSize 100 150}) >>= (`whenJust` killWindow))]

  -- Workspaces
  ^++^ subKeys "Workspaces"
--  [ ("M-w", addName "Previous Workspace"                     $ flashName)
  [ ("M-<Left>", addName "Previous Workspace"                $ moveTo Prev emptyWS)
  , ("M-<Right>", addName "Next Workspace"                   $ moveTo Next emptyWS)
  , ("M-S-h", addName "Shift to Previous Workspace"          $ shiftToPrev >> prevWS)
  , ("M-S-l", addName "Shift to Next Workspace"              $ shiftToNext >> nextWS)
  , ("M-M1-<Tab>", addName "Toggle Last Workspace"           $ toggleWS)
  --, ("M-M1-`", addName "Sw"                                  $ switchProjectPrompt def)
  , ("M-0", addName "Switch to Next in oup"                  $ moveTo Next $ (emptyWS) :&: ignoringWSs [ "{Hacking}"
                                                                                                   ])
  , ("M-5", addName "Switch to Next Hacking Page"            $ moveTo Next ((WSIs $ return (('>' `elem`) . W.tag)) :&: (Not emptyWS)))
  , ("M-6", addName "Switch to Next Programming Page"        $ moveTo Next ((WSIs $ return (('<' `elem`) . W.tag)) :&: (Not emptyWS)))
  , ("M-7", addName "Switch to Next Media Page"              $ moveTo Next ((WSIs $ return (('!' `elem`) . W.tag)) :&: (Not emptyWS)))
  , ("M-8", addName "Switch to Next Social Page"             $ moveTo Next ((WSIs $ return (('+' `elem`) . W.tag)) :&: (Not emptyWS)))
  , ("M-9", addName "Switch to Next Misc Page"               $ moveTo Next ((WSIs $ return (('~' `elem`) . W.tag)) :&: (Not emptyWS)))
  , ("M-S-0", addName "Switch to Next in oup"                $ moveTo Prev $ (emptyWS) :&: ignoringWSs [ "{Hacking}"
                                                                                                   , "{Programming}"
                                                                                                   , "{Media}"
                                                                                                   , "{Social}"
                                                                                                   , "{Misc}"
                                                                                                   , "{Hacking}.$Terminals>"
                                                                                                   , "{Hacking}.@Applications>"
                                                                                                   , "{Hacking}.#Browsers>"
                                                                                                   , "{Hacking}.|Scratch|"
                                                                                                   , "{Programming}.$Terminals<"
                                                                                                   , "{Programming}.@Applications<"
                                                                                                   , "{Programming}.#Browsers<"
                                                                                                   , "{Programming}.|Scratch|"
                                                                                                   , "{Media}.$Terminals!"
                                                                                                   , "{Media}.@Applications!"
                                                                                                   , "{Media}.#Browsers!"
                                                                                                   , "{Media}.|Scratch|"
                                                                                                   , "{Social}.$Terminals+"
                                                                                                   , "{Social}.@Applications+"
                                                                                                   , "{Social}.#Browsers+"
                                                                                                   , "{Social}.|Scratch|"
                                                                                                   , "{Misc}.$Terminals~"
                                                                                                   , "{Misc}.@Applications~"
                                                                                                   , "{Misc}.#Browsers~"
                                                                                                   , "{Misc}.|Scratch|"
                                                                                                   ])
  , ("M-S-5", addName "Switch to Next Hacking Page"          $ moveTo Prev ((WSIs $ return (('>' `elem`) . W.tag)) :&: (Not emptyWS)))
  , ("M-S-6", addName "Switch to Next Programming Page"      $ moveTo Prev ((WSIs $ return (('<' `elem`) . W.tag)) :&: (Not emptyWS)))
  , ("M-S-7", addName "Switch to Next Media Page"            $ moveTo Prev ((WSIs $ return (('!' `elem`) . W.tag)) :&: (Not emptyWS)))
  , ("M-S-8", addName "Switch to Next Social Page"           $ moveTo Prev ((WSIs $ return (('+' `elem`) . W.tag)) :&: (Not emptyWS)))
  , ("M-S-9", addName "Switch to Next Misc Page"             $ moveTo Prev ((WSIs $ return (('~' `elem`) . W.tag)) :&: (Not emptyWS)))
  , ("M-1", addName "Switch to Next Misc Page"               $ moveTo Next $ hiddenWS :&: ignoringWSs [ "{Hacking}.$Terminals>.1>"
                                                                                                      , "{Hacking}.$Terminals>.2>"
                                                                                                      , "{Hacking}.$Terminals>.3>"
                                                                                                      , "{Hacking}.$Terminals>.4>"
                                                                                                      , "{Hacking}.@Applications>.1>"
                                                                                                      , "{Hacking}.@Applications>.2>"
                                                                                                      , "{Hacking}.@Applications>.3>"
                                                                                                      , "{Hacking}.@Applications>.4>"
                                                                                                      , "{Hacking}.#Browsers>.1>"
                                                                                                      , "{Hacking}.#Browsers>.2>"
                                                                                                      , "{Hacking}.#Browsers>.3>"
                                                                                                      , "{Hacking}.#Browsers>.4>"
                                                                                                      , "{Programming}.$Terminals<.1<"
                                                                                                      , "{Programming}.$Terminals<.2<"
                                                                                                      , "{Programming}.$Terminals<.3<"
                                                                                                      , "{Programming}.$Terminals<.4<"
                                                                                                      , "{Programming}.@Applications<.1<"
                                                                                                      , "{Programming}.@Applications<.2<"
                                                                                                      , "{Programming}.@Applications<.3<"
                                                                                                      , "{Programming}.@Applications<.4<"
                                                                                                      , "{Programming}.#Browsers<.1<"
                                                                                                      , "{Programming}.#Browsers<.2<"
                                                                                                      , "{Programming}.#Browsers<.3<"
                                                                                                      , "{Programming}.#Browsers<.4<"
                                                                                                      , "{Media}.$Terminals!.1!"
                                                                                                      , "{Media}.$Terminals!.2!"
                                                                                                      , "{Media}.$Terminals!.3!"
                                                                                                      , "{Media}.$Terminals!.4!"
                                                                                                      , "{Media}.@Applications!.1!"
                                                                                                      , "{Media}.@Applications!.2!"
                                                                                                      , "{Media}.@Applications!.3!"
                                                                                                      , "{Media}.@Applications!.4!"
                                                                                                      , "{Media}.#Browsers!.1!"
                                                                                                      , "{Media}.#Browsers!.2!"
                                                                                                      , "{Media}.#Browsers!.3!"
                                                                                                      , "{Media}.#Browsers!.4!"
                                                                                                      , "{Social}.$Terminals+.1+"
                                                                                                      , "{Social}.$Terminals+.2+"
                                                                                                      , "{Social}.$Terminals+.3+"
                                                                                                      , "{Social}.$Terminals+.4+"
                                                                                                      , "{Social}.@Applications+.1+"
                                                                                                      , "{Social}.@Applications+.2+"
                                                                                                      , "{Social}.@Applications+.3+"
                                                                                                      , "{Social}.@Applications+.4+"
                                                                                                      , "{Social}.#Browsers+.1+"
                                                                                                      , "{Social}.#Browsers+.2+"
                                                                                                      , "{Social}.#Browsers+.3+"
                                                                                                      , "{Social}.#Browsers+.4+"
                                                                                                      , "{Misc}.$Terminals~.1~"
                                                                                                      , "{Misc}.$Terminals~.2~"
                                                                                                      , "{Misc}.$Terminals~.3~"
                                                                                                      , "{Misc}.$Terminals~.4~"
                                                                                                      , "{Misc}.@Applications~.1~"
                                                                                                      , "{Misc}.@Applications~.2~"
                                                                                                      , "{Misc}.@Applications~.3~"
                                                                                                      , "{Misc}.@Applications~.4~"
                                                                                                      , "{Misc}.#Browsers~.1~"
                                                                                                      , "{Misc}.#Browsers~.2~"
                                                                                                      , "{Misc}.#Browsers~.3~"
                                                                                                      , "{Misc}.#Browsers~.4~"
                                                                                                      , "{Hacking}.|Scratch|.1"
                                                                                                      , "{Hacking}.|Scratch|.2"
                                                                                                      , "{Hacking}.|Scratch|.3"
                                                                                                      , "{Hacking}.|Scratch|.4"
                                                                                                      , "{Programming}.|Scratch|.1"
                                                                                                      , "{Programming}.|Scratch|.2"
                                                                                                      , "{Programming}.|Scratch|.3"
                                                                                                      , "{Programming}.|Scratch|.4"
                                                                                                      , "{Media}.|Scratch|.1"
                                                                                                      , "{Media}.|Scratch|.2"
                                                                                                      , "{Media}.|Scratch|.3"
                                                                                                      , "{Media}.|Scratch|.4"
                                                                                                      , "{Social}.|Scratch|.1"
                                                                                                      , "{Social}.|Scratch|.2"
                                                                                                      , "{Social}.|Scratch|.3"
                                                                                                      , "{Social}.|Scratch|.4"
                                                                                                      , "{Misc}.|Scratch|.1"
                                                                                                      , "{Misc}.|Scratch|.2"
                                                                                                      , "{Misc}.|Scratch|.3"
                                                                                                      , "{Misc}.|Scratch|.4"
                                                                                                      , "{Hacking}"
                                                                                                      , "{Programming}"
                                                                                                      , "{Media}"
                                                                                                      , "{Social}"
                                                                                                      , "{Misc}"
                                                                                                      , "{Hacking}.@Applications>"
                                                                                                      , "{Hacking}.#Browsers>"
                                                                                                      , "{Hacking}.|Scratch|"
                                                                                                      , "{Programming}.@Applications<"
                                                                                                      , "{Programming}.#Browsers<"
                                                                                                      , "{Programming}.|Scratch|"
                                                                                                      , "{Media}.@Applications!"
                                                                                                      , "{Media}.#Browsers!"
                                                                                                      , "{Media}.|Scratch|"
                                                                                                      , "{Social}.@Applications+"
                                                                                                      , "{Social}.#Browsers+"
                                                                                                      , "{Social}.|Scratch|"
                                                                                                      , "{Misc}.@Applications~"
                                                                                                      , "{Misc}.#Browsers~"
                                                                                                      , "{Misc}.|Scratch|"
                                                                                                      ])
  , ("M-S-2", addName "Switch to Next Misc Page"               $ moveTo Prev $ hiddenWS :&: ignoringWSs [ "{Hacking}.$Terminals>.1>"
                                                                                                      , "{Hacking}.$Terminals>.2>"
                                                                                                      , "{Hacking}.$Terminals>.3>"
                                                                                                      , "{Hacking}.$Terminals>.4>"
                                                                                                      , "{Hacking}.@Applications>.1>"
                                                                                                      , "{Hacking}.@Applications>.2>"
                                                                                                      , "{Hacking}.@Applications>.3>"
                                                                                                      , "{Hacking}.@Applications>.4>"
                                                                                                      , "{Hacking}.#Browsers>.1>"
                                                                                                      , "{Hacking}.#Browsers>.2>"
                                                                                                      , "{Hacking}.#Browsers>.3>"
                                                                                                      , "{Hacking}.#Browsers>.4>"
                                                                                                      , "{Programming}.$Terminals<.1<"
                                                                                                      , "{Programming}.$Terminals<.2<"
                                                                                                      , "{Programming}.$Terminals<.3<"
                                                                                                      , "{Programming}.$Terminals<.4<"
                                                                                                      , "{Programming}.@Applications<.1<"
                                                                                                      , "{Programming}.@Applications<.2<"
                                                                                                      , "{Programming}.@Applications<.3<"
                                                                                                      , "{Programming}.@Applications<.4<"
                                                                                                      , "{Programming}.#Browsers<.1<"
                                                                                                      , "{Programming}.#Browsers<.2<"
                                                                                                      , "{Programming}.#Browsers<.3<"
                                                                                                      , "{Programming}.#Browsers<.4<"
                                                                                                      , "{Media}.$Terminals!.1!"
                                                                                                      , "{Media}.$Terminals!.2!"
                                                                                                      , "{Media}.$Terminals!.3!"
                                                                                                      , "{Media}.$Terminals!.4!"
                                                                                                      , "{Media}.@Applications!.1!"
                                                                                                      , "{Media}.@Applications!.2!"
                                                                                                      , "{Media}.@Applications!.3!"
                                                                                                      , "{Media}.@Applications!.4!"
                                                                                                      , "{Media}.#Browsers!.1!"
                                                                                                      , "{Media}.#Browsers!.2!"
                                                                                                      , "{Media}.#Browsers!.3!"
                                                                                                      , "{Media}.#Browsers!.4!"
                                                                                                      , "{Social}.$Terminals+.1+"
                                                                                                      , "{Social}.$Terminals+.2+"
                                                                                                      , "{Social}.$Terminals+.3+"
                                                                                                      , "{Social}.$Terminals+.4+"
                                                                                                      , "{Social}.@Applications+.1+"
                                                                                                      , "{Social}.@Applications+.2+"
                                                                                                      , "{Social}.@Applications+.3+"
                                                                                                      , "{Social}.@Applications+.4+"
                                                                                                      , "{Social}.#Browsers+.1+"
                                                                                                      , "{Social}.#Browsers+.2+"
                                                                                                      , "{Social}.#Browsers+.3+"
                                                                                                      , "{Social}.#Browsers+.4+"
                                                                                                      , "{Misc}.$Terminals~.1~"
                                                                                                      , "{Misc}.$Terminals~.2~"
                                                                                                      , "{Misc}.$Terminals~.3~"
                                                                                                      , "{Misc}.$Terminals~.4~"
                                                                                                      , "{Misc}.@Applications~.1~"
                                                                                                      , "{Misc}.@Applications~.2~"
                                                                                                      , "{Misc}.@Applications~.3~"
                                                                                                      , "{Misc}.@Applications~.4~"
                                                                                                      , "{Misc}.#Browsers~.1~"
                                                                                                      , "{Misc}.#Browsers~.2~"
                                                                                                      , "{Misc}.#Browsers~.3~"
                                                                                                      , "{Misc}.#Browsers~.4~"
                                                                                                      , "{Hacking}.|Scratch|.1"
                                                                                                      , "{Hacking}.|Scratch|.2"
                                                                                                      , "{Hacking}.|Scratch|.3"
                                                                                                      , "{Hacking}.|Scratch|.4"
                                                                                                      , "{Programming}.|Scratch|.1"
                                                                                                      , "{Programming}.|Scratch|.2"
                                                                                                      , "{Programming}.|Scratch|.3"
                                                                                                      , "{Programming}.|Scratch|.4"
                                                                                                      , "{Media}.|Scratch|.1"
                                                                                                      , "{Media}.|Scratch|.2"
                                                                                                      , "{Media}.|Scratch|.3"
                                                                                                      , "{Media}.|Scratch|.4"
                                                                                                      , "{Social}.|Scratch|.1"
                                                                                                      , "{Social}.|Scratch|.2"
                                                                                                      , "{Social}.|Scratch|.3"
                                                                                                      , "{Social}.|Scratch|.4"
                                                                                                      , "{Misc}.|Scratch|.1"
                                                                                                      , "{Misc}.|Scratch|.2"
                                                                                                      , "{Misc}.|Scratch|.3"
                                                                                                      , "{Misc}.|Scratch|.4"
                                                                                                      , "{Hacking}"
                                                                                                      , "{Programming}"
                                                                                                      , "{Media}"
                                                                                                      , "{Social}"
                                                                                                      , "{Misc}"
                                                                                                      , "{Hacking}.$Terminals>"
                                                                                                      , "{Hacking}.#Browsers>"
                                                                                                      , "{Hacking}.|Scratch|"
                                                                                                      , "{Programming}.$Terminals<"
                                                                                                      , "{Programming}.#Browsers<"
                                                                                                      , "{Programming}.|Scratch|"
                                                                                                      , "{Media}.$Terminals!"
                                                                                                      , "{Media}.#Browsers!"
                                                                                                      , "{Media}.|Scratch|"
                                                                                                      , "{Social}.$Terminals+"
                                                                                                      , "{Social}.#Browsers+"
                                                                                                      , "{Social}.|Scratch|"
                                                                                                      , "{Misc}.$Terminals~"
                                                                                                      , "{Misc}.#Browsers~"
                                                                                                      , "{Misc}.|Scratch|"
                                                                                                      ])
  , ("M-2", addName "Switch to Next Misc Page"               $ moveTo Next $ hiddenWS :&: ignoringWSs [ "{Hacking}.$Terminals>.1>"
                                                                                                      , "{Hacking}.$Terminals>.2>"
                                                                                                      , "{Hacking}.$Terminals>.3>"
                                                                                                      , "{Hacking}.$Terminals>.4>"
                                                                                                      , "{Hacking}.@Applications>.1>"
                                                                                                      , "{Hacking}.@Applications>.2>"
                                                                                                      , "{Hacking}.@Applications>.3>"
                                                                                                      , "{Hacking}.@Applications>.4>"
                                                                                                      , "{Hacking}.#Browsers>.1>"
                                                                                                      , "{Hacking}.#Browsers>.2>"
                                                                                                      , "{Hacking}.#Browsers>.3>"
                                                                                                      , "{Hacking}.#Browsers>.4>"
                                                                                                      , "{Programming}.$Terminals<.1<"
                                                                                                      , "{Programming}.$Terminals<.2<"
                                                                                                      , "{Programming}.$Terminals<.3<"
                                                                                                      , "{Programming}.$Terminals<.4<"
                                                                                                      , "{Programming}.@Applications<.1<"
                                                                                                      , "{Programming}.@Applications<.2<"
                                                                                                      , "{Programming}.@Applications<.3<"
                                                                                                      , "{Programming}.@Applications<.4<"
                                                                                                      , "{Programming}.#Browsers<.1<"
                                                                                                      , "{Programming}.#Browsers<.2<"
                                                                                                      , "{Programming}.#Browsers<.3<"
                                                                                                      , "{Programming}.#Browsers<.4<"
                                                                                                      , "{Media}.$Terminals!.1!"
                                                                                                      , "{Media}.$Terminals!.2!"
                                                                                                      , "{Media}.$Terminals!.3!"
                                                                                                      , "{Media}.$Terminals!.4!"
                                                                                                      , "{Media}.@Applications!.1!"
                                                                                                      , "{Media}.@Applications!.2!"
                                                                                                      , "{Media}.@Applications!.3!"
                                                                                                      , "{Media}.@Applications!.4!"
                                                                                                      , "{Media}.#Browsers!.1!"
                                                                                                      , "{Media}.#Browsers!.2!"
                                                                                                      , "{Media}.#Browsers!.3!"
                                                                                                      , "{Media}.#Browsers!.4!"
                                                                                                      , "{Social}.$Terminals+.1+"
                                                                                                      , "{Social}.$Terminals+.2+"
                                                                                                      , "{Social}.$Terminals+.3+"
                                                                                                      , "{Social}.$Terminals+.4+"
                                                                                                      , "{Social}.@Applications+.1+"
                                                                                                      , "{Social}.@Applications+.2+"
                                                                                                      , "{Social}.@Applications+.3+"
                                                                                                      , "{Social}.@Applications+.4+"
                                                                                                      , "{Social}.#Browsers+.1+"
                                                                                                      , "{Social}.#Browsers+.2+"
                                                                                                      , "{Social}.#Browsers+.3+"
                                                                                                      , "{Social}.#Browsers+.4+"
                                                                                                      , "{Misc}.$Terminals~.1~"
                                                                                                      , "{Misc}.$Terminals~.2~"
                                                                                                      , "{Misc}.$Terminals~.3~"
                                                                                                      , "{Misc}.$Terminals~.4~"
                                                                                                      , "{Misc}.@Applications~.1~"
                                                                                                      , "{Misc}.@Applications~.2~"
                                                                                                      , "{Misc}.@Applications~.3~"
                                                                                                      , "{Misc}.@Applications~.4~"
                                                                                                      , "{Misc}.#Browsers~.1~"
                                                                                                      , "{Misc}.#Browsers~.2~"
                                                                                                      , "{Misc}.#Browsers~.3~"
                                                                                                      , "{Misc}.#Browsers~.4~"
                                                                                                      , "{Hacking}.|Scratch|.1"
                                                                                                      , "{Hacking}.|Scratch|.2"
                                                                                                      , "{Hacking}.|Scratch|.3"
                                                                                                      , "{Hacking}.|Scratch|.4"
                                                                                                      , "{Programming}.|Scratch|.1"
                                                                                                      , "{Programming}.|Scratch|.2"
                                                                                                      , "{Programming}.|Scratch|.3"
                                                                                                      , "{Programming}.|Scratch|.4"
                                                                                                      , "{Media}.|Scratch|.1"
                                                                                                      , "{Media}.|Scratch|.2"
                                                                                                      , "{Media}.|Scratch|.3"
                                                                                                      , "{Media}.|Scratch|.4"
                                                                                                      , "{Social}.|Scratch|.1"
                                                                                                      , "{Social}.|Scratch|.2"
                                                                                                      , "{Social}.|Scratch|.3"
                                                                                                      , "{Social}.|Scratch|.4"
                                                                                                      , "{Misc}.|Scratch|.1"
                                                                                                      , "{Misc}.|Scratch|.2"
                                                                                                      , "{Misc}.|Scratch|.3"
                                                                                                      , "{Misc}.|Scratch|.4"
                                                                                                      , "{Hacking}"
                                                                                                      , "{Programming}"
                                                                                                      , "{Media}"
                                                                                                      , "{Social}"
                                                                                                      , "{Misc}"
                                                                                                      , "{Hacking}.$Terminals>"
                                                                                                      , "{Hacking}.#Browsers>"
                                                                                                      , "{Hacking}.|Scratch|"
                                                                                                      , "{Programming}.$Terminals<"
                                                                                                      , "{Programming}.#Browsers<"
                                                                                                      , "{Programming}.|Scratch|"
                                                                                                      , "{Media}.$Terminals!"
                                                                                                      , "{Media}.#Browsers!"
                                                                                                      , "{Media}.|Scratch|"
                                                                                                      , "{Social}.$Terminals+"
                                                                                                      , "{Social}.#Browsers+"
                                                                                                      , "{Social}.|Scratch|"
                                                                                                      , "{Misc}.$Terminals~"
                                                                                                      , "{Misc}.#Browsers~"
                                                                                                      , "{Misc}.|Scratch|"
                                                                                                      ])
  , ("M-S-3", addName "Switch to Next Misc Page"               $ moveTo Prev $ hiddenWS :&: ignoringWSs [ "{Hacking}.$Terminals>.1>"
                                                                                                      , "{Hacking}.$Terminals>.2>"
                                                                                                      , "{Hacking}.$Terminals>.3>"
                                                                                                      , "{Hacking}.$Terminals>.4>"
                                                                                                      , "{Hacking}.@Applications>.1>"
                                                                                                      , "{Hacking}.@Applications>.2>"
                                                                                                      , "{Hacking}.@Applications>.3>"
                                                                                                      , "{Hacking}.@Applications>.4>"
                                                                                                      , "{Hacking}.#Browsers>.1>"
                                                                                                      , "{Hacking}.#Browsers>.2>"
                                                                                                      , "{Hacking}.#Browsers>.3>"
                                                                                                      , "{Hacking}.#Browsers>.4>"
                                                                                                      , "{Programming}.$Terminals<.1<"
                                                                                                      , "{Programming}.$Terminals<.2<"
                                                                                                      , "{Programming}.$Terminals<.3<"
                                                                                                      , "{Programming}.$Terminals<.4<"
                                                                                                      , "{Programming}.@Applications<.1<"
                                                                                                      , "{Programming}.@Applications<.2<"
                                                                                                      , "{Programming}.@Applications<.3<"
                                                                                                      , "{Programming}.@Applications<.4<"
                                                                                                      , "{Programming}.#Browsers<.1<"
                                                                                                      , "{Programming}.#Browsers<.2<"
                                                                                                      , "{Programming}.#Browsers<.3<"
                                                                                                      , "{Programming}.#Browsers<.4<"
                                                                                                      , "{Media}.$Terminals!.1!"
                                                                                                      , "{Media}.$Terminals!.2!"
                                                                                                      , "{Media}.$Terminals!.3!"
                                                                                                      , "{Media}.$Terminals!.4!"
                                                                                                      , "{Media}.@Applications!.1!"
                                                                                                      , "{Media}.@Applications!.2!"
                                                                                                      , "{Media}.@Applications!.3!"
                                                                                                      , "{Media}.@Applications!.4!"
                                                                                                      , "{Media}.#Browsers!.1!"
                                                                                                      , "{Media}.#Browsers!.2!"
                                                                                                      , "{Media}.#Browsers!.3!"
                                                                                                      , "{Media}.#Browsers!.4!"
                                                                                                      , "{Social}.$Terminals+.1+"
                                                                                                      , "{Social}.$Terminals+.2+"
                                                                                                      , "{Social}.$Terminals+.3+"
                                                                                                      , "{Social}.$Terminals+.4+"
                                                                                                      , "{Social}.@Applications+.1+"
                                                                                                      , "{Social}.@Applications+.2+"
                                                                                                      , "{Social}.@Applications+.3+"
                                                                                                      , "{Social}.@Applications+.4+"
                                                                                                      , "{Social}.#Browsers+.1+"
                                                                                                      , "{Social}.#Browsers+.2+"
                                                                                                      , "{Social}.#Browsers+.3+"
                                                                                                      , "{Social}.#Browsers+.4+"
                                                                                                      , "{Misc}.$Terminals~.1~"
                                                                                                      , "{Misc}.$Terminals~.2~"
                                                                                                      , "{Misc}.$Terminals~.3~"
                                                                                                      , "{Misc}.$Terminals~.4~"
                                                                                                      , "{Misc}.@Applications~.1~"
                                                                                                      , "{Misc}.@Applications~.2~"
                                                                                                      , "{Misc}.@Applications~.3~"
                                                                                                      , "{Misc}.@Applications~.4~"
                                                                                                      , "{Misc}.#Browsers~.1~"
                                                                                                      , "{Misc}.#Browsers~.2~"
                                                                                                      , "{Misc}.#Browsers~.3~"
                                                                                                      , "{Misc}.#Browsers~.4~"
                                                                                                      , "{Hacking}.|Scratch|.1>"
                                                                                                      , "{Hacking}.|Scratch|.2>"
                                                                                                      , "{Hacking}.|Scratch|.3>"
                                                                                                      , "{Hacking}.|Scratch|.4>"
                                                                                                      , "{Programming}.|Scratch|.<1"
                                                                                                      , "{Programming}.|Scratch|.<2"
                                                                                                      , "{Programming}.|Scratch|.<3"
                                                                                                      , "{Programming}.|Scratch|.<4"
                                                                                                      , "{Media}.|Scratch|.!1"
                                                                                                      , "{Media}.|Scratch|.!2"
                                                                                                      , "{Media}.|Scratch|.!3"
                                                                                                      , "{Media}.|Scratch|.!4"
                                                                                                      , "{Social}.|Scratch|.+1"
                                                                                                      , "{Social}.|Scratch|.+2"
                                                                                                      , "{Social}.|Scratch|.+3"
                                                                                                      , "{Social}.|Scratch|.+4"
                                                                                                      , "{Misc}.|Scratch|.~1"
                                                                                                      , "{Misc}.|Scratch|.~2"
                                                                                                      , "{Misc}.|Scratch|.~3"
                                                                                                      , "{Misc}.|Scratch|.~4"
                                                                                                      , "{Hacking}"
                                                                                                      , "{Programming}"
                                                                                                      , "{Media}"
                                                                                                      , "{Social}"
                                                                                                      , "{Misc}"
                                                                                                      , "{Hacking}.$Terminals>"
                                                                                                      , "{Hacking}.@Applications>"
                                                                                                      , "{Hacking}.|Scratch|"
                                                                                                      , "{Programming}.$Terminals"
                                                                                                      , "{Programming}.@Applications"
                                                                                                      , "{Programming}.|Scratch|"
                                                                                                      , "{Media}.$Terminals"
                                                                                                      , "{Media}.@Applications"
                                                                                                      , "{Media}.|Scratch|"
                                                                                                      , "{Social}.$Terminals"
                                                                                                      , "{Social}.@Applications"
                                                                                                      , "{Social}.|Scratch|"
                                                                                                      , "{Misc}.$Terminals"
                                                                                                      , "{Misc}.@Applications"
                                                                                                      , "{Misc}.|Scratch|"
                                                                                                      ])
  , ("M-3", addName "Switch to Next Misc Page"               $ moveTo Next $ hiddenWS :&: ignoringWSs [ "{Hacking}.$Terminals>.1>"
                                                                                                      , "{Hacking}.$Terminals>.2>"
                                                                                                      , "{Hacking}.$Terminals>.3>"
                                                                                                      , "{Hacking}.$Terminals>.4>"
                                                                                                      , "{Hacking}.@Applications>.1>"
                                                                                                      , "{Hacking}.@Applications>.2>"
                                                                                                      , "{Hacking}.@Applications>.3>"
                                                                                                      , "{Hacking}.@Applications>.4>"
                                                                                                      , "{Hacking}.#Browsers>.1>"
                                                                                                      , "{Hacking}.#Browsers>.2>"
                                                                                                      , "{Hacking}.#Browsers>.3>"
                                                                                                      , "{Hacking}.#Browsers>.4>"
                                                                                                      , "{Programming}.$Terminals<.1<"
                                                                                                      , "{Programming}.$Terminals<.2<"
                                                                                                      , "{Programming}.$Terminals<.3<"
                                                                                                      , "{Programming}.$Terminals<.4<"
                                                                                                      , "{Programming}.@Applications<.1<"
                                                                                                      , "{Programming}.@Applications<.2<"
                                                                                                      , "{Programming}.@Applications<.3<"
                                                                                                      , "{Programming}.@Applications<.4<"
                                                                                                      , "{Programming}.#Browsers<.1<"
                                                                                                      , "{Programming}.#Browsers<.2<"
                                                                                                      , "{Programming}.#Browsers<.3<"
                                                                                                      , "{Programming}.#Browsers<.4<"
                                                                                                      , "{Media}.$Terminals!.1!"
                                                                                                      , "{Media}.$Terminals!.2!"
                                                                                                      , "{Media}.$Terminals!.3!"
                                                                                                      , "{Media}.$Terminals!.4!"
                                                                                                      , "{Media}.@Applications!.1!"
                                                                                                      , "{Media}.@Applications!.2!"
                                                                                                      , "{Media}.@Applications!.3!"
                                                                                                      , "{Media}.@Applications!.4!"
                                                                                                      , "{Media}.#Browsers!.1!"
                                                                                                      , "{Media}.#Browsers!.2!"
                                                                                                      , "{Media}.#Browsers!.3!"
                                                                                                      , "{Media}.#Browsers!.4!"
                                                                                                      , "{Social}.$Terminals+.1+"
                                                                                                      , "{Social}.$Terminals+.2+"
                                                                                                      , "{Social}.$Terminals+.3+"
                                                                                                      , "{Social}.$Terminals+.4+"
                                                                                                      , "{Social}.@Applications+.1+"
                                                                                                      , "{Social}.@Applications+.2+"
                                                                                                      , "{Social}.@Applications+.3+"
                                                                                                      , "{Social}.@Applications+.4+"
                                                                                                      , "{Social}.#Browsers+.1+"
                                                                                                      , "{Social}.#Browsers+.2+"
                                                                                                      , "{Social}.#Browsers+.3+"
                                                                                                      , "{Social}.#Browsers+.4+"
                                                                                                      , "{Misc}.$Terminals~.1~"
                                                                                                      , "{Misc}.$Terminals~.2~"
                                                                                                      , "{Misc}.$Terminals~.3~"
                                                                                                      , "{Misc}.$Terminals~.4~"
                                                                                                      , "{Misc}.@Applications~.1~"
                                                                                                      , "{Misc}.@Applications~.2~"
                                                                                                      , "{Misc}.@Applications~.3~"
                                                                                                      , "{Misc}.@Applications~.4~"
                                                                                                      , "{Misc}.#Browsers~.1~"
                                                                                                      , "{Misc}.#Browsers~.2~"
                                                                                                      , "{Misc}.#Browsers~.3~"
                                                                                                      , "{Misc}.#Browsers~.4~"
                                                                                                      , "{Hacking}.|Scratch|.1>"
                                                                                                      , "{Hacking}.|Scratch|.2>"
                                                                                                      , "{Hacking}.|Scratch|.3>"
                                                                                                      , "{Hacking}.|Scratch|.4>"
                                                                                                      , "{Programming}.|Scratch|.<1"
                                                                                                      , "{Programming}.|Scratch|.<2"
                                                                                                      , "{Programming}.|Scratch|.<3"
                                                                                                      , "{Programming}.|Scratch|.<4"
                                                                                                      , "{Media}.|Scratch|.!1"
                                                                                                      , "{Media}.|Scratch|.!2"
                                                                                                      , "{Media}.|Scratch|.!3"
                                                                                                      , "{Media}.|Scratch|.!4"
                                                                                                      , "{Social}.|Scratch|.+1"
                                                                                                      , "{Social}.|Scratch|.+2"
                                                                                                      , "{Social}.|Scratch|.+3"
                                                                                                      , "{Social}.|Scratch|.+4"
                                                                                                      , "{Misc}.|Scratch|.~1"
                                                                                                      , "{Misc}.|Scratch|.~2"
                                                                                                      , "{Misc}.|Scratch|.~3"
                                                                                                      , "{Misc}.|Scratch|.~4"
                                                                                                      , "{Hacking}"
                                                                                                      , "{Programming}"
                                                                                                      , "{Media}"
                                                                                                      , "{Social}"
                                                                                                      , "{Misc}"
                                                                                                      , "{Hacking}.$Terminals>"
                                                                                                      , "{Hacking}.@Applications>"
                                                                                                      , "{Hacking}.|Scratch|"
                                                                                                      , "{Programming}.$Terminals"
                                                                                                      , "{Programming}.@Applications"
                                                                                                      , "{Programming}.|Scratch|"
                                                                                                      , "{Media}.$Terminals"
                                                                                                      , "{Media}.@Applications"
                                                                                                      , "{Media}.|Scratch|"
                                                                                                      , "{Social}.$Terminals"
                                                                                                      , "{Social}.@Applications"
                                                                                                      , "{Social}.|Scratch|"
                                                                                                      , "{Misc}.$Terminals"
                                                                                                      , "{Misc}.@Applications"
                                                                                                      , "{Misc}.|Scratch|"
                                                                                                      ])
  , ("M-S-4", addName "Switch to Next Misc Page"               $ moveTo Prev $ hiddenWS :&: ignoringWSs [ "{Hacking}.$Terminals>.1>"
                                                                                                      , "{Hacking}.$Terminals>.2>"
                                                                                                      , "{Hacking}.$Terminals>.3>"
                                                                                                      , "{Hacking}.$Terminals>.4>"
                                                                                                      , "{Hacking}.@Applications>.1>"
                                                                                                      , "{Hacking}.@Applications>.2>"
                                                                                                      , "{Hacking}.@Applications>.3>"
                                                                                                      , "{Hacking}.@Applications>.4>"
                                                                                                      , "{Hacking}.#Browsers>.1>"
                                                                                                      , "{Hacking}.#Browsers>.2>"
                                                                                                      , "{Hacking}.#Browsers>.3>"
                                                                                                      , "{Hacking}.#Browsers>.4>"
                                                                                                      , "{Programming}.$Terminals<.1<"
                                                                                                      , "{Programming}.$Terminals<.2<"
                                                                                                      , "{Programming}.$Terminals<.3<"
                                                                                                      , "{Programming}.$Terminals<.4<"
                                                                                                      , "{Programming}.@Applications<.1<"
                                                                                                      , "{Programming}.@Applications<.2<"
                                                                                                      , "{Programming}.@Applications<.3<"
                                                                                                      , "{Programming}.@Applications<.4<"
                                                                                                      , "{Programming}.#Browsers<.1<"
                                                                                                      , "{Programming}.#Browsers<.2<"
                                                                                                      , "{Programming}.#Browsers<.3<"
                                                                                                      , "{Programming}.#Browsers<.4<"
                                                                                                      , "{Media}.$Terminals!.1!"
                                                                                                      , "{Media}.$Terminals!.2!"
                                                                                                      , "{Media}.$Terminals!.3!"
                                                                                                      , "{Media}.$Terminals!.4!"
                                                                                                      , "{Media}.@Applications!.1!"
                                                                                                      , "{Media}.@Applications!.2!"
                                                                                                      , "{Media}.@Applications!.3!"
                                                                                                      , "{Media}.@Applications!.4!"
                                                                                                      , "{Media}.#Browsers!.1!"
                                                                                                      , "{Media}.#Browsers!.2!"
                                                                                                      , "{Media}.#Browsers!.3!"
                                                                                                      , "{Media}.#Browsers!.4!"
                                                                                                      , "{Social}.$Terminals+.1+"
                                                                                                      , "{Social}.$Terminals+.2+"
                                                                                                      , "{Social}.$Terminals+.3+"
                                                                                                      , "{Social}.$Terminals+.4+"
                                                                                                      , "{Social}.@Applications+.1+"
                                                                                                      , "{Social}.@Applications+.2+"
                                                                                                      , "{Social}.@Applications+.3+"
                                                                                                      , "{Social}.@Applications+.4+"
                                                                                                      , "{Social}.#Browsers+.1+"
                                                                                                      , "{Social}.#Browsers+.2+"
                                                                                                      , "{Social}.#Browsers+.3+"
                                                                                                      , "{Social}.#Browsers+.4+"
                                                                                                      , "{Misc}.$Terminals~.1~"
                                                                                                      , "{Misc}.$Terminals~.2~"
                                                                                                      , "{Misc}.$Terminals~.3~"
                                                                                                      , "{Misc}.$Terminals~.4~"
                                                                                                      , "{Misc}.@Applications~.1~"
                                                                                                      , "{Misc}.@Applications~.2~"
                                                                                                      , "{Misc}.@Applications~.3~"
                                                                                                      , "{Misc}.@Applications~.4~"
                                                                                                      , "{Misc}.#Browsers~.1~"
                                                                                                      , "{Misc}.#Browsers~.2~"
                                                                                                      , "{Misc}.#Browsers~.3~"
                                                                                                      , "{Misc}.#Browsers~.4~"
                                                                                                      , "{Hacking}.|Scratch|.1>"
                                                                                                      , "{Hacking}.|Scratch|.2>"
                                                                                                      , "{Hacking}.|Scratch|.3>"
                                                                                                      , "{Hacking}.|Scratch|.4>"
                                                                                                      , "{Programming}.|Scratch|.<1"
                                                                                                      , "{Programming}.|Scratch|.<2"
                                                                                                      , "{Programming}.|Scratch|.<3"
                                                                                                      , "{Programming}.|Scratch|.<4"
                                                                                                      , "{Media}.|Scratch|.!1"
                                                                                                      , "{Media}.|Scratch|.!2"
                                                                                                      , "{Media}.|Scratch|.!3"
                                                                                                      , "{Media}.|Scratch|.!4"
                                                                                                      , "{Social}.|Scratch|.+1"
                                                                                                      , "{Social}.|Scratch|.+2"
                                                                                                      , "{Social}.|Scratch|.+3"
                                                                                                      , "{Social}.|Scratch|.+4"
                                                                                                      , "{Misc}.|Scratch|.~1"
                                                                                                      , "{Misc}.|Scratch|.~2"
                                                                                                      , "{Misc}.|Scratch|.~3"
                                                                                                      , "{Misc}.|Scratch|.~4"
                                                                                                      , "{Hacking}"
                                                                                                      , "{Programming}"
                                                                                                      , "{Media}"
                                                                                                      , "{Social}"
                                                                                                      , "{Misc}"
                                                                                                      , "{Hacking}.$Terminals>"
                                                                                                      , "{Hacking}.@Applications>"
                                                                                                      , "{Hacking}.#Browsers"
                                                                                                      , "{Programming}.$Terminals"
                                                                                                      , "{Programming}.@Applications"
                                                                                                      , "{Programming}.#Browsers"
                                                                                                      , "{Media}.$Terminals"
                                                                                                      , "{Media}.@Applications"
                                                                                                      , "{Media}.#Browsers"
                                                                                                      , "{Social}.$Terminals"
                                                                                                      , "{Social}.@Applications"
                                                                                                      , "{Social}.#Browsers"
                                                                                                      , "{Misc}.$Terminals"
                                                                                                      , "{Misc}.@Applications"
                                                                                                      , "{Misc}.#Browsers"
                                                                                                      ])
  , ("M-4", addName "Switch to Next Misc Page"               $ moveTo Next $ hiddenWS :&: ignoringWSs [ "{Hacking}.$Terminals>.1>"
                                                                                                      , "{Hacking}.$Terminals>.2>"
                                                                                                      , "{Hacking}.$Terminals>.3>"
                                                                                                      , "{Hacking}.$Terminals>.4>"
                                                                                                      , "{Hacking}.@Applications>.1>"
                                                                                                      , "{Hacking}.@Applications>.2>"
                                                                                                      , "{Hacking}.@Applications>.3>"
                                                                                                      , "{Hacking}.@Applications>.4>"
                                                                                                      , "{Hacking}.#Browsers>.1>"
                                                                                                      , "{Hacking}.#Browsers>.2>"
                                                                                                      , "{Hacking}.#Browsers>.3>"
                                                                                                      , "{Hacking}.#Browsers>.4>"
                                                                                                      , "{Programming}.$Terminals<.1<"
                                                                                                      , "{Programming}.$Terminals<.2<"
                                                                                                      , "{Programming}.$Terminals<.3<"
                                                                                                      , "{Programming}.$Terminals<.4<"
                                                                                                      , "{Programming}.@Applications<.1<"
                                                                                                      , "{Programming}.@Applications<.2<"
                                                                                                      , "{Programming}.@Applications<.3<"
                                                                                                      , "{Programming}.@Applications<.4<"
                                                                                                      , "{Programming}.#Browsers<.1<"
                                                                                                      , "{Programming}.#Browsers<.2<"
                                                                                                      , "{Programming}.#Browsers<.3<"
                                                                                                      , "{Programming}.#Browsers<.4<"
                                                                                                      , "{Media}.$Terminals!.1!"
                                                                                                      , "{Media}.$Terminals!.2!"
                                                                                                      , "{Media}.$Terminals!.3!"
                                                                                                      , "{Media}.$Terminals!.4!"
                                                                                                      , "{Media}.@Applications!.1!"
                                                                                                      , "{Media}.@Applications!.2!"
                                                                                                      , "{Media}.@Applications!.3!"
                                                                                                      , "{Media}.@Applications!.4!"
                                                                                                      , "{Media}.#Browsers!.1!"
                                                                                                      , "{Media}.#Browsers!.2!"
                                                                                                      , "{Media}.#Browsers!.3!"
                                                                                                      , "{Media}.#Browsers!.4!"
                                                                                                      , "{Social}.$Terminals+.1+"
                                                                                                      , "{Social}.$Terminals+.2+"
                                                                                                      , "{Social}.$Terminals+.3+"
                                                                                                      , "{Social}.$Terminals+.4+"
                                                                                                      , "{Social}.@Applications+.1+"
                                                                                                      , "{Social}.@Applications+.2+"
                                                                                                      , "{Social}.@Applications+.3+"
                                                                                                      , "{Social}.@Applications+.4+"
                                                                                                      , "{Social}.#Browsers+.1+"
                                                                                                      , "{Social}.#Browsers+.2+"
                                                                                                      , "{Social}.#Browsers+.3+"
                                                                                                      , "{Social}.#Browsers+.4+"
                                                                                                      , "{Misc}.$Terminals~.1~"
                                                                                                      , "{Misc}.$Terminals~.2~"
                                                                                                      , "{Misc}.$Terminals~.3~"
                                                                                                      , "{Misc}.$Terminals~.4~"
                                                                                                      , "{Misc}.@Applications~.1~"
                                                                                                      , "{Misc}.@Applications~.2~"
                                                                                                      , "{Misc}.@Applications~.3~"
                                                                                                      , "{Misc}.@Applications~.4~"
                                                                                                      , "{Misc}.#Browsers~.1~"
                                                                                                      , "{Misc}.#Browsers~.2~"
                                                                                                      , "{Misc}.#Browsers~.3~"
                                                                                                      , "{Misc}.#Browsers~.4~"
                                                                                                      , "{Hacking}.|Scratch|.1>"
                                                                                                      , "{Hacking}.|Scratch|.2>"
                                                                                                      , "{Hacking}.|Scratch|.3>"
                                                                                                      , "{Hacking}.|Scratch|.4>"
                                                                                                      , "{Programming}.|Scratch|.<1"
                                                                                                      , "{Programming}.|Scratch|.<2"
                                                                                                      , "{Programming}.|Scratch|.<3"
                                                                                                      , "{Programming}.|Scratch|.<4"
                                                                                                      , "{Media}.|Scratch|.!1"
                                                                                                      , "{Media}.|Scratch|.!2"
                                                                                                      , "{Media}.|Scratch|.!3"
                                                                                                      , "{Media}.|Scratch|.!4"
                                                                                                      , "{Social}.|Scratch|.+1"
                                                                                                      , "{Social}.|Scratch|.+2"
                                                                                                      , "{Social}.|Scratch|.+3"
                                                                                                      , "{Social}.|Scratch|.+4"
                                                                                                      , "{Misc}.|Scratch|.~1"
                                                                                                      , "{Misc}.|Scratch|.~2"
                                                                                                      , "{Misc}.|Scratch|.~3"
                                                                                                      , "{Misc}.|Scratch|.~4"
                                                                                                      , "{Hacking}"
                                                                                                      , "{Programming}"
                                                                                                      , "{Media}"
                                                                                                      , "{Social}"
                                                                                                      , "{Misc}"
                                                                                                      , "{Hacking}.$Terminals>"
                                                                                                      , "{Hacking}.@Applications>"
                                                                                                      , "{Hacking}.#Browsers"
                                                                                                      , "{Programming}.$Terminals"
                                                                                                      , "{Programming}.@Applications"
                                                                                                      , "{Programming}.#Browsers"
                                                                                                      , "{Media}.$Terminals"
                                                                                                      , "{Media}.@Applications"
                                                                                                      , "{Media}.#Browsers"
                                                                                                      , "{Social}.$Terminals"
                                                                                                      , "{Social}.@Applications"
                                                                                                      , "{Social}.#Browsers"
                                                                                                      , "{Misc}.$Terminals"
                                                                                                      , "{Misc}.@Applications"
                                                                                                      , "{Misc}.#Browsers"
                                                                                                      ])
  , ("M-S-1", addName "Switch to Next Misc Page"               $ moveTo Prev $ hiddenWS :&: ignoringWSs [ "{Hacking}.$Terminals>.1>"
                                                                                                      , "{Hacking}.$Terminals>.2>"
                                                                                                      , "{Hacking}.$Terminals>.3>"
                                                                                                      , "{Hacking}.$Terminals>.4>"
                                                                                                      , "{Hacking}.@Applications>.1>"
                                                                                                      , "{Hacking}.@Applications>.2>"
                                                                                                      , "{Hacking}.@Applications>.3>"
                                                                                                      , "{Hacking}.@Applications>.4>"
                                                                                                      , "{Hacking}.#Browsers>.1>"
                                                                                                      , "{Hacking}.#Browsers>.2>"
                                                                                                      , "{Hacking}.#Browsers>.3>"
                                                                                                      , "{Hacking}.#Browsers>.4>"
                                                                                                      , "{Programming}.$Terminals<.1<"
                                                                                                      , "{Programming}.$Terminals<.2<"
                                                                                                      , "{Programming}.$Terminals<.3<"
                                                                                                      , "{Programming}.$Terminals<.4<"
                                                                                                      , "{Programming}.@Applications<.1<"
                                                                                                      , "{Programming}.@Applications<.2<"
                                                                                                      , "{Programming}.@Applications<.3<"
                                                                                                      , "{Programming}.@Applications<.4<"
                                                                                                      , "{Programming}.#Browsers<.1<"
                                                                                                      , "{Programming}.#Browsers<.2<"
                                                                                                      , "{Programming}.#Browsers<.3<"
                                                                                                      , "{Programming}.#Browsers<.4<"
                                                                                                      , "{Media}.$Terminals!.1!"
                                                                                                      , "{Media}.$Terminals!.2!"
                                                                                                      , "{Media}.$Terminals!.3!"
                                                                                                      , "{Media}.$Terminals!.4!"
                                                                                                      , "{Media}.@Applications!.1!"
                                                                                                      , "{Media}.@Applications!.2!"
                                                                                                      , "{Media}.@Applications!.3!"
                                                                                                      , "{Media}.@Applications!.4!"
                                                                                                      , "{Media}.#Browsers!.1!"
                                                                                                      , "{Media}.#Browsers!.2!"
                                                                                                      , "{Media}.#Browsers!.3!"
                                                                                                      , "{Media}.#Browsers!.4!"
                                                                                                      , "{Social}.$Terminals+.1+"
                                                                                                      , "{Social}.$Terminals+.2+"
                                                                                                      , "{Social}.$Terminals+.3+"
                                                                                                      , "{Social}.$Terminals+.4+"
                                                                                                      , "{Social}.@Applications+.1+"
                                                                                                      , "{Social}.@Applications+.2+"
                                                                                                      , "{Social}.@Applications+.3+"
                                                                                                      , "{Social}.@Applications+.4+"
                                                                                                      , "{Social}.#Browsers+.1+"
                                                                                                      , "{Social}.#Browsers+.2+"
                                                                                                      , "{Social}.#Browsers+.3+"
                                                                                                      , "{Social}.#Browsers+.4+"
                                                                                                      , "{Misc}.$Terminals~.1~"
                                                                                                      , "{Misc}.$Terminals~.2~"
                                                                                                      , "{Misc}.$Terminals~.3~"
                                                                                                      , "{Misc}.$Terminals~.4~"
                                                                                                      , "{Misc}.@Applications~.1~"
                                                                                                      , "{Misc}.@Applications~.2~"
                                                                                                      , "{Misc}.@Applications~.3~"
                                                                                                      , "{Misc}.@Applications~.4~"
                                                                                                      , "{Misc}.#Browsers~.1~"
                                                                                                      , "{Misc}.#Browsers~.2~"
                                                                                                      , "{Misc}.#Browsers~.3~"
                                                                                                      , "{Misc}.#Browsers~.4~"
                                                                                                      , "{Hacking}.|Scratch|.1"
                                                                                                      , "{Hacking}.|Scratch|.2"
                                                                                                      , "{Hacking}.|Scratch|.3"
                                                                                                      , "{Hacking}.|Scratch|.4"
                                                                                                      , "{Programming}.|Scratch|.1"
                                                                                                      , "{Programming}.|Scratch|.2"
                                                                                                      , "{Programming}.|Scratch|.3"
                                                                                                      , "{Programming}.|Scratch|.4"
                                                                                                      , "{Media}.|Scratch|.1"
                                                                                                      , "{Media}.|Scratch|.2"
                                                                                                      , "{Media}.|Scratch|.3"
                                                                                                      , "{Media}.|Scratch|.4"
                                                                                                      , "{Social}.|Scratch|.1"
                                                                                                      , "{Social}.|Scratch|.2"
                                                                                                      , "{Social}.|Scratch|.3"
                                                                                                      , "{Social}.|Scratch|.4"
                                                                                                      , "{Misc}.|Scratch|.1"
                                                                                                      , "{Misc}.|Scratch|.2"
                                                                                                      , "{Misc}.|Scratch|.3"
                                                                                                      , "{Misc}.|Scratch|.4"
                                                                                                      , "{Hacking}"
                                                                                                      , "{Programming}"
                                                                                                      , "{Media}"
                                                                                                      , "{Social}"
                                                                                                      , "{Misc}"
                                                                                                      , "{Hacking}.@Applications>"
                                                                                                      , "{Hacking}.#Browsers>"
                                                                                                      , "{Hacking}.|Scratch|"
                                                                                                      , "{Programming}.@Applications<"
                                                                                                      , "{Programming}.#Browsers<"
                                                                                                      , "{Programming}.|Scratch|"
                                                                                                      , "{Media}.@Applications!"
                                                                                                      , "{Media}.#Browsers!"
                                                                                                      , "{Media}.|Scratch|"
                                                                                                      , "{Social}.@Applications+"
                                                                                                      , "{Social}.#Browsers+"
                                                                                                      , "{Social}.|Scratch|"
                                                                                                      , "{Misc}.@Applications~"
                                                                                                      , "{Misc}.#Browsers~"
                                                                                                      , "{Misc}.|Scratch|"
                                                                                                      ])
  , ("M-S-`", addName "Switch to Next Programming Page"      $ moveTo Prev $ hiddenWS :&: ignoringWSs [ "{Hacking}.$Terminals>.1>"
                                                                                                      , "{Hacking}.$Terminals>.2>"
                                                                                                      , "{Hacking}.$Terminals>.3>"
                                                                                                      , "{Hacking}.$Terminals>.4>"
                                                                                                      , "{Hacking}.@Applications>.1>"
                                                                                                      , "{Hacking}.@Applications>.2>"
                                                                                                      , "{Hacking}.@Applications>.3>"
                                                                                                      , "{Hacking}.@Applications>.4>"
                                                                                                      , "{Hacking}.#Browsers>.1>"
                                                                                                      , "{Hacking}.#Browsers>.2>"
                                                                                                      , "{Hacking}.#Browsers>.3>"
                                                                                                      , "{Hacking}.#Browsers>.4>"
                                                                                                      , "{Hacking}.|Scratch|.1>"
                                                                                                      , "{Hacking}.|Scratch|.2>"
                                                                                                      , "{Hacking}.|Scratch|.3>"
                                                                                                      , "{Hacking}.|Scratch|.4>"
                                                                                                      , "{Programming}.$Terminals<.1<"
                                                                                                      , "{Programming}.$Terminals<.2<"
                                                                                                      , "{Programming}.$Terminals<.3<"
                                                                                                      , "{Programming}.$Terminals<.4<"
                                                                                                      , "{Programming}.@Applications<.1<"
                                                                                                      , "{Programming}.@Applications<.2<"
                                                                                                      , "{Programming}.@Applications<.3<"
                                                                                                      , "{Programming}.@Applications<.4<"
                                                                                                      , "{Programming}.#Browsers<.1<"
                                                                                                      , "{Programming}.#Browsers<.2<"
                                                                                                      , "{Programming}.#Browsers<.3<"
                                                                                                      , "{Programming}.#Browsers<.4<"
                                                                                                      , "{Programming}.|Scratch|.<1"
                                                                                                      , "{Programming}.|Scratch|.<2"
                                                                                                      , "{Programming}.|Scratch|.<3"
                                                                                                      , "{Programming}.|Scratch|.<4"
                                                                                                      , "{Media}.$Terminals!.1!"
                                                                                                      , "{Media}.$Terminals!.2!"
                                                                                                      , "{Media}.$Terminals!.3!"
                                                                                                      , "{Media}.$Terminals!.4!"
                                                                                                      , "{Media}.@Applications!.1!"
                                                                                                      , "{Media}.@Applications!.2!"
                                                                                                      , "{Media}.@Applications!.3!"
                                                                                                      , "{Media}.@Applications!.4!"
                                                                                                      , "{Media}.#Browsers!.1!"
                                                                                                      , "{Media}.#Browsers!.2!"
                                                                                                      , "{Media}.#Browsers!.3!"
                                                                                                      , "{Media}.#Browsers!.4!"
                                                                                                      , "{Media}.|Scratch|.!1"
                                                                                                      , "{Media}.|Scratch|.!2"
                                                                                                      , "{Media}.|Scratch|.!3"
                                                                                                      , "{Media}.|Scratch|.!4"
                                                                                                      , "{Social}.$Terminals+.1+"
                                                                                                      , "{Social}.$Terminals+.2+"
                                                                                                      , "{Social}.$Terminals+.3+"
                                                                                                      , "{Social}.$Terminals+.4+"
                                                                                                      , "{Social}.@Applications+.1+"
                                                                                                      , "{Social}.@Applications+.2+"
                                                                                                      , "{Social}.@Applications+.3+"
                                                                                                      , "{Social}.@Applications+.4+"
                                                                                                      , "{Social}.#Browsers+.1+"
                                                                                                      , "{Social}.#Browsers+.2+"
                                                                                                      , "{Social}.#Browsers+.3+"
                                                                                                      , "{Social}.#Browsers+.4+"
                                                                                                      , "{Social}.|Scratch|.+1"
                                                                                                      , "{Social}.|Scratch|.+2"
                                                                                                      , "{Social}.|Scratch|.+3"
                                                                                                      , "{Social}.|Scratch|.+4"
                                                                                                      , "{Misc}.$Terminals~.1~"
                                                                                                      , "{Misc}.$Terminals~.2~"
                                                                                                      , "{Misc}.$Terminals~.3~"
                                                                                                      , "{Misc}.$Terminals~.4~"
                                                                                                      , "{Misc}.@Applications~.1~"
                                                                                                      , "{Misc}.@Applications~.2~"
                                                                                                      , "{Misc}.@Applications~.3~"
                                                                                                      , "{Misc}.@Applications~.4~"
                                                                                                      , "{Misc}.#Browsers~.1~"
                                                                                                      , "{Misc}.#Browsers~.2~"
                                                                                                      , "{Misc}.#Browsers~.3~"
                                                                                                      , "{Misc}.#Browsers~.4~"
                                                                                                      , "{Misc}.|Scratch|.~1"
                                                                                                      , "{Misc}.|Scratch|.~2"
                                                                                                      , "{Misc}.|Scratch|.~3"
                                                                                                      , "{Misc}.|Scratch|.~4"
                                                                                                      , "{Hacking}.$Terminals>"
                                                                                                      , "{Hacking}.@Applications>"
                                                                                                      , "{Hacking}.#Browsers"
                                                                                                      , "{Hacking}.|Scratch|"
                                                                                                      , "{Programming}.$Terminals"
                                                                                                      , "{Programming}.@Applications"
                                                                                                      , "{Programming}.#Browsers"
                                                                                                      , "{Programming}.|Scratch|"
                                                                                                      , "{Media}.$Terminals"
                                                                                                      , "{Media}.@Applications"
                                                                                                      , "{Media}.#Browsers"
                                                                                                      , "{Media}.|Scratch|"
                                                                                                      , "{Social}.$Terminals"
                                                                                                      , "{Social}.@Applications"
                                                                                                      , "{Social}.#Browsers"
                                                                                                      , "{Social}.|Scratch|"
                                                                                                      , "{Misc}.$Terminals"
                                                                                                      , "{Misc}.@Applications"
                                                                                                      , "{Misc}.#Browsers"
                                                                                                      , "{Misc}.|Scratch|"
                                                                                                      ])
  , ("M-M1-`", addName "Switch to Next Programming Page"      $ moveTo Prev $ hiddenWS :&: ignoringWSs [ "{Hacking}.$Terminals>.1>"
                                                                                                      , "{Hacking}.$Terminals>.2>"
                                                                                                      , "{Hacking}.$Terminals>.3>"
                                                                                                      , "{Hacking}.$Terminals>.4>"
                                                                                                      , "{Hacking}.@Applications>.1>"
                                                                                                      , "{Hacking}.@Applications>.2>"
                                                                                                      , "{Hacking}.@Applications>.3>"
                                                                                                      , "{Hacking}.@Applications>.4>"
                                                                                                      , "{Hacking}.#Browsers>.1>"
                                                                                                      , "{Hacking}.#Browsers>.2>"
                                                                                                      , "{Hacking}.#Browsers>.3>"
                                                                                                      , "{Hacking}.#Browsers>.4>"
                                                                                                      , "{Hacking}.|Scratch|.1>"
                                                                                                      , "{Hacking}.|Scratch|.2>"
                                                                                                      , "{Hacking}.|Scratch|.3>"
                                                                                                      , "{Hacking}.|Scratch|.4>"
                                                                                                      , "{Programming}.$Terminals<.1<"
                                                                                                      , "{Programming}.$Terminals<.2<"
                                                                                                      , "{Programming}.$Terminals<.3<"
                                                                                                      , "{Programming}.$Terminals<.4<"
                                                                                                      , "{Programming}.@Applications<.1<"
                                                                                                      , "{Programming}.@Applications<.2<"
                                                                                                      , "{Programming}.@Applications<.3<"
                                                                                                      , "{Programming}.@Applications<.4<"
                                                                                                      , "{Programming}.#Browsers<.1<"
                                                                                                      , "{Programming}.#Browsers<.2<"
                                                                                                      , "{Programming}.#Browsers<.3<"
                                                                                                      , "{Programming}.#Browsers<.4<"
                                                                                                      , "{Programming}.|Scratch|.<1"
                                                                                                      , "{Programming}.|Scratch|.<2"
                                                                                                      , "{Programming}.|Scratch|.<3"
                                                                                                      , "{Programming}.|Scratch|.<4"
                                                                                                      , "{Media}.$Terminals!.1!"
                                                                                                      , "{Media}.$Terminals!.2!"
                                                                                                      , "{Media}.$Terminals!.3!"
                                                                                                      , "{Media}.$Terminals!.4!"
                                                                                                      , "{Media}.@Applications!.1!"
                                                                                                      , "{Media}.@Applications!.2!"
                                                                                                      , "{Media}.@Applications!.3!"
                                                                                                      , "{Media}.@Applications!.4!"
                                                                                                      , "{Media}.#Browsers!.1!"
                                                                                                      , "{Media}.#Browsers!.2!"
                                                                                                      , "{Media}.#Browsers!.3!"
                                                                                                      , "{Media}.#Browsers!.4!"
                                                                                                      , "{Media}.|Scratch|.!1"
                                                                                                      , "{Media}.|Scratch|.!2"
                                                                                                      , "{Media}.|Scratch|.!3"
                                                                                                      , "{Media}.|Scratch|.!4"
                                                                                                      , "{Social}.$Terminals+.1+"
                                                                                                      , "{Social}.$Terminals+.2+"
                                                                                                      , "{Social}.$Terminals+.3+"
                                                                                                      , "{Social}.$Terminals+.4+"
                                                                                                      , "{Social}.@Applications+.1+"
                                                                                                      , "{Social}.@Applications+.2+"
                                                                                                      , "{Social}.@Applications+.3+"
                                                                                                      , "{Social}.@Applications+.4+"
                                                                                                      , "{Social}.#Browsers+.1+"
                                                                                                      , "{Social}.#Browsers+.2+"
                                                                                                      , "{Social}.#Browsers+.3+"
                                                                                                      , "{Social}.#Browsers+.4+"
                                                                                                      , "{Social}.|Scratch|.+1"
                                                                                                      , "{Social}.|Scratch|.+2"
                                                                                                      , "{Social}.|Scratch|.+3"
                                                                                                      , "{Social}.|Scratch|.+4"
                                                                                                      , "{Misc}.$Terminals~.1~"
                                                                                                      , "{Misc}.$Terminals~.2~"
                                                                                                      , "{Misc}.$Terminals~.3~"
                                                                                                      , "{Misc}.$Terminals~.4~"
                                                                                                      , "{Misc}.@Applications~.1~"
                                                                                                      , "{Misc}.@Applications~.2~"
                                                                                                      , "{Misc}.@Applications~.3~"
                                                                                                      , "{Misc}.@Applications~.4~"
                                                                                                      , "{Misc}.#Browsers~.1~"
                                                                                                      , "{Misc}.#Browsers~.2~"
                                                                                                      , "{Misc}.#Browsers~.3~"
                                                                                                      , "{Misc}.#Browsers~.4~"
                                                                                                      , "{Misc}.|Scratch|.~1"
                                                                                                      , "{Misc}.|Scratch|.~2"
                                                                                                      , "{Misc}.|Scratch|.~3"
                                                                                                      , "{Misc}.|Scratch|.~4"
                                                                                                      , "{Hacking}.$Terminals>"
                                                                                                      , "{Hacking}.@Applications>"
                                                                                                      , "{Hacking}.#Browsers"
                                                                                                      , "{Hacking}.|Scratch|"
                                                                                                      , "{Programming}.$Terminals"
                                                                                                      , "{Programming}.@Applications"
                                                                                                      , "{Programming}.#Browsers"
                                                                                                      , "{Programming}.|Scratch|"
                                                                                                      , "{Media}.$Terminals"
                                                                                                      , "{Media}.@Applications"
                                                                                                      , "{Media}.#Browsers"
                                                                                                      , "{Media}.|Scratch|"
                                                                                                      , "{Social}.$Terminals"
                                                                                                      , "{Social}.@Applications"
                                                                                                      , "{Social}.#Browsers"
                                                                                                      , "{Social}.|Scratch|"
                                                                                                      , "{Misc}.$Terminals"
                                                                                                      , "{Misc}.@Applications"
                                                                                                      , "{Misc}.#Browsers"
                                                                                                      , "{Misc}.|Scratch|"
                                                                                                      , "{Programming}"
                                                                                                      , "{Media}"
                                                                                                      , "{Social}"
                                                                                                      , "{Misc}"
                                                                                                      ])
  , ("M-`", addName "Switch to Next Programming Page"        $ moveTo Next $ hiddenWS :&: ignoringWSs [ "{Hacking}.$Terminals>.1>"
                                                                                                      , "{Hacking}.$Terminals>.2>"
                                                                                                      , "{Hacking}.$Terminals>.3>"
                                                                                                      , "{Hacking}.$Terminals>.4>"
                                                                                                      , "{Hacking}.@Applications>.1>"
                                                                                                      , "{Hacking}.@Applications>.2>"
                                                                                                      , "{Hacking}.@Applications>.3>"
                                                                                                      , "{Hacking}.@Applications>.4>"
                                                                                                      , "{Hacking}.#Browsers>.1>"
                                                                                                      , "{Hacking}.#Browsers>.2>"
                                                                                                      , "{Hacking}.#Browsers>.3>"
                                                                                                      , "{Hacking}.#Browsers>.4>"
                                                                                                      , "{Hacking}.|Scratch|.1>"
                                                                                                      , "{Hacking}.|Scratch|.2>"
                                                                                                      , "{Hacking}.|Scratch|.3>"
                                                                                                      , "{Hacking}.|Scratch|.4>"
                                                                                                      , "{Programming}.$Terminals<.1<"
                                                                                                      , "{Programming}.$Terminals<.2<"
                                                                                                      , "{Programming}.$Terminals<.3<"
                                                                                                      , "{Programming}.$Terminals<.4<"
                                                                                                      , "{Programming}.@Applications<.1<"
                                                                                                      , "{Programming}.@Applications<.2<"
                                                                                                      , "{Programming}.@Applications<.3<"
                                                                                                      , "{Programming}.@Applications<.4<"
                                                                                                      , "{Programming}.#Browsers<.1<"
                                                                                                      , "{Programming}.#Browsers<.2<"
                                                                                                      , "{Programming}.#Browsers<.3<"
                                                                                                      , "{Programming}.#Browsers<.4<"
                                                                                                      , "{Programming}.|Scratch|.<1"
                                                                                                      , "{Programming}.|Scratch|.<2"
                                                                                                      , "{Programming}.|Scratch|.<3"
                                                                                                      , "{Programming}.|Scratch|.<4"
                                                                                                      , "{Media}.$Terminals!.1!"
                                                                                                      , "{Media}.$Terminals!.2!"
                                                                                                      , "{Media}.$Terminals!.3!"
                                                                                                      , "{Media}.$Terminals!.4!"
                                                                                                      , "{Media}.@Applications!.1!"
                                                                                                      , "{Media}.@Applications!.2!"
                                                                                                      , "{Media}.@Applications!.3!"
                                                                                                      , "{Media}.@Applications!.4!"
                                                                                                      , "{Media}.#Browsers!.1!"
                                                                                                      , "{Media}.#Browsers!.2!"
                                                                                                      , "{Media}.#Browsers!.3!"
                                                                                                      , "{Media}.#Browsers!.4!"
                                                                                                      , "{Media}.|Scratch|.!1"
                                                                                                      , "{Media}.|Scratch|.!2"
                                                                                                      , "{Media}.|Scratch|.!3"
                                                                                                      , "{Media}.|Scratch|.!4"
                                                                                                      , "{Social}.$Terminals+.1+"
                                                                                                      , "{Social}.$Terminals+.2+"
                                                                                                      , "{Social}.$Terminals+.3+"
                                                                                                      , "{Social}.$Terminals+.4+"
                                                                                                      , "{Social}.@Applications+.1+"
                                                                                                      , "{Social}.@Applications+.2+"
                                                                                                      , "{Social}.@Applications+.3+"
                                                                                                      , "{Social}.@Applications+.4+"
                                                                                                      , "{Social}.#Browsers+.1+"
                                                                                                      , "{Social}.#Browsers+.2+"
                                                                                                      , "{Social}.#Browsers+.3+"
                                                                                                      , "{Social}.#Browsers+.4+"
                                                                                                      , "{Social}.|Scratch|.+1"
                                                                                                      , "{Social}.|Scratch|.+2"
                                                                                                      , "{Social}.|Scratch|.+3"
                                                                                                      , "{Social}.|Scratch|.+4"
                                                                                                      , "{Misc}.$Terminals~.1~"
                                                                                                      , "{Misc}.$Terminals~.2~"
                                                                                                      , "{Misc}.$Terminals~.3~"
                                                                                                      , "{Misc}.$Terminals~.4~"
                                                                                                      , "{Misc}.@Applications~.1~"
                                                                                                      , "{Misc}.@Applications~.2~"
                                                                                                      , "{Misc}.@Applications~.3~"
                                                                                                      , "{Misc}.@Applications~.4~"
                                                                                                      , "{Misc}.#Browsers~.1~"
                                                                                                      , "{Misc}.#Browsers~.2~"
                                                                                                      , "{Misc}.#Browsers~.3~"
                                                                                                      , "{Misc}.#Browsers~.4~"
                                                                                                      , "{Misc}.|Scratch|.~1"
                                                                                                      , "{Misc}.|Scratch|.~2"
                                                                                                      , "{Misc}.|Scratch|.~3"
                                                                                                      , "{Misc}.|Scratch|.~4"
                                                                                                      , "{Hacking}.$Terminals>"
                                                                                                      , "{Hacking}.@Applications"
                                                                                                      , "{Hacking}.#Browsers"
                                                                                                      , "{Hacking}.|Scratch|"
                                                                                                      , "{Programming}.$Terminals"
                                                                                                      , "{Programming}.@Applications"
                                                                                                      , "{Programming}.#Browsers"
                                                                                                      , "{Programming}.|Scratch|"
                                                                                                      , "{Media}.$Terminals"
                                                                                                      , "{Media}.@Applications"
                                                                                                      , "{Media}.#Browsers"
                                                                                                      , "{Media}.|Scratch|"
                                                                                                      , "{Social}.$Terminals"
                                                                                                      , "{Social}.@Applications"
                                                                                                      , "{Social}.#Browsers"
                                                                                                      , "{Social}.|Scratch|"
                                                                                                      , "{Misc}.$Terminals"
                                                                                                      , "{Misc}.@Applications"
                                                                                                      , "{Misc}.#Browsers"
                                                                                                      , "{Misc}.|Scratch|"
                                                                                                      ])

  ]

  -- Multimedia Keys
  ^++^ subKeys "Multimedia keys"
  [ ("<XF86AudioPlay>", addName "mocp play"           $ spawn "mocp --play")
  , ("<XF86AudioPrev>", addName "mocp next"           $ spawn "mocp --previous")
  , ("<XF86AudioNext>", addName "mocp prev"           $ spawn "mocp --next")
  , ("<XF86AudioMute>", addName "Toggle audio mute"   $ spawn "amixer set Master toggle")
  , ("<XF86AudioLowerVolume>", addName "Lower vol"    $ spawn "amixer set Master 5%- unmute")
  , ("<XF86AudioRaiseVolume>", addName "Raise vol"    $ spawn "amixer set Master 5%+ unmute")
  , ("<XF86HomePage>", addName "Open home page"       $ spawn (myBrowser ++ " https://www.youtube.com/c/DistroTube"))
  , ("<XF86Search>", addName "Web search (dmscripts)" $ spawn "dm-websearch")
  , ("<XF86Mail>", addName "Email client"             $ runOrRaise "thunderbird" (resource =? "thunderbird"))
  , ("<XF86Calculator>", addName "Calculator"         $ runOrRaise "qalculate-gtk" (resource =? "qalculate-gtk"))
  , ("<XF86Eject>", addName "Eject /dev/cdrom"        $ spawn "eject /dev/cdrom")
  , ("<Print>", addName "Take screenshot (dmscripts)" $ spawn "dm-maim")
  ]

  -- The following lines are needed for named scratchpads.
    where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
          nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

main :: IO ()
main = do
  -- Launching three instances of xmobar on their monitors.
--  xmproc0 <- spawnPipe ("xmobar -x 0 $HOME/.config/xmobar/" ++ colorScheme ++ "-xmobarrc")
--  xmproc1 <- spawnPipe ("xmobar -x 1 $HOME/.config/xmobar/" ++ colorScheme ++ "-xmobarrc")
--  xmproc2 <- spawnPipe ("xmobar -x 2 $HOME/.config/xmobar/" ++ colorScheme ++ "-xmobarrc")
  -- the xmonad, ya know...what the WM is named after!
  xmonad $ addDescrKeys' ((mod4Mask, xK_F1), showKeybindings) myKeys $ ewmh $ docks $ dynamicProjects projects def
    { manageHook         = myManageHook <+> manageDocks
    --, handleEventHook    = docks
                           -- Uncomment this line to enable fullscreen support on things like YouTube/Netflix.
                           -- This works perfect on SINGLE monitor systems. On multi-monitor systems,
                           -- it adds a border around the window if screen does not have focus. So, my solution
                           -- is to use a keybinding to toggle fullscreen noborders instead.  (M-<Space>)
                           -- <+> fullscreenEventHook
    , modMask            = myModMask
    , terminal           = myTerminal
    , startupHook        = myStartupHook
    , layoutHook         = minimize . BW.boringWindows $ showWName' myShowWNameTheme $ myLayoutHook
    , workspaces         = TS.toWorkspaces myWorkspaces
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormColor
    , focusedBorderColor = myFocusColor
--    , logHook = dynamicLogWithPP $  filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
--        { ppOutput = \x -> hPutStrLn xmproc0 x   -- xmobar on monitor 1
--                        >> hPutStrLn xmproc1 x   -- xmobar on monitor 2
--                        >> hPutStrLn xmproc2 x   -- xmobar on monitor 3
--        , ppCurrent = xmobarColor color06 "" . wrap
--                      ("<box type=Bottom width=2 mb=2 color=" ++ color06 ++ ">") "</box>"
--          -- Visible but not current workspace
--        , ppVisible = xmobarColor color06 "" . clickable
--          -- Hidden workspace
--        , ppHidden = xmobarColor color05 "" . wrap
--                     ("<box type=Top width=2 mt=2 color=" ++ color05 ++ ">") "</box>" . --clickable
--          -- Hidden workspaces (no windows)
--        , ppHiddenNoWindows = xmobarColor color05 ""  . clickable
--          -- Title of active window
--        , ppTitle = xmobarColor color16 "" . shorten 60
--          -- Separator character
--        , ppSep =  "<fc=" ++ color09 ++ "> <fn=1>|</fn> </fc>"
--          -- Urgent workspace
--        , ppUrgent = xmobarColor color02 "" . wrap "!" "!"
--          -- Adding # of windows on current workspace to the bar
--        , ppExtras  = [windowCount]
--          -- order of things in xmobar
--        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
--        }
    }
