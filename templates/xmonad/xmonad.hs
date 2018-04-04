import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Util.EZConfig

main =
  xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

myTerminal = "urxvt -e tmux -2"

myWorkspaces =
  ["1 \xf0ac", "2 \xf044", "3 \xf120", "4 \xf15c", "5", "6", "7 \xf3fe", "8 \xf1ab", "9 \xf013"]

-- Status Bar
myBar = "xmobar"
myPP = xmobarPP {ppCurrent = xmobarColor "#429942" "" . wrap "[" "]"}
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myLayout = mkToggle (single FULL) (tiled ||| Mirror tiled)
  where tiled = Tall nmaster delta ratio
        nmaster = 1
        ratio = 0.618
        delta = 3/100

myModMask = mod1Mask
myKeys =
  [ ((myModMask, xK_o), toggleWS)
  , ((myModMask, xK_f), sendMessage $ Toggle FULL)
  , ((myModMask .|. shiftMask, xK_l), spawn "i3lock-next >/dev/null 2>&1")
  ]

myConfig =
  def {terminal = myTerminal, layoutHook = myLayout, workspaces = myWorkspaces} `additionalKeys`
  myKeys
