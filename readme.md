[![Hackage](https://img.shields.io/hackage/v/xmobar.svg)](http://hackage.haskell.org/package/xmobar)
[![Build Status](https://travis-ci.org/jaor/xmobar.svg?branch=master)](https://travis-ci.org/jaor/xmobar)

# About

Xmobar is a minimalistic status bar. It was originally designed and
implemented by Andrea Rossato to work with [xmonad], but it is
actually usable with any window manager.

Xmobar was inspired by the [Ion3] status bar, and supports similar
features, like dynamic color management, icons, output templates, and
extensibility through plugins.

These are two xmobar instances using the author's configuration:

![top](https://gitlab.com/jaor/xmobar-config/raw/master/img/xmobar-top.png)

![bottom](https://gitlab.com/jaor/xmobar-config/raw/master/img/xmobar-bottom.png)

and [this one](doc/xmobar-xmonad.png) is a full desktop with [xmonad]
and, again, two instances of xmobar.

[xmonad]: http://xmonad.org
[Ion3]: http://tuomov.iki.fi/software/

# Bug Reports

To submit bug reports you can use the [bug tracker over at
Github](https://github.com/jaor/xmobar/issues).

# Installation

## Using cabal-install

Xmobar is available from [Hackage], and you can install it using
`cabal-install`:

        cabal install xmobar

Xmobar versions >= 0.27 require GHC version >= 8.0.2.  Due to an
intermittent bug in GHC, we recommend using either GHC 8.0.2, 8.2.2 or
8.6.

See below for a list of optional compilation flags that will enable
some optional plugins. For instance, to install xmobar with all the
bells and whistles, use:

        cabal install xmobar --flags="all_extensions"

## From source

If you don't have `cabal-install` installed, you can get xmobar's
source code in a variety of ways:

  - From [Hackage]. Just download the latest release from xmobar's
    hackage page.
  - From [Github]. You can also obtain a tarball in [Github's
    downloads page]. You'll find there links to each tagged release.
  - From the bleeding edge repo. If you prefer to live dangerously,
    just get the latest and greatest (and buggiest, I guess) using
    git:

        git clone git://github.com/jaor/xmobar


[Github's downloads page]: https://github.com/jaor/xmobar/downloads

If you have cabal installed, you can now use it from within xmobar's
source tree:

        cabal install -fall_extensions


There is also a barebones `stack.yaml` file that will allow you to
build the xmobar executable with stances of the form:

        stack install --flag xmobar:all_extensions


## Optional features

You can configure xmobar to include some optional plugins and
features, which are not compiled by default. To that end, you need to
add one or more flags to either the cabal install command or the
configure setup step, as shown in the examples above.

Extensions need additional libraries (listed below) that will be
automatically downloaded and installed if you're using cabal install.
Otherwise, you'll need to install them yourself.

- `with_dbus` Enables support for DBUS by making xmobar to publish a
  service on the session bus.  Requires the [dbus] package.

- `with_threaded` Uses GHC's threaded runtime.  Use this option if
  xmobar enters a high-CPU regime right after starting.

- `with_utf8` UTF-8 support. Requires the [utf8-string] package.

- `with_xft` Antialiased fonts. Requires the [X11-xft] package. This
  option automatically enables UTF-8.  To use XFT fonts you need to
  use the `xft:` prefix in the `font` configuration option. For
  instance:

        font = "xft:Times New Roman-10:italic"

  Or to have fallback fonts, just separate them by commas:

        font = "xft:Open Sans:size=9,WenQuanYi Zen Hei:size=9"

- `with_mpd` Enables support for the [MPD] daemon. Requires the
  [libmpd] package.

- `with_mpris` Enables support for MPRIS v1/v2 protocol.  Requires the
  [dbus] and [text] packages.

- `with_inotify` Support for inotify in modern Linux kernels. This
  option is needed for the MBox and Mail plugins to work. Requires the
  [hinotify] package.

- `with_nl80211` Support for wireless cards on Linux via nl80211 (all
   upstream drivers). Enables the Wireless plugin. Requires [netlink]
   and [cereal] packages.

- `with_iwlib` Support for wireless cards via Wext ioctls
   (deprecated). Enables the Wireless plugin. No Haskell library is
   required, but you will need the [iwlib] C library and headers in your
   system (e.g., install `libiw-dev` in Debian-based systems or
   `wireless_tools` on Arch Linux). Conflicts with `with_nl80211`.

- `with_alsa` Support for ALSA sound cards. Enables the Volume
   plugin. Requires the [alsa-mixer] package.  To install the latter,
   you'll need the [libasound] C library and headers in your system
   (e.g., install `libasound2-dev` in Debian-based systems).

- `with_datezone` Support for other timezones. Enables the DateZone
   plugin.  Requires [timezone-olson] and [timezone-series] package.

- `with_xpm` Support for xpm image file format. This will allow loading
  .xpm files in `<icon>`.  Requires the [libXpm] C library.

- `with_uvmeter` Enables UVMeter plugin. The plugin shows UV data for
   Australia.

- `with_weather` Support to display weather information. Enables
   Weather plugin.

- `all_extensions` Enables all the extensions above.

# Running xmobar

You can now run xmobar with:

        xmobar /path/to/config &

or

        xmobar &

if you have the default configuration file saved as
`$XDG\_CONFIG\_HOME/xmobar/xmobarrc` (defaulting to
`~/.config/xmobar/xmobarrc`), or `~/.xmobarrc`.

## Signal Handling

Since 0.14 xmobar reacts to SIGUSR1 and SIGUSR2:

- After receiving SIGUSR1 xmobar moves its position to the next screen.

- After receiving SIGUSR2 xmobar repositions itself on the current screen.

# Configuration

## Quick Start

See [examples/xmobar.config] for an example.

[examples/xmobar.config]: http://github.com/jaor/xmobar/raw/master/examples/xmobar.config

For the output template:

- `%command%` will execute command and print the output. The output
  may contain markups to change the characters' color.

- `<fc=#FF0000>string</fc>` will print `string` with `#FF0000` color
  (red).

- `<fn=1>string</fn>` will print `string` with the first font from
  `additionalFonts`.  The index `0` corresponds to the standard font.

- `<icon=/path/to/icon.xbm/>` will insert the given bitmap. XPM image
  format is also supported when compiled with `--flags="with_xpm"`.

- ```<action=`command` button=12345>``` will execute given command
  when clicked with specified buttons. If not specified, button is
  equal to 1 (left mouse button). Using old syntax (without backticks
  surrounding `command`) will result in `button` attribute being
  ignored.

- `<raw=len:str/>` allows the encapsulation of arbitrary text `str` (which
  must be `len` `Char`s long, where `len` is encoded as a decimal sequence).
  Careful use of this and `UnsafeStdinReader`, for example, permits window
  managers to feed xmobar strings with `<action>` tags mixed with un-trusted
  content (e.g. window titles).  For example, if xmobar is invoked as

        xmobar -c "[Run UnsafeStdinReader]" -t "%UnsafeStdinReader%"

  and receives on standard input the line

        <action=`echo test` button=1><raw=41:<action=`echo mooo` button=1>foo</action>/></action>`

  then it will display the text ```<action=`echo mooo` button=1>foo</action>```,
  which, when clicked, will cause `test` to be echoed.

Other configuration options:

- `font` Name of the font to be used. Use the `xft:` prefix for XFT
  fonts.

- `additionalFonts` Haskell-style list of fonts to be used with the
  `fn`-template.  Use the `xft:` prefix for XFT fonts.  See also
  `textOffsets` below.

- `bgColor` Background color.

- `fgColor` Default font color.

- `alpha` The transparency.  0 is transparent, 255 is opaque.

- `position` Top, TopP, TopW, TopSize, Bottom, BottomP, BottomW,
  BottomSize or Static (with x, y, width and height).

  TopP and BottomP take 2 arguments: left padding and right padding.

  TopW and BottomW take 2 arguments: an alignment parameter (L for
  left, C for centered, R for Right) and an integer for the percentage
  width xmobar window will have in respect to the screen width.

  TopSize and BottomSize take 3 arguments: an alignment parameter, an
  integer for the percentage width, and an integer for the minimum
  pixel height that the xmobar window will have.

  For example:

          position = BottomW C 75

  to place xmobar at the bottom, centered with the 75% of the screen
  width.  Or

          position = BottomP 120 0

  to place xmobar at the bottom, with 120 pixel indent of the left.
  Or

          position = Static { xpos = 0 , ypos = 0, width = 1024, height = 15 }

  or

          position = Top

- `textOffset` The vertical offset, in pixels, for the text baseline.
   If negative or not given, xmobar will try to center text
   vertically.

- `textOffsets` A list of vertical offsets, in pixels, for the text
   baseline, to be used with the each of the fonts in
   `additionalFonts` (if any).  If negative or not given, xmobar will
   try to center text vertically for that font.

- `iconOffset` The vertical offset, in pixels, for icons bottom line.
   If negative or not given, xmobar will try to center icons
   vertically.

- `lowerOnStart` When True the window is sent the bottom of the window
  stack initially.

- `hideOnStart` When set to True the window is initially not mapped,
  i.e. hidden. It then can be toggled manually (for example using the
  dbus interface) or automatically (by a plugin) to make it reappear.

- `allDesktops` When set to True (the default), xmobar will tell the
  window manager explicitly to be shown in all desktops, by setting
  `_NET_WM_DESKTOP` to 0xffffffff.

- `overrideRedirect` If you're running xmobar in a tiling window
  manager, you might need to set this option to `False` so that it
  behaves as a docked application.  Defaults to `True`.

- `pickBroadest` When multiple displays are available, xmobar will
  choose by default the first one to place itself.  With this flag set
  to `True` (the default is `False`) it will choose the broadest one
  instead.

- `persistent` When True the window status is fixed i.e. hiding or
  revealing is not possible. This option can be toggled at
  runtime. Defaults to False.

- `border` TopB, TopBM, BottomB, BottomBM, FullB, FullBM or NoBorder
  (default).

  TopB, BottomB, FullB take no arguments, and request drawing a border
  at the top, bottom or around xmobar's window, respectively.

  TopBM, BottomBM, FullBM take an integer argument, which is the
  margin, in pixels, between the border of the window and the drawn
  border.

- `borderColor` Border color.

- `borderWidth` Border width in pixels.

- `iconRoot` Root folder where icons are stored. For <icon=path/> if
  path start with `"/"`, `"./"` or `"../"` it is interpreted as it is.
  Otherwise it will have `iconRoot ++ "/"` prepended to it. Default is
  `"."`.

- `commands` For setting the options of the programs to run
  (optional).

- `sepChar` The character to be used for indicating commands in the
  output template (default '%').

- `alignSep` a 2 character string for aligning text in the output
  template. The text before the first character will be align to left,
  the text in between the 2 characters will be centered, and the text
  after the second character will be align to the right.

- `template` The output template.

- `wmClass` The value for the window's X11 WM_CLASS property.
  Defaults to "xmobar".

- `wmName` The value for the window's X11 WM_NAME property.  Defaults
  to "xmobar".

### Running xmobar with i3status

xmobar can be used to display information generated by [i3status], a
small program that gathers system information and outputs it in
formats suitable for being displayed by the dzen2 status bar, wmii's
status bar or xmobar's `StdinReader`.  See [i3status manual] for
further details.

### Dynamically sizing xmobar

See [this idea] by Jonas Camillus Jeppensen for a way of adapting
dynamically xmobar's size and run it alongside a system tray widget
such as trayer or stalonetray (although the idea is not limited to
trays, really).  For your convenience, there is a version of Jonas'
script in [examples/padding-icon.sh](./examples/padding-icon.sh).

[this idea]: https://github.com/jaor/xmobar/issues/239#issuecomment-233206552

## Command Line Options

xmobar can be either configured with a configuration file or with
command line options. In the second case, the command line options
will overwrite the corresponding options set in the configuration
file.

Example:

    xmobar -B white -a right -F blue -t '%LIPB%' -c '[Run Weather "LIPB" [] 36000]'

This is the list of command line options (the output of
xmobar --help):

    Usage: xmobar [OPTION...] [FILE]
    Options:
      -h, -?        --help                 This help
      -V            --version              Show version information
      -v            --verbose              Emit verbose debugging messages
      -r            --recompile            Force recompilation (for Haskell FILE)
      -f font name  --font=font name       Font name
      -w class      --wmclass=class        X11 WM_CLASS property
      -n name       --wmname=name          X11 WM_NAME property
      -B bg color   --bgcolor=bg color     Background color. Default black
      -F fg color   --fgcolor=fg color     Foreground color. Default grey
      -A alpha      --alpha=alpha          Transparency: 0 is transparent
                                           and 255 (the default) is opaque
      -o            --top                  Place xmobar at the top of the screen
      -b            --bottom               Place xmobar at the bottom of the screen
      -p            --position=position    Specify position, same as in config file
      -d            --dock                 Try to start xmobar as a dock
      -a alignsep   --alignsep=alignsep    Separators for left, center and right text
                                           alignment. Default: '}{'
      -s char       --sepchar=char         Character used to separate commands in
                                           the output template. Default '%'
      -t template   --template=template    Output template
      -i path       --iconroot=path        Default directory for icon pattern files
      -c commands   --commands=commands    List of commands to be executed
      -C command    --add-command=command  Add to the list of commands to be executed
      -x screen     --screen=screen        On which X screen number to start

    Mail bug reports and suggestions to <mail@jao.io>

## The DBus Interface

When compiled with the optional `with_dbus` flag, xmobar can be
controlled over dbus. All signals defined in [src/Signal.hs] as `data
SignalType` can now be sent over dbus to xmobar.  Due to current
limitations of the implementation only one process of xmobar can
acquire the dbus. This is handled on a first-come-first-served basis,
meaning that the first process will get the dbus interface. Other
processes will run without further problems, yet have no dbus
interface.

[src/Signal.hs]: https://github.com/jaor/xmobar/blob/master/src/Xmobar/System/Signal.hs

- Bus Name: `org.Xmobar.Control`
- Object Path: `/org/Xmobar/Control`
- Member Name: Any of SignalType, e.g. `string:Reveal`
- Interface Name: `org.Xmobar.Control`

An example using the `dbus-send` command line utility:

        dbus-send \
            --session \
            --dest=org.Xmobar.Control \
            --type=method_call \
            --print-reply \
            '/org/Xmobar/Control' \
            org.Xmobar.Control.SendSignal \
            "string:Toggle 0"

It is also possible to send multiple signals at once:

        # send to another screen, reveal and toggle the persistent flag
        dbus-send [..] \
            "string:ChangeScreen 0" "string:Reveal 0" "string:TogglePersistent"

The `Toggle`, `Reveal`, and `Hide` signals take an additional integer
argument that denotes an initial delay, in tenths of a second, before
the command takes effect.

### Example for using the DBus IPC interface with XMonad

Bind the key which should {,un}map xmobar to a dummy value. This is necessary
for {,un}grabKey in xmonad.

    ((0, xK_Alt_L   ), return ())

Also, install `avoidStruts` layout modifier from `XMonad.Hooks.ManageDocks`

Finally, install these two event hooks (`handleEventHook` in `XConfig`)
`myDocksEventHook` is a replacement for `docksEventHook` which reacts on unmap
events as well (which `docksEventHook` doesn't).

    import qualified XMonad.Util.ExtensibleState as XS

    data DockToggleTime = DTT { lastTime :: Time } deriving (Eq, Show, Typeable)

    instance ExtensionClass DockToggleTime where
        initialValue = DTT 0

    toggleDocksHook :: Int -> KeySym -> Event -> X All
    toggleDocksHook to ks ( KeyEvent { ev_event_display = d
                                     , ev_event_type    = et
                                     , ev_keycode       = ekc
                                     , ev_time          = etime
                                     } ) =
            io (keysymToKeycode d ks) >>= toggleDocks >> return (All True)
        where
        toggleDocks kc
            | ekc == kc && et == keyPress = do
                safeSendSignal ["Reveal 0", "TogglePersistent"]
                XS.put ( DTT etime )
            | ekc == kc && et == keyRelease = do
                gap <- XS.gets ( (-) etime . lastTime )
                safeSendSignal [ "TogglePersistent"
                               , "Hide " ++ show (if gap < 400 then to else 0)
                               ]
            | otherwise = return ()

        safeSendSignal s = catchX (io $ sendSignal s) (return ())
        sendSignal    = withSession . callSignal
        withSession mc = connectSession >>= \c -> callNoReply c mc >> disconnect c
        callSignal :: [String] -> MethodCall
        callSignal s = ( methodCall
                         ( objectPath_    "/org/Xmobar/Control" )
                         ( interfaceName_ "org.Xmobar.Control"  )
                         ( memberName_    "SendSignal"          )
                       ) { methodCallDestination = Just $ busName_ "org.Xmobar.Control"
                         , methodCallBody        = map toVariant s
                         }

    toggleDocksHook _ _ _ = return (All True)

    myDocksEventHook :: Event -> X All
    myDocksEventHook e = do
        when (et == mapNotify || et == unmapNotify) $
            whenX ((not `fmap` (isClient w)) <&&> runQuery checkDock w) refresh
        return (All True)
        where w  = ev_window e
              et = ev_event_type e


## The Output Template

The output template must contain at least one command. xmobar will
parse the template and will search for the command to be executed in
the `commands` configuration option. First an `alias` will be searched
(plugins such as Weather or Network have default aliases, see below).
After that, the command name will be tried. If a command is found, the
arguments specified in the `commands` list will be used.

If no command is found in the `commands` list, xmobar will ask the
operating system to execute a program with the name found in the
template. If the execution is not successful an error will be
reported.

It's possible to insert in the global templates icon directives of the
form:

     <icon=/path/to/bitmap.xbm/>

which will produce the expected result. Accepted image formats are XBM
and XPM (when `with_xpm` flag is enabled). If path does not start with
`"/"`, `"./"`, `"../"` it will have `iconRoot ++ "/"` prepended to it.

It's also possible to use action directives of the form:

     <action=`command` button=12345>

which will be executed when clicked on with specified mouse buttons. This tag
can be nested, allowing different commands to be run depending on button clicked.

## The `commands` Configuration Option

The `commands` configuration option is a list of commands information
and arguments to be used by xmobar when parsing the output template.
Each member of the list consists in a command prefixed by the `Run`
keyword. Each command has arguments to control the way xmobar is going
to execute it.

The option consists in a list of commands separated by a comma and
enclosed by square parenthesis.

Example:

    [Run Memory ["-t","Mem: <usedratio>%"] 10, Run Swap [] 10]

to run the Memory monitor plugin with the specified template, and the
swap monitor plugin, with default options, every second.  And here's
an example of a template for the commands above using an icon:

    template="<icon=/home/jao/.xmobar/mem.xbm/><memory> <swap>"

This example will run "xclock" command when date is clicked:

    template="<action=`xclock`>%date%</action>

The only internal available command is `Com` (see below Executing
External Commands). All other commands are provided by plugins. xmobar
comes with some plugins, providing a set of system monitors, a
standard input reader, an Unix named pipe reader, a configurable date
plugin, and much more: we list all available plugins below.

Other commands can be created as plugins with the Plugin
infrastructure. See below.

## System Monitor Plugins

This is the description of the system monitor plugins available in
xmobar.  Some of them are only installed when an optional build option
is set: we mention that fact, when needed, in their description.

Each monitor has an `alias` to be used in the output template.
Monitors have default aliases.  The sections below describe every
monitor in turn, but before we provide a list of the configuration
options (or *monitor arguments*) they all share.

### Icon patterns

Some monitors allow usage of strings that depend on some integer value
from 0 to 8 by replacing all occurrences of `"%%"` with it
(i.e. `"<icon=/path/to/icon_%%.xpm/>"` will be interpreted
as `"<icon=/path/to/icon_3.xpm/>"` when the value is `3`, also `"%"` is interpreted
as `"%"`, `"%%"` as `"3"`, `"%%%"` as `"3%"`, `"%%%%"` as `"33"` and so on). Essentially
it allows to replace vertical bars with custom icons. For example,

    Run Brightness
      [ "-t", "<ipat>"
      , "--"
      , "--brightness-icon-pattern", "<icon=bright_%%.xpm/>"
      ] 30

Will display `bright_0.xpm` to `bright_8.xpm` depending on current brightness
value.

### Default Monitor Arguments

Monitors accept a common set of arguments, described in the first
subsection below.  In addition, some monitors accept additional options
that are specific to them.  When specifying the list of arguments in
your configuration, the common options come first, followed by "--",
followed by any monitor-specific options.

These are the options available for all monitors below:

- `-t` _string_  Output template
    - Template for the monitor output. Field names must be enclosed
      between pointy brackets (`<foo>`) and will be substituted by the
      computed values. You can also specify the foreground (and
      optionally, background) color for a region by bracketing it
      between `<fc=fgcolor>` (or `<fc=fgcolor,bgcolor>`) and
      `</fc>`. The rest of the template is output verbatim.
    - Long option: `--template`
    - Default value: per monitor (see above).
- `-H` _number_ The high threshold.
    - Numerical values higher than _number_ will be displayed with the
      color specified by `-h` (see below).
    - Long option: `--High`
    - Default value: 66
- `-L` _number_ The low threshold.
    - Numerical values higher than _number_ and lower than the high
      threshold will be displayed with the color specified by `-n`
      (see below). Values lower than _number_ will use the `-l` color.
    - Long option: `--Low`
    - Default value: 33
- `-h` _color_  High threshold color.
    - Color for displaying values above the high threshold. _color_ can
      be either a name (e.g. "blue") or an hexadecimal RGB (e.g.
      "#FF0000").
    - Long option: `--high`
    - Default: none (use the default foreground).
- `-n` _color_  Color for 'normal' values
    - Color used for values greater than the low threshold but lower
      than the high one.
    - Long option: `--normal`
    - Default: none (use the default foreground).
- `-l` _color_  The low threshold color
    - Color for displaying values below the low threshold.
    - Long option: `--low`
    - Default: none (use the default foreground).
- `-S` _boolean_ Display optional suffixes
    - When set to a true designator ("True", "Yes" or "On"), optional
      value suffixes such as the '%' symbol or optional units will be
      displayed.
    - Long option: `--suffix`
    - Default: False.
- `-p` _number_ Percentages padding
    - Width, in number of digits, for quantities representing
      percentages. For instance `-p 3` means that all percentages
      in the monitor will be represented using 3 digits.
    - Long option: `--ppad`
    - Default value: 0 (don't pad)
- `-d` _number_ Decimal digits
    - Number of digits after the decimal period to use in float values.
    - Long option: `--ddigits`
    - Default value: 0 (display only integer part)
- `-m` _number_ Minimum field width
    - Minimum width, in number of characters, of the fields in the
      monitor template. Values whose printed representation is shorter
      than this value will be padded using the padding characters
      given by the `-c` option with the alignment specified by `-a`
      (see below).
    - Long option: `--minwidth`
    - Default: 0
- `-M` _number_ Maximum field width
    - Maximum width, in number of characters, of the fields in the
      monitor template. Values whose printed representation is longer
      than this value will be truncated.
    - Long option: `--maxwidth`
    - Default: 0 (no maximum width)
- `-e` _string_ Maximum width ellipsis
    - Ellipsis to be added to the field when it has reached its
      max width.
    - Long option: `--maxwidthellipsis`
    - Default: "" (no ellipsis)
- `-w` _number_ Fixed field width
    - All fields will be set to this width, padding or truncating as
      needed.
    - Long option: `--width`
    - Default: 0 (variable width)
- `-T` _number_ Maximum total width
    - Maximum total width of the text.
    - Long option: `--maxtwidth`
    - Default: 0 (no limit)
- `-E` _string_ Maximum total width ellipsis
    - Ellipsis to be added to the total text when it has reached
      its max width.
    - Long option: `--maxtwidthellipsis`
    - Default: "" (no ellipsis)
- `-c` _string_
    - Characters used for padding. The characters of _string_ are used
      cyclically. E.g., with `-P +- -w 6`, a field with value "foo"
      will be represented as "+-+foo".
    - Long option: `--padchars`
    - Default value: " "
- `-a` r|l Field alignment
    - Whether to use right (r) or left (l) alignment of field values
      when padding.
    - Long option: `--align`
    - Default value: r (padding to the left)
- `-b` _string_ Bar background
    - Characters used, cyclically, to draw the background of bars.
      For instance, if you set this option to "·.", an empty bar will
      look like this: `·.·.·.·.·.`
    - Long option: `--bback`
    - Default value: ":"
- `-f` _string_ Bar foreground
    - Characters used, cyclically, to draw the foreground of bars.
    - Long option: `--bfore`
    - Default value: "#"
- `-W` _number_ Bar width
    - Total number of characters used to draw bars.
    - Long option: `--bwidth`
    - Default value: 10
- `-x` _string_ N/A string
    - String to be used when the monitor is not available
    - Long option: `--nastring`
    - Default value: "N/A"

Commands' arguments must be set as a list. E.g.:

    Run Weather "EGPF" ["-t", "<station>: <tempC>C"] 36000

In this case xmobar will run the weather monitor, getting information
for the weather station ID EGPF (Glasgow Airport, as a homage to GHC)
every hour (36000 tenth of seconds), with a template that will output
something like:

    Glasgow Airport: 16.0C


### `Uptime Args RefreshRate`

- Aliases to `uptime`
- Args: default monitor arguments. The low and high
  thresholds refer to the number of days.
- Variables that can be used with the `-t`/`--template` argument:
  `days`, `hours`, `minutes`, `seconds`. The total uptime is the
  sum of all those fields. You can set the `-S` argument to "True"
  to add units to the display of those numeric fields.
- Default template: `Up: <days>d <hours>h <minutes>m`

### `Weather StationID Args RefreshRate`

- Aliases to the Station ID: so `Weather "LIPB" []` can be used in
  template as `%LIPB%`
- Thresholds refer to temperature in the selected units
- Args: default monitor arguments, plus:
  - `--weathers` _string_ : display a default string when the `weather`
    variable is not reported.
    - short option: `-w`
    - Default: ""
  - `--useManager` _bool_ : Whether to use one single manager per monitor for
    managing network connections or create a new one every time a connection is
    made.
    - Short option: `-m`
    - Default: True
- Variables that can be used with the `-t`/`--template` argument:
	    `station`, `stationState`, `year`, `month`, `day`, `hour`,
	    `windCardinal`, `windAzimuth`, `windMph`, `windKnots`, `windMs`, `windKmh`
        `visibility`, `skyCondition`, `weather`, `tempC`, `tempF`,
	    `dewPointC`, `dewPointF`, `rh`, `pressure`
- Default template: `<station>: <tempC>C, rh <rh>% (<hour>)`
- Retrieves weather information from http://tgftp.nws.noaa.gov.

### `WeatherX StationID SkyConditions Args RefreshRate`

- Works in the same way as `Weather`, but takes an additional
  argument, a list of pairs from sky conditions to their replacement
  (typically a unicode string or an icon specification).
- Use the variable `skyConditionS` to display the replacement of the
  corresponding sky condition.  All other `Weather` template variables
  are available as well.

For example:

```haskell
  WeatherX "LEBL"
           [ ("clear", "🌣")
           , ("sunny", "🌣")
           , ("mostly clear", "🌤")
           , ("mostly sunny", "🌤")
           , ("partly sunny", "⛅")
           , ("fair", "🌑")
           , ("cloudy","☁")
           , ("overcast","☁")
           , ("partly cloudy", "⛅")
           , ("mostly cloudy", "🌧")
           , ("considerable cloudiness", "⛈")]
           ["-t", "<fn=2><skyConditionS></fn> <tempC>° <rh>%  <windKmh> (<hour>)"
           , "-L","10", "-H", "25", "--normal", "black"
           , "--high", "lightgoldenrod4", "--low", "darkseagreen4"]
           18000
```

As mentioned, the replacement string can also be an icon
specification, such as `("clear", "<icon=weather-clear.xbm/>")`.

### `Network Interface Args RefreshRate`

- Aliases to the interface name: so `Network "eth0" []` can be used as
  `%eth0%`
- Thresholds refer to velocities expressed in Kb/s
- Args: default monitor arguments, plus:
  - `--rx-icon-pattern`: dynamic string for reception rate in `rxipat`.
  - `--tx-icon-pattern`: dynamic string for transmission rate in `txipat`.
  - `--up`: string used for the `up` variable value when the
    interface is up.
- Variables that can be used with the `-t`/`--template` argument:
  `dev`, `rx`, `tx`, `rxbar`, `rxvbar`, `rxipat`, `txbar`, `txvbar`,
  `txipat`, `up`. Reception and transmission rates (`rx` and `tx`) are
  displayed by default as Kb/s, without any suffixes, but you can set
  the `-S` to "True" to make them displayed with adaptive units (Kb/s,
  Mb/s, etc.).
- Default template: `<dev>: <rx>KB|<tx>KB`

### `DynNetwork Args RefreshRate`

- Active interface is detected automatically
- Aliases to "dynnetwork"
- Thresholds are expressed in Kb/s
- Args: default monitor arguments, plus:
  - `--rx-icon-pattern`: dynamic string for reception rate in `rxipat`.
  - `--tx-icon-pattern`: dynamic string for transmission rate in `txipat`
  - `--devices`: comma-separated list of devices to show.
- Variables that can be used with the `-t`/`--template` argument:
  `dev`, `rx`, `tx`, `rxbar`, `rxvbar`, `rxipat`, `txbar`, `txvbar`,
  `txipat`. Reception and transmission rates (`rx` and `tx`) are displayed
  in Kbytes per second, and you can set the `-S` to "True" to make them
  displayed with units (the string "Kb/s").
- Default template: `<dev>: <rx>KB|<tx>KB`
- Example of usage of `--devices` option: `["--", "--devices", "wlp2s0,enp0s20f41"]`

### `Wireless Interface Args RefreshRate`

- If set to "", first suitable wireless interface is used.
- Aliases to the interface name with the suffix "wi": thus, `Wireless
  "wlan0" []` can be used as `%wlan0wi%`, and `Wireless "" []` as `%wi%`.
- Args: default monitor arguments, plus:
  - `--quality-icon-pattern`: dynamic string for connection quality in `qualityipat`.
- Variables that can be used with the `-t`/`--template` argument:
            `ssid`, `signal`, `quality`, `qualitybar`, `qualityvbar`, `qualityipat`
- Thresholds refer to link quality on a `[0, 100]` scale. Note that
  `quality` is calculated from `signal` (in dBm) by a possibly lossy
  conversion. It is also not taking into account many factors such as
  noise level, air busy time, transcievers' capabilities and the
  others which can have drastic impact on the link performance.
- Default template: `<ssid> <quality>`
- To activate this plugin you must pass `--flags="with_nl80211"` or
  `--flags="with_iwlib"` during compilation

### `Memory Args RefreshRate`

- Aliases to `memory`
- Args: default monitor arguments, plus:
  - `--used-icon-pattern`: dynamic string for used memory ratio in `usedipat`.
  - `--free-icon-pattern`: dynamic string for free memory ratio in `freeipat`.
  - `--available-icon-pattern`: dynamic string for available memory ratio in `availableipat`.
- Thresholds refer to percentage of used memory
- Variables that can be used with the `-t`/`--template` argument:
             `total`, `free`, `buffer`, `cache`, `available`, `used`,
             `usedratio`, `usedbar`, `usedvbar`, `usedipat`,
             `freeratio`, `freebar`, `freevbar`, `freeipat`,
             `availableratio`, `availablebar`, `availablevbar`, `availableipat`
- Default template: `Mem: <usedratio>% (<cache>M)`

### `Swap Args RefreshRate`

- Aliases to `swap`
- Args: default monitor arguments
- Thresholds refer to percentage of used swap
- Variables that can be used with the `-t`/`--template` argument:
	    `total`, `used`, `free`, `usedratio`
- Default template: `Swap: <usedratio>%`

### `Cpu Args RefreshRate`

- Aliases to `cpu`
- Args: default monitor arguments, plus:
  - `--load-icon-pattern`: dynamic string for cpu load in `ipat`
- Thresholds refer to percentage of CPU load
- Variables that can be used with the `-t`/`--template` argument:
	    `total`, `bar`, `vbar`, `ipat`, `user`, `nice`, `system`, `idle`, `iowait`
- Default template: `Cpu: <total>%`

### `MultiCpu Args RefreshRate`

- Aliases to `multicpu`
- Args: default monitor arguments, plus:
  - `--load-icon-pattern`: dynamic string for overall cpu load in `ipat`.
  - `--load-icon-patterns`: dynamic string for each cpu load in `autoipat`, `ipat{i}`.
                              This option can be specified several times. nth option
                              corresponds to nth cpu.
  - `--fallback-icon-pattern`: dynamic string used by `autoipat` and `ipat{i}` when no
                             `--load-icon-patterns` has been provided for `cpu{i}`
  - `--contiguous-icons`: flag (no value needs to be provided) that
                          causes the load icons to be drawn without padding.
- Thresholds refer to percentage of CPU load
- Variables that can be used with the `-t`/`--template` argument:
	    `autototal`, `autobar`, `autovbar`, `autoipat`, `autouser`, `autonice`,
	    `autosystem`, `autoidle`, `total`, `bar`, `vbar`, `ipat`, `user`, `nice`,
	    `system`, `idle`, `total0`, `bar0`, `vbar0`, `ipat0`, `user0`, `nice0`,
	    `system0`, `idle0`, ...
  The auto* variables automatically detect the number of CPUs on the system
  and display one entry for each.
- Default template: `Cpu: <total>%`

### `Battery Args RefreshRate`

- Same as `BatteryP ["BAT", "BAT0", "BAT1", "BAT2"] Args RefreshRate`.

### `BatteryP Dirs Args RefreshRate`

- Aliases to `battery`

- Dirs: list of directories in `/sys/class/power_supply/` where to
  look for the ACPI files of each battery. Example:
  `["BAT0","BAT1","BAT2"]`. Only up to 3 existing directories will be
  searched.

- Args: default monitor arguments, plus the following specific ones
  (these options, being specific to the monitor, are to be specified
  after a `--` in the argument list):
  - `-O`: string for AC "on" status (default: "On")
  - `-i`: string for AC "idle" status (default: "On")
  - `-o`: string for AC "off" status (default: "Off")
  - `-L`: low power (`watts`) threshold (default: 10)
  - `-H`: high power threshold (default: 12)
  - `-l`: color to display power lower than the `-L` threshold
  - `-m`: color to display power lower than the `-H` threshold
  - `-h`: color to display power higher than the `-H` threshold
  - `-p`: color to display positive power (battery charging)
  - `-f`: file in `/sys/class/power_supply` with AC info (default:
    "AC/online")
  - `-A`: a number between 0 and 100, threshold below which the action
    given by `-a`, if any, is performed (default: 5)
  - `-a`: a string with a system command that is run when the
    percentage left in the battery is less or equal than the threshold
    given by the `-A` option.  If not present, no action is
    undertaken.
  - `-P`: to include a percentage symbol in `left`.
  - `--on-icon-pattern`: dynamic string for current battery charge
    when AC is "on" in `leftipat`.
  - `--off-icon-pattern`: dynamic string for current battery charge
    when AC is "off" in `leftipat`.
  - `--idle-icon-pattern`: dynamic string for current battery charge
    when AC is "idle" in `leftipat`.
  - `--lows`: string for AC "off" status and power lower than the `-L`
    threshold (default: "")
  - `--mediums`: string for AC "off" status and power lower than the `-H`
    threshold (default: "")
  - `--highs`: string for AC "off" status and power higher than the `-H`
    threshold (default: "")


- Variables that can be used with the `-t`/`--template` argument:
	    `left`, `leftbar`, `leftvbar`, `leftipat`, `timeleft`, `watts`, `acstatus`
- Default template: `Batt: <watts>, <left>% / <timeleft>`
- Example (note that you need "--" to separate regular monitor options from
  Battery's specific ones):

         Run BatteryP ["BAT0"]
                      ["-t", "<acstatus><watts> (<left>%)",
                       "-L", "10", "-H", "80", "-p", "3",
                       "--", "-O", "<fc=green>On</fc> - ", "-i", "",
                       "-L", "-15", "-H", "-5",
                       "-l", "red", "-m", "blue", "-h", "green"
                       "-a", "notify-send -u critical 'Battery running out!!'",
                       "-A", "3"]
                      600

  In the above example, the thresholds before the "--" separator
  affect only the `<left>` and `<leftbar>` fields, while those after
  the separator affect how `<watts>` is displayed. For this monitor,
  neither the generic nor the specific options have any effect on
  `<timeleft>`.  We are also telling the monitor to execute the unix
  command `notify-send` when the percentage left in the battery
  reaches 6%.

  It is also possible to specify template variables in the `-O` and
  `-o` switches, as in the following example:

         Run BatteryP ["BAT0"]
                      ["-t", "<acstatus>"
                      , "-L", "10", "-H", "80"
                      , "-l", "red", "-h", "green"
                      , "--", "-O", "Charging", "-o", "Battery: <left>%"
                      ] 10

- The "idle" AC state is selected whenever the AC power entering the
  battery is zero.

### `BatteryN Dirs Args RefreshRate Alias`

Works like `BatteryP`, but lets you specify an alias for the monitor
other than "battery".  Useful in case you one separate monitors for
more than one battery.

### `TopProc Args RefreshRate`

- Aliases to `top`
- Args: default monitor arguments. The low and high
  thresholds (`-L` and `-H`) denote, for memory entries, the percent
  of the process memory over the total amount of memory currently in
  use and, for cpu entries, the activity percentage (i.e., the value
  of `cpuN`, which takes values between 0 and 100).
- Variables that can be used with the `-t`/`--template` argument:
	    `no`, `name1`, `cpu1`, `both1`, `mname1`, `mem1`, `mboth1`,
            `name2`, `cpu2`, `both2`, `mname2`, `mem2`, `mboth2`, ...
- Default template: `<both1>`
- Displays the name and cpu/mem usage of running processes (`bothn`
  and `mboth` display both, and is useful to specify an overall
  maximum and/or minimum width, using the `-m`/`-M` arguments. `no` gives
  the total number of processes.

### `TopMem Args RefreshRate`

- Aliases to `topmem`
- Args: default monitor arguments. The low and high
  thresholds (`-L` and `-H`) denote the percent of the process memory
  over the total amount of memory currently in use.
- Variables that can be used with the `-t`/`--template` argument:
	    `name1`, `mem1`, `both1`, `name2`, `mem2`, `both2`, ...
- Default template: `<both1>`
- Displays the name and RSS (resident memory size) of running
  processes (`bothn` displays both, and is useful to specify an
  overall maximum and/or minimum width, using the `-m`/`-M` arguments.

### `DiskU Disks Args RefreshRate`

- Aliases to `disku`
- Disks: list of pairs of the form (device or mount point, template),
  where the template can contain `<size>`, `<free>`, `<used>`, `<freep>` or
  `<usedp>`, `<freebar>`, `<freevbar>`, `<freeipat>`, `<usedbar>`,
  `<usedvbar>` or `<usedipat>` for total, free, used, free percentage and
  used percentage of the given file system capacity.
- Thresholds refer to usage percentage.
- Args: default monitor arguments. `-t`/`--template` is ignored. Plus
  - `--free-icon-pattern`: dynamic string for free disk space in `freeipat`.
  - `--used-icon-pattern`: dynamic string for used disk space in `usedipat`.
- Default template: none (you must specify a template for each file system).
- Example:

         DiskU [("/", "<used>/<size>"), ("sdb1", "<usedbar>")]
               ["-L", "20", "-H", "50", "-m", "1", "-p", "3"]
               20

### `DiskIO Disks Args RefreshRate`

- Aliases to `diskio`
- Disks: list of pairs of the form (device or mount point, template),
  where the template can contain `<total>`, `<read>`, `<write>` for
  total, read and write speed, respectively, as well as `<totalb>`,
  `<readb>`, `<writeb>`, which report number of bytes during the last
  refresh period rather than speed. There are also bar versions of
  each: `<totalbar>`, `<totalvbar>`, `<totalipat>`, `<readbar>`,
  `<readvbar>`, `<readipat>`, `<writebar>`, `<writevbar>`, and
  `<writeipat>`; and their "bytes" counterparts: `<totalbbar>`,
  `<totalbvbar>`, `<totalbipat>`, `<readbbar>`, `<readbvbar>`,
  `<readbipat>`, `<writebbar>`, `<writebvbar>`, and `<writebipat>`.
- Thresholds refer to speed in b/s
- Args: default monitor arguments. `-t`/`--template` is ignored. Plus
  - `--total-icon-pattern`: dynamic string for total disk I/O in `<totalipat>`.
  - `--write-icon-pattern`: dynamic string for write disk I/O in `<writeipat>`.
  - `--read-icon-pattern`: dynamic string for read disk I/O in `<readipat>`.
- Default template: none (you must specify a template for each file system).
- Example:

         DiskIO [("/", "<read> <write>"), ("sdb1", "<total>")] [] 10

### `ThermalZone Number Args RefreshRate`

- Aliases to "thermaln": so `ThermalZone 0 []` can be used in template
  as `%thermal0%`
- Thresholds refer to temperature in degrees
- Args: default monitor arguments
- Variables that can be used with the `-t`/`--template` argument:
	    `temp`
- Default template: `<temp>C`
- This plugin works only on systems with devices having thermal zone.
  Check directories in `/sys/class/thermal` for possible values of the
  zone number (e.g., 0 corresponds to `thermal_zone0` in that
  directory).
- Example:

         Run ThermalZone 0 ["-t","<id>: <temp>C"] 30

#### `Thermal Zone Args RefreshRate`

- **This plugin is deprecated. Use `ThermalZone` instead.**

- Aliases to the Zone: so `Thermal "THRM" []` can be used in template
  as `%THRM%`
- Args: default monitor arguments
- Thresholds refer to temperature in degrees
- Variables that can be used with the `-t`/`--template` argument:
	    `temp`
- Default template: `Thm: <temp>C`
- This plugin works only on systems with devices having thermal zone.
  Check directories in /proc/acpi/thermal_zone for possible values.
- Example:

         Run Thermal "THRM" ["-t","iwl4965-temp: <temp>C"] 50

### `CpuFreq Args RefreshRate`

- Aliases to `cpufreq`
- Args: default monitor arguments
- Thresholds refer to frequency in GHz
- Variables that can be used with the `-t`/`--template` argument:
	    `cpu0`, `cpu1`, ..,  `cpuN`
- Default template: `Freq: <cpu0>GHz`
- This monitor requires acpi_cpufreq module to be loaded in kernel
- Example:

         Run CpuFreq ["-t", "Freq:<cpu0>|<cpu1>GHz", "-L", "0", "-H", "2",
                      "-l", "lightblue", "-n","white", "-h", "red"] 50

### `CoreTemp Args RefreshRate`

- Aliases to `coretemp`
- Args: default monitor arguments
- Thresholds refer to temperature in degrees
- Variables that can be used with the `-t`/`--template` argument:
	    `core0`, `core1`, ..,  `coreN`
- Default template: `Temp: <core0>C`
- This monitor requires coretemp module to be loaded in kernel
- Example:

         Run CoreTemp ["-t", "Temp:<core0>|<core1>C",
                       "-L", "40", "-H", "60",
                       "-l", "lightblue", "-n", "gray90", "-h", "red"] 50

### `MultiCoreTemp Args RefreshRate`

- Aliases to `multicoretemp`
- Args: default monitor arguments, plus:
  - `--max-icon-pattern`: dynamic string for overall cpu load in `maxipat`.
  - `--avg-icon-pattern`: dynamic string for overall cpu load in `avgipat`.
  - `--mintemp`: temperature in degree Celsius, that sets the lower
    limit for percentage calculation.
  - `--maxtemp`: temperature in degree Celsius, that sets the upper
    limit for percentage calculation.
- Thresholds refer to temperature in degree Celsius
- Variables that can be used with the `-t`/`--template` argument:
            `max`, `maxpc`, `maxbar`, `maxvbar`, `maxipat`,
            `avg`, `avgpc`, `avgbar`, `avgvbar`, `avgipat`,
            `core0`, `core1`, ..., `coreN`

  The *pc, *bar, *vbar and *ipat variables are showing percentages on the scale
  defined by `--mintemp` and `--maxtemp`.
  The max* and avg* variables to the highest and the average core temperature.
- Default template: `Temp: <max>°C - <maxpc>%`
- This monitor requires coretemp module to be loaded in kernel
- Example:

         Run MultiCoreTemp ["-t", "Temp: <avg>°C | <avgpc>%",
                            "-L", "60", "-H", "80",
                            "-l", "green", "-n", "yellow", "-h", "red",
                            "--", "--mintemp", "20", "--maxtemp", "100"] 50

### `Volume Mixer Element Args RefreshRate`

- Aliases to the mixer name and element name separated by a colon. Thus,
  `Volume "default" "Master" [] 10` can be used as `%default:Master%`.
- Args: default monitor arguments. Also accepts:
    - `-O` _string_ On string
        - The string used in place of `<status>` when the mixer element
          is on. Defaults to "[on]".
        - Long option: `--on`
    - `-o` _string_ Off string
        - The string used in place of `<status>` when the mixer element
          is off. Defaults to "[off]".
        - Long option: `--off`
    - `-C` _color_ On color
        - The color to be used for `<status>` when the mixer element
          is on. Defaults to "green".
        - Long option: `--onc`
    - `-c` _color_ Off color
        - The color to be used for `<status>` when the mixer element
          is off. Defaults to "red".
        - Long option: `--offc`
    - `--highd` _number_ High threshold for dB. Defaults to -5.0.
    - `--lowd` _number_ Low threshold for dB. Defaults to -30.0.
    - `--volume-icon-pattern` _string_ dynamic string for current volume in `volumeipat`.
    - `-H` _number_ High threshold for volume (in %). Defaults to 60.0.
        - Long option: `--highv`
    - `-L` _number_ Low threshold for volume (in %). Defaults to 20.0.
        - Long option: `--lowv`
    - `-h`: _string_ High string
        - The string added in front of `<status>` when the mixer element
          is on and the volume percentage is higher than the `-H` threshold.
          Defaults to "".
        - Long option: `--highs`
    - `-m`: _string_ Medium string
        - The string added in front of `<status>` when the mixer element
          is on and the volume percentage is lower than the `-H` threshold.
          Defaults to "".
        - Long option: `--mediums`
    - `-l`: _string_ Low string
        - The string added in front of `<status>` when the mixer element
          is on and the volume percentage is lower than the `-L` threshold.
          Defaults to "".
        - Long option: `--lows`
- Variables that can be used with the `-t`/`--template` argument:
            `volume`, `volumebar`, `volumevbar`, `volumeipat`, `dB`, `status`,
            `volumestatus`
- Note that `dB` might only return 0 on your system. This is known
  to happen on systems with a pulseaudio backend.
- Default template: `Vol: <volume>% <status>`
- Requires the package [alsa-core] and [alsa-mixer] installed in your
  system. In addition, to activate this plugin you must pass
  `--flags="with_alsa"` during compilation.

### `Alsa Mixer Element Args`

Like [Volume](#volume-mixer-element-args-refreshrate), but with the
following differences:
- Uses event-based refreshing via `alsactl monitor` instead of
  polling, so it will refresh instantly when there's a volume change,
  and won't use CPU until a change happens.
- Aliases to `alsa:` followed by the mixer name and element name
  separated by a colon. Thus, `Alsa "default" "Master" []` can be used
  as `%alsa:default:Master%`.
- Additional options (after the `--`):
    - `--alsactl=/path/to/alsactl`
        - If this option is not specified, `alsactl` will be sought in
          your `PATH` first, and failing that, at `/usr/sbin/alsactl`
          (this is its location on Debian systems. `alsactl monitor`
          works as a non-root user despite living in `/usr/sbin`.).
- `stdbuf` (from coreutils) must be (and most probably already is) in
  your `PATH`.

### `MPD Args RefreshRate`

- This monitor will only be compiled if you ask for it using the
  `with_mpd` flag. It needs [libmpd] 5.0 or later (available on Hackage).
- Aliases to `mpd`
- Args: default monitor arguments. In addition you can provide `-P`,
  `-S` and `-Z`, with an string argument, to represent the playing,
  stopped and paused states in the `statei` template field.  The
  environment variables `MPD_HOST` and `MPD_PORT` are used to
  configure the mpd server to communicate with, unless given in the
  additional arguments `-p` (`--port`) and `-h` (`--host`). Also
  available:
  - `lapsed-icon-pattern`: dynamic string for current track position in `ipat`.
- Variables that can be used with the `-t`/`--template` argument:
             `bar`, `vbar`, `ipat`, `state`, `statei`, `volume`, `length`,
             `lapsed`, `remaining`,
             `plength` (playlist length), `ppos` (playlist position),
             `flags` (ncmpcpp-style playback mode),
             `name`, `artist`, `composer`, `performer`,
             `album`, `title`, `track`, `file`, `genre`, `date`
- Default template: `MPD: <state>`
- Example (note that you need "--" to separate regular monitor options from
  MPD's specific ones):

         Run MPD ["-t",
                  "<composer> <title> (<album>) <track>/<plength> <statei> [<flags>]",
                  "--", "-P", ">>", "-Z", "|", "-S", "><"] 10

### `Mpris1 PlayerName Args RefreshRate`

- Aliases to `mpris1`
- Requires [dbus] and [text] packages.
  To activate, pass `--flags="with_mpris"` during compilation.
- PlayerName: player supporting MPRIS v1 protocol.  Some players need
  this to be an all lowercase name (e.g. "spotify"), but some others
  don't.
- Args: default monitor arguments.
- Variables that can be used with the `-t`/`--template` argument:
            `album`, `artist`, `arturl`, `length`, `title`, `tracknumber`
- Default template: `<artist> - <title>`
- Example:

         Run Mpris1 "clementine" ["-t", "<artist> - [<tracknumber>] <title>"] 10

### `Mpris2 PlayerName Args RefreshRate`

- Aliases to `mpris2`
- Requires [dbus] and [text] packages.
  To activate, pass `--flags="with_mpris"` during compilation.
- PlayerName: player supporting MPRIS v2 protocol.  Some players need
  this to be an all lowercase name (e.g. "spotify"), but some others
  don't.
- Args: default monitor arguments.
- Variables that can be used with the `-t`/`--template` argument:
            `album`, `artist`, `arturl`, `length`, `title`,
            `tracknumber`, `composer`, `genre`
- Default template: `<artist> - <title>`
- Example:

         Run Mpris2 "spotify" ["-t", "<artist> - [<composer>] <title>"] 10

### `Mail Args Alias`

- Args: list of maildirs in form
  `[("name1","path1"),...]`. Paths may start with a '~'
  to expand to the user's home directory.
- This plugin requires inotify support in your Linux kernel and the
  [hinotify] package. To activate, pass `--flags="with_inotify"`
  during compilation.
- Example:

         Run Mail [("inbox", "~/var/mail/inbox"),
                   ("lists", "~/var/mail/lists")]
                  "mail"

### `MailX Args Opts Alias`

- Args: list of maildirs in form
  `[("name1","path1","color1"),...]`. Paths may start with a '~'
  to expand to the user's home directory.  When mails are present,
  counts are displayed with the given name and color.
- Opts is a possibly empty list of options, as flags. Possible values:
   -d dir  --dir dir a string giving the base directory where maildir files with
                     a relative path live.
   -p prefix --prefix prefix  a string giving a prefix for the list
                      of displayed mail counts
   -s suffix --suffix suffix  a string giving a suffix for the list
                      of displayed mail counts
- This plugin requires inotify support in your Linux kernel and the
  [hinotify] package. To activate, pass `--flags="with_inotify"`
  during compilation.
- Example:

         Run MailX [("I", "inbox", "green"),
                    ("L", "lists", "orange")]
                   ["-d", "~/var/mail", "-p", " ", "-s", " "]
                   "mail"


### `MBox Mboxes Opts Alias`

- Mboxes a list of mbox files of the form `[("name", "path", "color")]`,
  where name is the displayed name, path the absolute or relative (to
  BaseDir) path of the mbox file, and color the color to use to display
  the mail count (use an empty string for the default).
- Opts is a possibly empty list of options, as flags. Possible values:
   -a  --all (no arg)  Show all mailboxes, even if empty.
   -u (no arg) Show only the mailboxes' names, sans counts.
   -d dir  --dir dir a string giving the base directory where mbox files with
                     a relative path live.
   -p prefix --prefix prefix  a string giving a prefix for the list
                      of displayed mail counts
   -s suffix --suffix suffix  a string giving a suffix for the list
                      of displayed mail counts
- Paths may start with a '~' to expand to the user's home directory.
- This plugin requires inotify support in your Linux kernel and the
  [hinotify] package. To activate, pass `--flags="with_inotify"`
  during compilation.
- Example. The following command look for mails in `/var/mail/inbox`
  and `~/foo/mbox`, and will put a space in front of the printed string
  (when it's not empty); it can be used in the template with the alias
  `mbox`:

         Run MBox [("I ", "inbox", "red"), ("O ", "~/foo/mbox", "")]
                  ["-d", "/var/mail/", "-p", " "] "mbox"

### `XPropertyLog PropName`

- Aliases to `PropName`
- Reads the X property named by `PropName` (a string) and displays its
  value. The [examples/xmonadpropwrite.hs script] in xmobar's
  distribution can be used to set the given property from the output
  of any other program or script.

[examples/xmonadpropwrite.hs script]: https://github.com/jaor/xmobar/raw/master/examples/xmonadpropwrite.hs

### `UnsafeXPropertyLog PropName`

- Aliases to `PropName`
- Same as `XPropertyLog`, but the input is not filtered to avoid
  injection of actions (cf. `UnsafeXMonadLog`).  The program writing
  the value of the read property is responsible of performing any
  needed cleanups.

### `NamedXPropertyLog PropName Alias`

- Aliases to `Alias`
- Same as `XPropertyLog`, but a custom alias can be specified.

### `NamedXPropertyLog PropName Alias`

- Aliases to `Alias`
- Same as `UnsafeXPropertyLog`, but a custom alias can be specified.

### `Brightness Args RefreshRate`

- Aliases to `bright`
- Args: default monitor arguments, plus the following specif ones:
    - `-D`: directory in `/sys/class/backlight/` with files in it
       (default: "acpi_video0")
    - `-C`: file with the current brightness (default:
       actual_brightness)
    - `-M`: file with the maximum brightness (default:
       max_brightness)
    - `--brightness-icon-pattern`: dynamic string for current brightness in `ipat`.
- Variables that can be used with the `-t`/`--template` argument:
	    `vbar`, `percent`, `bar`, `ipat`
- Default template: `<percent>`
- Example:

       Run Brightness ["-t", "<bar>"] 60

### `Kbd Opts`

- Registers to XKB/X11-Events and output the currently active keyboard layout.
  Supports replacement of layout names.
- Aliases to `kbd`
- Opts is a list of tuples:
    -  first element of the tuple is the search string
    -  second element of the tuple is the corresponding replacement
- Example:

		Run Kbd [("us(dvorak)", "DV"), ("us", "US")]

### `Locks`

- Displays the status of Caps Lock, Num Lock and Scroll Lock.
- Aliases to `locks`
- Example:

	Run Locks

### `CatInt n filename`

- Reads and displays an integer from the file whose path is `filename`
  (especially useful with files in `/sys`).
- Aliases as `catn` (e.g. `Cat 0` as `cat0`, etc.) so you can
  have several.
- Example:

    Run CatInt 0 "/sys/devices/platform/thinkpad_hwmon/fan1_input" [] 50

### `UVMeter`

- Aliases to "uv " + station id. For example: `%uv Brisbane%` or `%uv
  Alice Springs%`
- Args: default monitor arguments, plus:
  - `--useManager` _bool_ : Whether to use one single manager per monitor for
    managing network connections or create a new one every time a connection is
    made.
    - Short option: `-m`
    - Default: True

- *Reminder:* Keep the refresh rate high, to avoid making unnecessary
  requests every time the plug-in is run.
- Station IDs can be found here:
  http://www.arpansa.gov.au/uvindex/realtime/xml/uvvalues.xml
- Example:

        Run UVMeter "Brisbane" ["-H", "3", "-L", "3", "--low", "green", "--high", "red"] 900

## Executing External Commands

In order to execute an external command you can either write the
command name in the template, in this case it will be executed without
arguments, or you can configure it in the "commands" configuration
option list with the Com template command:

`Com ProgramName Args Alias RefreshRate`

- ProgramName: the name of the program
- Args: the arguments to be passed to the program at execution time
- RefreshRate: number of tenths of second between re-runs of the
  command. A zero or negative rate means that the command will be
  executed only once.
- Alias: a name to be used in the template. If the alias is en empty
  string the program name can be used in the template.

E.g.:

        Run Com "uname" ["-s","-r"] "" 0

can be used in the output template as `%uname%` (and xmobar will call
_uname_ only once), while

        Run Com "date" ["+\"%a %b %_d %H:%M\""] "mydate" 600

can be used in the output template as `%mydate%`.

Sometimes, you don't mind if the command executed exits with an error,
or you might want to display a custom message in that case.  To that
end, you can use the `ComX` variant:

`ComX ProgramName Args ExitMessage Alias RefreshRate`

Works like `Com`, but displaying `ExitMessage` (a string) if the
execution fails.  For instance:

        Run ComX "date" ["+\"%a %b %_d %H:%M\""] "N/A" "mydate" 600

will display "N/A" if for some reason the `date` invocation fails.

## Other Plugins

### `StdinReader`

- Aliases to StdinReader
- Displays any text received by xmobar on its standard input.
- Strips actions from the text received.  This means you can't pass dynamic
  actions via stdin.  This is safer than `UnsafeStdinReader` because there is
  no need to escape the content before passing it to xmobar's standard input.

### `UnsafeStdinReader`

- Aliases to UnsafeStdinReader
- Displays any text received by xmobar on its standard input.
- Will not do anything to the text received.  This means you can pass dynamic
  actions via stdin.  Be careful to remove tags from dynamic text that you
  pipe-thru to xmobar's standard input, e.g. window's title.  There is no way
  to escape the tags, i.e. you can't print a literal `<action>` tag as a text
  on xmobar.
- Sample usage: send to xmobar's stdin the list of your workspaces enclosed by
  actions tags that switches the workspaces to be able to switch workspaces by
  clicking on xmobar:
  ```<action=`xdotool key alt+1`>ws1</action> <action=`xdotool key alt+1`>ws2</action>```

### `Date Format Alias RefreshRate`

- Format is a time format string, as accepted by the standard ISO C
  `strftime` function (or Haskell's `formatCalendarTime`).
- Sample usage: `Run Date "%a %b %_d %Y <fc=#ee9a00>%H:%M:%S</fc>" "date" 10`

### `DateZone Format Locale Zone Alias RefreshRate`

- Format is a time format string, as accepted by the standard ISO C
  `strftime` function (or Haskell's `formatCalendarTime`).
- If Locale is "" the default locale of the system is used, otherwise the given
  locale. If there are more instances of DateZone, using "" as input for Locale
  is not recommended.
- Zone is the name of the TimeZone. It is assumed that the tz database is stored
  in /usr/share/zoneinfo/. If "" is given as Zone, the default system time is
  used.
- Sample usage:
  `Run DateZone "%a %H:%M:%S" "de_DE.UTF-8" "Europe/Vienna" "viennaTime" 10`

### `CommandReader "/path/to/program" Alias`

- Runs the given program, and displays its standard output.

### `PipeReader "default text:/path/to/pipe" Alias`

- Reads its displayed output from the given pipe.
- Prefix an optional default text separated by a colon
- Expands environment variables in the first argument of syntax '${VAR}' or '$VAR'

### `MarqueePipeReader "default text:/path/to/pipe" (length, rate, sep) Alias`

- Generally equivalent to PipeReader
- Text is displayed as marquee with the specified length, rate in 10th
  seconds and separator when it wraps around

        Run MarqueePipeReader "/tmp/testpipe" (10, 7, "+") "mpipe"

- Expands environment variables in the first argument

### `BufferedPipeReader Alias [(Timeout, Bool, "/path/to/pipe1"), ..]`

- Display data from multiple pipes.
- Timeout (in tenth of seconds) is the value after which the previous
  content is restored i.e. if there was already something from a
  previous pipe it will be put on display again, overwriting the
  current status.
- A pipe with Timeout of 0 will be displayed permanently, just like
  `PipeReader`
- The boolean option indicates whether new data for this pipe should
  make xmobar appear (unhide, reveal). In this case, the Timeout
  additionally specifies when the window should be hidden again. The
  output is restored in any case.
- Use it for OSD-like status bars e.g. for setting the volume or
  brightness:

        Run BufferedPipeReader "bpr"
            [ (  0, False, "/tmp/xmobar_window"  )
            , ( 15,  True, "/tmp/xmobar_status"  )
            ]

  Have your window manager send window titles to
  `"/tmp/xmobar_window"`. They will always be shown and not reveal
  your xmobar.  Sending some status information to
  `"/tmp/xmobar_status"` will reveal xmonad for 1.5 seconds and
  temporarily overwrite the window titles.
- Take a look at [examples/status.sh]
- Expands environment variables for the pipe path

[examples/status.sh]: http://github.com/jaor/xmobar/raw/master/examples/status.sh


### `XMonadLog`

- Aliases to XMonadLog
- Displays information from xmonad's `_XMONAD_LOG`. You can set this
  property by using `xmonadPropLog` as your log hook in xmonad's
  configuration, as in the following example (more info [here]):

        main = do
          spawn "xmobar"
          xmonad $ defaultConfig {
            logHook = dynamicLogString defaultPP >>= xmonadPropLog
          }
   This plugin can be used as a sometimes more convenient alternative
   to `StdinReader`. For instance, it allows you to (re)start xmobar
   outside xmonad.

[here]: http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Hooks-DynamicLog.html

### `UnsafeXMonadLog`

- Aliases to UnsafeXMonadLog
- Similar to StdinReader versus UnsafeStdinReader, this does not strip `<action
  ...>` tags from XMonad's `_XMONAD_LOG`.
- It is advised that you still use `xmobarStrip` for the ppTitle in your
  logHook:

        myPP = defaultPP { ppTitle = xmobarStrip }
        main = xmonad $ defaultConfig {
          logHook = dynamicLogString myPP >>= xmonadPropLog
        }

### `HandleReader Handle Alias`

- Display data from a Haskell `Handle`
- This plugin is only useful if you are running xmobar from another Haskell
  program like XMonad.
- You can use `System.Process.createPipe` to create a pair of `read` & `write`
  Handles. Pass the `read` Handle to HandleReader and write your output to the
  `write` Handle:

        (readHandle, writeHandle) <- createPipe
        xmobarProcess <- forkProcess $ xmobar myConfig
                { commands =
                    Run (HandleReader readHandle "handle") : commands myConfig
                }
        hPutStr writeHandle "Hello World"


# Plugins

## Writing a Plugin

Writing a plugin for xmobar should be very simple. You need to create
a data type with at least one constructor.

Next you must declare this data type an instance of the `Exec` class, by
defining the 1 needed method (alternatively `start` or `run`) and 2
optional ones (alias and rate):

        start :: e -> (String -> IO ()) -> IO ()
        run   :: e -> IO String
        rate  :: e -> Int
        alias :: e -> String

`start` must receive a callback to be used to display the `String`
produced by the plugin. This method can be used for plugins that need
to perform asynchronous actions. See
`src/Xmobar/Plugins/PipeReader.hs` for an example.

`run` can be used for simpler plugins. If you define only `run` the
plugin will be run every second. To overwrite this default you just
need to implement `rate`, which must return the number of tenth of
seconds between every successive runs. See `examples/xmobar.hs` for an
example of a plugin that runs just once, and
`src/Xmobar/Plugins/Date.hs` for one that implements `rate`.

Notice that Date could be implemented as:

        instance Exec Date where
            alias (Date _ a _) = a
            start (Date f _ r) = date f r

        date :: String -> Int -> (String -> IO ()) -> IO ()
        date format r callback = do go
            where go = do
                    t <- toCalendarTime =<< getClockTime
                    callback $ formatCalendarTime defaultTimeLocale format t
                    tenthSeconds r >> go

This implementation is equivalent to the one you can read in
`Plugins/Date.hs`.

`alias` is the name to be used in the output template. Default alias
will be the data type constructor.

After that your type constructor can be used as an argument for the
Runnable type constructor `Run` in the `commands` list of the
configuration options.

## Using a Plugin

To use your new plugin, you need to use a pure Haskell configuration
for xmobar, and load your definitions there.  You can see an example
in [examples/xmobar.hs](./examples/xmobar.hs) showing you how to write
a Haskell configuration that uses a new plugin, all in one file.

When xmobar runs with the full path to that Haskell file as its
argument (or if you put it in `~/.config/xmobar/xmobar.hs`), and with
the xmobar library installed, the Haskell code will be compiled as
needed, and the new executable spawned for you.

That's it!

## Configurations written in pure Haskell

xmobar can be used as a pure Haskell program, that is compiled with
your specific configuration, expressed as Haskell source code.  For an
example, see [the author's
configuration](https://gitlab.com/jaor/xmobar-config/).

# Authors and credits

Andrea Rossato originally designed and implemented xmobar up to
version 0.11.1. Since then, it is maintained and developed by [jao],
with the help of the greater xmobar and Haskell communities.

In particular, xmobar [incorporates patches] by Mohammed Alshiekh,
Alex Ameen, Axel Angel, Dhananjay Balan, Claudio Bley, Dragos Boca,
Ben Boeckel, Duncan Burke, Roman Cheplyaka, Patrick Chilton, Antoine
Eiche, Nathaniel Wesley Filardo, John Goerzen, Reto Hablützel, Juraj
Hercek, Tomáš Janoušek, Ada Joule, Spencer Janssen, Roman Joost,
Jochen Keil, Lennart Kolmodin, Krzysztof Kosciuszkiewicz, Dmitry
Kurochkin, Todd Lunter, Vanessa McHale, Robert J. Macomber, Dmitry
Malikov, David McLean, Marcin Mikołajczyk, Dino Morelli, Tony Morris,
Eric Mrak, Thiago Negri, Edward O'Callaghan, Svein Ove, Martin Perner,
Jens Petersen, Alexander Polakov, Sibi Prabakaran, Pavan Rikhi, Petr
Rockai, Andrew Emmanuel Rosa, Sackville-West, Markus Scherer, Daniel
Schüssler, Olivier Schneider, Alexander Shabalin, Valentin Shirokov,
Peter Simons, Alexander Solovyov, Will Song, John Soros, Felix
Springer, Travis Staton, Artem Tarasov, Samuli Thomasson, Edward
Tjörnhammar, Sergei Trofimovich, Thomas Tuegel, John Tyree, Jan
Vornberger, Anton Vorontsov, Daniel Wagner, Zev Weiss, Phil Xiaojun
Hu, Edward Z. Yang and Norbert Zeh.

[jao]: http://jao.io
[incorporates patches]: http://www.ohloh.net/p/xmobar/contributors

## Thanks

__Andrea Rossato__:

Thanks to Robert Manea and Spencer Janssen for their help in
understanding how X works. They gave me suggestions on how to solve
many problems with xmobar.

Thanks to Claus Reinke for make me understand existential types (or at
least for letting me think I grasp existential types...;-).

__jao__:

Thanks to Andrea for creating xmobar in the first place, and for
giving me the chance to contribute.

# Related

- To understand the internal mysteries of xmobar you may try reading
  [this tutorial] on X Window Programming in Haskell.

- My [sawflibs] project includes a module to automate running xmobar
  in [sawfish].

[this tutorial]: http://www.haskell.org/haskellwiki/X_window_programming_in_Haskell
[sawflibs]: http://github.com/jaor/sawflibs

# License

This software is released under a BSD-style license. See [LICENSE] for
more details.

Copyright &copy; 2010-2020 Jose Antonio Ortega Ruiz

Copyright &copy; 2007-2010 Andrea Rossato

[Github]: http://github.com/jaor/xmobar/
[Github page]: http://github.com/jaor/xmobar
[Hackage]: http://hackage.haskell.org/package/xmobar/
[LICENSE]: https://github.com/jaor/xmobar/raw/master/license
[Mailing list]: http://projects.haskell.org/cgi-bin/mailman/listinfo/xmobar
[MPD]: http://mpd.wikia.com/
[X11-xft]: http://hackage.haskell.org/package/X11-xft/
[i3status]: http://i3wm.org/i3status/
[i3status manual]: http://i3wm.org/i3status/manpage.html#_using_i3status_with_xmobar
[iwlib]: http://www.hpl.hp.com/personal/Jean_Tourrilhes/Linux/Tools.html
[libasound]: http://packages.debian.org/stable/libasound2-dev
[hinotify]: http://hackage.haskell.org/package/hinotify/
[libmpd]: http://hackage.haskell.org/package/libmpd/
[dbus]: http://hackage.haskell.org/package/dbus
[text]: http://hackage.haskell.org/package/text
[sawfish]: http://sawfish.wikia.com/
[utf8-string]: http://hackage.haskell.org/package/utf8-string/
[alsa-core]: http://hackage.haskell.org/package/alsa-core
[alsa-mixer]: http://hackage.haskell.org/package/alsa-mixer
[timezone-olson]: http://hackage.haskell.org/package/timezone-olson
[timezone-series]: http://hackage.haskell.org/package/timezone-series
[libXpm]: http://cgit.freedesktop.org/xorg/lib/libXpm
[http-conduit]: http://hackage.haskell.org/package/http-conduit
[http-types]: http://hackage.haskell.org/package/http-types
