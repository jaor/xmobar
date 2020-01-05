## Version 0.33

_New features_

  - New template parameter `<weather>` for the `Weather` plugin, potentially
    displaying specific weather conditions that are occurring near the
    station (thanks to *slotThe*).
  - New option `--weathers`, for `Weather` to display a default string in
    case the `weather` field is not reported (thanks to *slotThe*).
  - New template parameter `<volumestatus>` for the `Volume` plugin, combining
    the effects of `<volume>` and `<status>`.  This will show the volume
    (possibly prefixed by `onString` or a percentage-based string) if and only
    if the volume is not muted.  Otherwise it will show the `offString`.

## Version 0.32 (December, 2019)

_New features_

  - New options `--host` and `--port` for `MPD` monitor.
  - New plugin `MailX` extending `Mail` with colors and prefix/suffix.
  - New options `--lows`, `--mediums`, and `--highs` for `Battery`
    to display an additional string depending on battery level (thanks
    to *slotThe*).
  - New options `-L` and `-H` for `Volume` to set low and high volume
    levels, as well as `-l`, `-m`, and `-h` to display an additional
    string depending on current volume level (thanks to *slotThe*).
  - New option `-P` in `Battery` to add a `%` symbol to `<left>`.
  - New option `--devices` in `DynNetwork` to select what interfaces
    to monitor (thanks to *vindex10*).
  - DateZone plugin now also checks TZDIR (thanks to Emmanuel Rosa).

_Bug fixes_

   - `Kbd` plugin: ignore "terminate" layout token (thanks to Greg
     Steuck).
   - Fixed compilation with GHC 8.8.x (thanks to Vanessa McHale).
   - Avoid creating `~/.xmobar` ([issue #405]).

[issue #405]: https://github.com/jaor/xmobar/issues/405

## Version 0.31 (October, 2019)

_New features_

  - New option `--contiguous-icons` for `MultiCpu` to draw icons
    without padding (see [issue #388]).
  - New version of libmpd (0.9.0.10), thanks to John Tyree

[issue #388]: https://github.com/jaor/xmobar/issues/388

## Version 0.30 (August, 2019)

_New features_

  - New monitor `MultiCoreTemp`, thanks to Felix Springer.
  - `DiskIO`: Additional template variables for absolute number of
    bytes rather than speeds (see [issue #390]).
  - `WeatherX`: An extension to the `Weather` monitor allowing the
    spefication of custom strings or icons for sky conditions.
  - The battery monitors accept the new arguments `-a` and `-A` to
    specify a system command executed if battery left goes beyond a
    given threshold.

[issue #390]: https://github.com/jaor/xmobar/issues/390

## Version 0.29.5 (March, 2019)

_Bug fixes_

  - Honour command line flags when starting xmobar without any
    configuration file (issue #375).
  - `Alsa` plugin: restart `alsactl` if it quits, e.g. because of a
    sleep/awake cycle (issue #376).
  - `Weather` using the new HTTPS URL, which requires http-conduit as
    a dependency (issue #378).
  - `MarqueePipeReader` exported at the API level (issue #381).

## Version 0.29.4 (December, 2018)

Upgrade to alsa-mixer 0.3.0.  See issues #372 and #373.

## Version 0.29.3 (December, 2018)

_Bug fixes_

  - Upper bound for alsa_mixer (see [issue #372])

[issue #372]: https://github.com/jaor/xmobar/issues/372

## Version 0.29.2 (December, 2018)

_Bug fixes_

  - Work as usual with .xmobarrc (see [issue #371]).

[issue #371]: https://github.com/jaor/xmobar/issues/371

## Version 0.29.1 (December, 2018)

_Bug fixes_

  - Honour command line flags (fixes [issue #370]).
  - Expose Cmd and CmdX in Xmobar interface.

[issue #370]: https://github.com/jaor/xmobar/issues/370

## Version 0.29 (December, 2018)

_New features_

  - New `Alsa` monitor for volume display that doesn't use polling, by
    Daniel Schüssler.
  - `ThermalZone` retries reading missing files, thanks to Reed Koser.
  - `TopProc` ignores kernel threads (see also [issue #369]).
  - `Wireless` simple autodetection of wirless interface name (if set
    to "").
  - Experimental support for using xmobar as a library, so that
    configurations are in fact Haskell programs a la Xmonad (beta
    stage, with help from Pavan Rikhi). Traditional config files are
    still (and will continue to be) supported.

_Bug fixes_

  - Correctly parsing configuration options `mwClass` and `wmName`.

[issue #369]: https://github.com/jaor/xmobar/issues/369

## Version 0.28.1 (October, 2018)

Dependencies updated to work with GHC 8.6, avoiding [issue #354].

[issue #354]: https://github.com/jaor/xmobar/issues/354

## Version 0.28 (August, 2018)

_New features_

  - New `ComX`, which runs a command with custom exit error message.

_Bug fixes_

  - hinotify version upgraded ([issue #356])

[issue #356]: https://github.com/jaor/xmobar/issues/356

## Version 0.27 (July, 2018)

_New features_

  - Dropped support for GHC 7.x.  See [issue #352].
  - New configuration option, `textOffsets` to specify separate
    vertical offsets for each of the fonts in `additionalFonts`.  See
    [issue #311].

_Bug fixes_

   - Use the maximum width options `-T` and `-E` correctly when a
     monitor subtemplate contains font and color tags.

[issue #311]: https://github.com/jaor/xmobar/issues/311
[issue #352]: https://github.com/jaor/xmobar/issues/352

## Version 0.26 (April, 2018)

_New features_

  - New options to specify the ellipsis string for fields (`-e`) and
    total width (`-E`), thanks to Olivier Schneider.
  - `MultiCpu`: New option to specify fallback icon paths
    (`--fallback-icon-pattern`), thanks to Reed Koser.

_Bug fixes_

  - `CpuFreq` monitors honours `-d` (number of digits) if no suffix is
    given (cf. [issue #335]).
  - Race condition in network monitor fixed ([issue #347]).
  - Limiting dbus supported version (see [issue #346]).

[issue #335]: https://github.com/jaor/xmobar/issues/335
[issue #346]: https://github.com/jaor/xmobar/issues/346
[issue #347]: https://github.com/jaor/xmobar/issues/347

## Version 0.25 (February, 2018)

_New features_

  - The X11 properties `WM_NAME` and `WM_CLASS` for xmobar windows can
    be setup in either the configuration file (`wmName` and `wmClass`)
    or using the new flags `-n` adn `-w` (see [issue #323]).
  - New flag `with_weather`, to build sans `Weather` monitor.

_Bug fixes_

   - Honouring -x in MPris monitor ([issue #325])


[issue #323]: https://github.com/jaor/xmobar/issues/323
[issue #325]: https://github.com/jaor/xmobar/issues/325

## Version 0.24.5 (May, 2017)

_Bug fixes_

  - Fix for vertical bars ([issue #303])

[issue #303]: https://github.com/jaor/xmobar/issues/303

## Version 0.24.4 (April, 2017)

_Infrastructure_

  - Whe have now an active [Travis CI setup].

[Travis CI setup]: https://travis-ci.org/jaor/xmobar

_Bug fixes_

  - Reduce wakeups rate via rtsopts V0 (with new flag with_rtsopts to
    disable it) (see [issue #89]).
  - Fix Net monitor for lage uptimes and bytecounts (Tomas Janoušek).
  - Fix MultiCpu monitor for large uptimes (Zev Weiss).
  - Fix compilation when XMP is disabled (Sergi Trofimovich).
  - Better fatal error messages (Michael Bishop).
  - More paths to read from in CoreTemp (see [issue #291]).

[issue #291]: https://github.com/jaor/xmobar/issues/291

## Version 0.24.3 (Sep 5, 2016)

_Bug fixes_

  - Battery monitor: fixes for cases where status is not consistently
    reported by the kernel (see [issue #271]).

[issue #271]: https://github.com/jaor/xmobar/issues/271

## Version 0.24.2 (Aug 8, 2016)

_Bug fixes_

   - New Weather plugin URL (see [issue #270]).

[issue #270]: https://github.com/jaor/xmobar/issues/270

## Version 0.24.1 (Jul 28, 2016)

_Bug fixes_

   - Restoring compatibility with GHC 7.6, (see [issue #269]).

[issue #269]: https://github.com/jaor/xmobar/issues/269

## Version 0.24 (Jul 26, 2016)

_New features_

   - Use the new compilation flag `with_conduit` to compile an
     implemetation of the  `Weather` plugin that will work behind
     proxies, by Dragos Boca.
   - New command line argument (`-p`) to specify xmobar's position, by
     Valentin Shirokov.
   - Wind speed in Km/h and m/s for the `Weather` plugin, by Michael
     Knabe.
   - `UVMeter`, a new optional plugin showing UV data for Australian
     users, by Roman Joost.
   - New template parameter `<date>` for the `MPD` plugin (by Bruno
     Heridet)
   - New monitor argument `-T` to specify the maximum total width of
     the monitor text.
   - New $VAR parameter syntax for using env vars in pipe monitors, by
     Will Song (see [issue #268]).

_Bug fixes_

   - The `Volume` plugin now supports capture devices (Antoine Eiche).
   - The `MPD` plugin will now automatically update on options changes
     (Ben Boeckel).
   - The `Battery` plugin does now a better job at tracking AC status
     and times (see [issue #231]).
   - `PipeReader` was polling too often (thanks to zlbruce).
   - The `MPris` monitor now honours field width and padding optons
     (-M, -w, etc.).
   - `Batt`: sensible thresholds for high/low power consumption (see
     [issue #265]).

[issue #231]: https://github.com/jaor/xmobar/issues/225
[issue #265]: https://github.com/jaor/xmobar/issues/225
[issue #268]: https://github.com/jaor/xmobar/issues/268

## Version 0.23.1 (Apr 14, 2015)

_Bug fixes_

  - Vertical alignment for icons, [issue #216]
  - Improvements to vertical text alignment, with many thanks to Jan
    Palus (see ongoing discussion in [issue #221]).
  - Better error handling in `Volume` monitor, [issue #215]
  - Compilation with ghc 7.8 and 7.10 (thanks to Edward Tjörnhammar,
    see [issue #225]).

[issue #225]: https://github.com/jaor/xmobar/issues/225
[issue #221]: https://github.com/jaor/xmobar/issues/221
[issue #216]: https://github.com/jaor/xmobar/issues/216
[issue #215]: https://github.com/jaor/xmobar/issues/215

## Version 0.23 (Mar 8, 2015)

_New features_

  - Xmobar can now use a (semi)transparent background: set it with the
    new configuration option `alpha` (thanks to Edward Z. Yang, see
    [issue #114]).
  - Multiple fonts available by means of the new configuration
    parameter `additionalFonts` and the template markers
    `<fn=1>...</fn>`, by Markus Scherer.
  - New variables in `Mem` monitor for available memory as reported by
    Linux 3.14 and newer, by Samuli Thomasson.
  - New configuration parameters `textOffset` and `iconOffset` (see
    discussion in [issue #171] and [issue #201]).
  - New template variable `flags` in `MPD` monitor, by Duncan Burke.

[issue #171]: https://github.com/jaor/xmobar/issues/171
[issue #201]: https://github.com/jaor/xmobar/issues/201
[issue #114]: https://github.com/jaor/xmobar/issues/114

_Bug fixes_

  - We honour the `-S` (show suffix) monitor flag in `CpuFreq`.
  - Better `Weather` parsing of wind direction, by Dino Morelli (see
    [pull #212]).

[pull #212]: https://github.com/jaor/xmobar/pull/212

## Version 0.22.1 (Oct 11, 2014)

_Bug fixes_

  - Fix compilation in systems that don't need mtl newer than the one
    shipped with GHC.

## Version 0.22 (Oct 9, 2014)

_New features_

  - New `MarqueePipeReader` plugin by Reto Hablützel.
  - `Network` monitor has now adaptive units (`-S` is used now to
    switch them on), by Axel Angel.
  - `Weather` now offers `dewPointC` and `dewPointF` instead of
    `dewPoint`, and the new `windCardinal`, `windAzimuth`, `windMph`
    and `windKnots` variables, by Tony Morris.
  - Strings in the configuration file can now span multiple lines
    using Haskell-style multiline script, thanks to dunric
  - Icons can now be also xpm files (if xmobar is compiled with
    `with_xpm`), thanks to Alexander Shabalin.
  - New `borderWidth` option to set xmobar's boder width, thanks to
    Travis Staton.
  - Support for multiple Xft fonts, thanks to Phil Xiaojun Hu and
    Cedric staub (see [pull request #196]).
  - Icon patterns for several monitors, that allow you to specify a
    collection of icons to use for successive integer variable values,
    thanks to Alexander Shabalin (see [pull request #192] and the
    documentation for details).
  - Upgrade to libmpd 0.9.

_Bug fixes_

  - Much more efficient implementation of the `Locks` plugin, thanks
    to Anton Vorontsov (see [pull request #195]).
  - Not colorizing total disk size in `DiskU` ([issue #189]).
  - Avoiding zombies on click actions, thanks to Phil Xiaojun Hu
    ([issue #181]).

[issue #181]: https://github.com/jaor/xmobar/issues/181
[issue #189]: https://github.com/jaor/xmobar/issues/189
[pull request #192]: https://github.com/jaor/xmobar/pull/192
[pull request #195]: https://github.com/jaor/xmobar/pull/195
[pull request #196]: https://github.com/jaor/xmobar/pull/196

## Version 0.21 (Jul 1, 2014)

_New features_

  - Vertical bar indicators using unicode available for most monitors,
    by Eric Mrak.
  - `CoreTemp`: support for linux 3.15, by Tomáš Janoušek.
  - New `CatInt` monitor, displaying integer values read from a file,
    by Nathaniel Wesley Filardo.
  - New `<raw>` tag for safer input parsing, also by Nathaniel.
  - New `UnsafeXMonadLog` monitor (by Eric Mrak) and their xproperty
    counterparts, `UnsafeXPropertyLog` and `UnsafeNamedXPropertyLog`.

_Bug fixes_

  - `Network` now reports status for ppp connections (see
    [issue #89]).
  - Fix for very long running `Cpu` monitors, by Robert J Macomber.

[issue #89]: https://github.com/jaor/xmobar/issues/89

## Version 0.20.1 (March 13, 2014)

_New features_

  - Back to picking by default the first available screen, with a new
    configuration option, `pickBroadest`, for choosing the broadest
    (see [issue #158]).

[issue #158]: https://github.com/jaor/xmobar/issues/158

## Version 0.20 (March 10, 2014)

_New features_

  - Mouse actions now support multiple buttons, by Marcin Mikołajczyk.
  - Non supported monitors are ignored in configuration files (see
    [issue #139]), by Adam Vogt.

_Bug fixes_

  - `Disk` monitor now ignores non-existent devices (Reto Hablützel).
  - `Weather` is now non-blocking and doesn't use curl (Ben Boeckel).
  - Fix for `Memory` monitor in 3.14 kernels (Ben Boeckel).
  - Fix for infinite loops in `AutoMPD` ([issue #76], [issue #111]).
  - More robust AC readings in `BatteryP`.
  - Fix for `Top` monitor's readings for processes whose name contains
    blanks.
  - Fixes for geometry computation on multihead (Dmitry Malikov).
  - Fixes for missing XDG configuration (Thiago Negri and James McCoy,
    see [issue #133]).
  - Compatibility with latest `directory` (1.2.0.2).

[issue #76]: https://github.com/jaor/xmobar/issues/76
[issue #111]: https://github.com/jaor/xmobar/issues/111
[issue #133]: https://github.com/jaor/xmobar/issues/133
[issue #139]: https://github.com/jaor/xmobar/issues/133

## Version 0.19 (October 27, 2013)

As of this release, the old bug tracker at Google code is deprecated.
Please use [Github's tracker] for new bugs.

_New features_

  - New monitor `BatteryN`, a variant of `BatteryP` that lets you
    specify the name of the monitor in the template.
  - Support for configuration file living in `XDG_CONFIG_HOME` (see
    [github #99]).
  - `Com` uses safer `runInteractiveProcess` instead of spawning a
    shell (David McLean).  If you're using shell expansion in your
    `Com` (e.g. "~/bin/script") here's a workaround: `Run Com
    "/bin/bash" ["-c", "~/bin/script"]` (cf. [github #127]).
  - New plugin `UnsafeStdinReader` that allows actions from stdin.
    Now it's possible to have clickable workspaces!
    (Thiago Negri, see [github #125]).
  - New monitor configuration option (`-x` or `--nastring`) that allows
    specifying what string to display when a monitor is not available
    (defaulting to "N/A"). Cf. [github #119].

_Bug fixes_

  - Using the width options `-w`, `-m` and `-M` in battery monitors
    watts display ([github #118]).
  - Using the `-d` option in `CoreTemp` ([github #115])
  - Fix for systems not supporting PCRE regular expressions: we use
    now BCEs, so regex-compat should be enough everywhere (see
    [github #117]).
  - Weather monitor: support for stations without name (Sergei
    Trofimovich, [issue #65]).

[Github's tracker]: https://github.com/jaor/xmobar/issues
[github #99]: https://github.com/jaor/xmobar/issues/115
[github #115]: https://github.com/jaor/xmobar/issues/115
[github #117]: https://github.com/jaor/xmobar/issues/117
[github #125]: https://github.com/jaor/xmobar/issues/125
[issue #65]: http://code.google.com/p/xmobar/issues/detail?id=65
[github #118]: https://github.com/jaor/xmobar/issues/118
[github #119]: https://github.com/jaor/xmobar/issues/119
[github #127]: https://github.com/jaor/xmobar/issues/127

## Version 0.18 (June 5, 2013)

_New features_

  - All extra argument monitors taking a string (e.g. `-O` for
    `BatteryP`) accept now template variables (see [github #109] and
    [#110]).  Thanks to Todd Lunter.

  - New battery monitor extra argument, `-i`, for the idle status.

_Bug fixes_

  - Safer standard input parsing, avoiding <action> injections.


[github #109]: https://github.com/jaor/xmobar/issues/109
[#110]: https://github.com/jaor/xmobar/issues/110

## Version 0.17 (May 5, 2013)

_New features_

  - Icons support: it's now possible to insert bitmaps in the template
    (Edward O'Callaghan, Alexander Polakov and Tomáš Janoušek).
  - Initial support for reacting to mouse clicks (Alexander Polakov).
  - New `TopP` and `BottomP` alignments, taking left and right
    paddings (thanks to Dmitry Malikov).
  - New `<freeratio>` field for memory monitor (Peter Simons).
  - New `allDesktops` and `overrideRedirect` configuration options,
    providing dock behaviour in tiling WMs (when set to True and False
    respectively). Cf. discussion at [github #105].
  - Experimental `-d` (start as a dock) option, may address [github #67]
    in some window managers.

_Bug fixes_

  - Partial (as reports go) fix for [github #77].
  - Safer volume plugin (Dmitry Malikov).
  - Battery percentage capped at 100% (RJ Regenold).

[github #67]: https://github.com/jaor/xmobar/issues/67
[github #77]: https://github.com/jaor/xmobar/issues/77
[github #105]: https://github.com/jaor/xmobar/issues/105


## Version 0.16 (Dec 3, 2012)

_New features_

  - New monitor `AutoMPD`, which uses asynchronous events to display
    MPD status (thanks to Ben Boeckel).
  - New monitor `BufferedPipeReader` displaying data from multiple
    pipes (thanks to Jochen Keil).
  - New monitor `DynNetwork`, which detects the active interface
    automatically, by Reto Hablützel
  - New monitor, `Locks`, displaying the status of lock keys, by
    Patrick Chilton.
  - Extension for DBUS signal handling (Jochen Keil)
  - Hide/Reveal: one can send signals to xmobar and make it (un)hide
    itself (Jochen again).
  - `PipeReader`'s default text is now configurable, by Reto Hablützel.
  - Dependencies updated to latest mtl and libmpd (thanks to Sergei
    Trofimovich).
  - Dependencies on the deprecated dbus-core removed in favour of
    dbus 0.10 (thanks to Jochen Keil).
  - MPris2 now includes genre and composer among its fields.

_Bug fixes_

  - `DiskIO` now can report overall activity in all partitions of a device
    which is not mounted itself (e.g., sda when sda1, sda3, etc. are
    the mounted partitions).  Thanks to John Soros. See [github #73].
  - `DiskU`, the disk usage monitor, works again correctly on Linux,
    instead of randomly crashing every now and then, and reporting
    wrong used size.
  - When using antialiased fonts, we were causing a memory leak in the
    X server by repeatedly allocating colors that, apparently, the
    server doesn't know how to get rid of (even when told so!).  We're
    caching them now and X server memory doesn't grow.
  - Compilation errors and warnings with GHC 7.6 removed (thanks to
    Raghavendra D Prabhu for his reports in [github #71]).

_Known problems_

Some users have reported problems with xmobar compiled with GHC 7.6 in
ArchLinux: see [github #78] and pointers therein.  Please, send
reports of any problems or successes in that regard so that we can fix
any remaining issues.  Thanks!

[github #71]: https://github.com/jaor/xmobar/issues/71
[github #73]: https://github.com/jaor/xmobar/issues/73
[github #78]: https://github.com/jaor/xmobar/issues/78

## Version 0.15 (June 4, 2012)

_Incompatible changes_

  - `Batt` monitor no longer uses `-c` to specify the charge file: it
    should figure things out by itself (cf. [issue 69]).

_New features_

  - New command line option, `-C`, that allows *adding* commands to
    those specified in the configuration file (Ben Boeckel).
  - Dependency on GHC's threaded runtime has been eliminated.
  - New MPRIS (versions 1 and 2) monitor, by Artem Tarasov.
  - New monitor option `-d` to specify the number of decimal places to
    display for float numbers.  Defaults to 0. See [issue 58].
  - New compilation option `--with_threaded`, to use GHC's threaded
    runtime to compile xmobar.  Disabled by default (cf. discussion in
    [github #36]).

_Bug fixes_

  - Stricter build dependencies versioning in cabal file.
  - [issue 56] vertical alignment of text improved.
  - [issue 64] display of `watts` in `Batt` monitor fixed.
  - [issue 69] miscellaneous battery reporting issues.
  - [issue 67] compilation in DragonFly.
  - DiskIO works also when device path in mtab are symbolic links
    to the real device file.
  - Wireless monitor honours padding settings for ESSID names.
  - CoreTemp monitor fixed for newer kernels ([github #38]).

[issue 56]: http://code.google.com/p/xmobar/issues/detail?id=56
[issue 58]: http://code.google.com/p/xmobar/issues/detail?id=58
[issue 64]: http://code.google.com/p/xmobar/issues/detail?id=64
[issue 67]: http://code.google.com/p/xmobar/issues/detail?id=67
[issue 69]: http://code.google.com/p/xmobar/issues/detail?id=69
[github #36]: https://github.com/jaor/xmobar/issues/36
[github #38]: https://github.com/jaor/xmobar/issues/38

## Version 0.14 (Dec 10, 2011)

_New features_

  - New brightness monitor, courtesy of Martin Perner.
  - New DateZone plugin, for configurable timezone and localized
    datetimes, also by Martin.
  - New keyboard layout monitor (Kbd).  Yes, by Martin.
  - Rewrite of the event handling ([issue 53], [issue 57]), you
    guessed it.
  - Cpu monitor now also reports `iowait` field ([issue 55]).
  - Battery monitor: the full charge file is now settable in the
    monitor arguments (olpc systems use `charge_full_design`; see
    [issue 62]).

_Bug fixes_

  - [issue 45] Fix for crashes with AC status changes in battery monitor.
  - [issue 48] The <quality> field of Wireless behaves like a percentage.
  - [issue 50]/[issue 61]: `MPD` monitor now works with libmpd 0.6.
  - [issue 60] Fixes for crashes on power resume for battery monitor.
  - Template sections without fields are now correctly displayed.
  - Catch errors when reading battery status (Ben Boeckel).
  - Compilation issues with ghc 7.x (Sergei Trofimovich).
  - Fixes for CoreTemp monitor in new kernels (Norbert Zeh).
  - Fix for pulseaudio problems in volume monitor (Martin Perner).
  - Fix for parsing errors when a `Run` entry ended in an array
    (Martin).
  - Fixed compilation in OpenBSD (Ivo van der Sangen).

[issue 45]: http://code.google.com/p/xmobar/issues/detail?id=45
[issue 48]: http://code.google.com/p/xmobar/issues/detail?id=48
[issue 50]: http://code.google.com/p/xmobar/issues/detail?id=50
[issue 53]: http://code.google.com/p/xmobar/issues/detail?id=53
[issue 55]: http://code.google.com/p/xmobar/issues/detail?id=55
[issue 57]: http://code.google.com/p/xmobar/issues/detail?id=57
[issue 60]: http://code.google.com/p/xmobar/issues/detail?id=60
[issue 61]: http://code.google.com/p/xmobar/issues/detail?id=61
[issue 62]: http://code.google.com/p/xmobar/issues/detail?id=62

## Version 0.13 (March 28, 2011)

_New features_

  - New `Volume` monitor displaying ALSA soundcards information, by
    Thomas Tuegel.
  - New `ThermalZone` plugin substituting `Thermal` and using linux's
    *sysfs* interface (you need this one if you're using a kernel
    version equal to or above 2.6.37). See [issue 44].
  - xmobar app new has WM_CLASS, WM_NAME and _NET_WM_PID xprops
    ([issue 38]).

_Incompatible changes_

  - In the process of solving [issue 14], we've broken those
    configurations that rely on including alignment separators in the
    input fed to `StdinReader`.
  - The MPD plugin does not accept host and port options anymore: use
    the environment variables MPD_HOST and MPD_PORT instead.
  - The `Mail` plugin now takes a second parameter (a string)
    specifying its alias. As a side-effect, this solves [issue 30].

_Bug fixes_

  - [issue 14] `StdinReader` and other plugins accepting external
    input don't get confused anymore when characters from `alignSep`
    appear in their input.
  - [issue 27] `BottomSize` placement now respects its width argument.
  - [issue 28] Compilation in Mac OS X fixed.
  - [issue 30] `Mail` plugin can be specified anywhere in commands list.
  - [issue 36] Battery monitor now supports non-standard locations of
    the `/sys/class/power_supply/AC/online` file.
  - [issue 40] Battery monitor now supports the new power_now, that
    replaces current_now in linux kernels for v. 2.36 and above.
  - [issue 42] More accurate net monitor rates.
  - DiskIO, Cpu and MultiCpu monitors are also more accurate now.
  - Text is now correctly centered vertically.
  - `FullBM` border spec fixed.

[issue 14]: http://code.google.com/p/xmobar/issues/detail?id=14
[issue 27]: http://code.google.com/p/xmobar/issues/detail?id=27
[issue 28]: http://code.google.com/p/xmobar/issues/detail?id=28
[issue 30]: http://code.google.com/p/xmobar/issues/detail?id=30
[issue 36]: http://code.google.com/p/xmobar/issues/detail?id=36
[issue 38]: http://code.google.com/p/xmobar/issues/detail?id=38
[issue 40]: http://code.google.com/p/xmobar/issues/detail?id=40
[issue 42]: http://code.google.com/p/xmobar/issues/detail?id=42
[issue 44]: http://code.google.com/p/xmobar/issues/detail?id=44

## Version 0.12 (Dec 24, 2010)

xmobar has a new [maintainer], a new [website], a new [mailing
list] and uses a new [source code repository].

Many thanks to Andrea Rossato, xmobar's author and maintainer so far,
for creating xmobar in the first place, and for giving me the chance
to become its maintainer. And a big thanks to Ben Boeckel, Petr Rockai
and Norbert Zeh for their patches.

[website]: http://projects.haskell.org/xmobar/
[mailing list]: http://projects.haskell.org/cgi-bin/mailman/listinfo/xmobar
[source code repository]: https://github.com/jaor/xmobar
[maintainer]: http://hacks-galore.org/jao/

_New features_

  - Window borders: configuration options `border` and `borderColor`
    allow drawing borders around xmobar's window.
  - New monitor, `Uptime`, showing the system uptime.
  - New monitor argument (`-S`) to enable displaying the `%` symbol in
    percentages or other suffixes (e.g., units in Uptime and Network);
    the symbol is now never included by default.
  - New 'run once' commands, by specifying a 0 refresh rate in `Run
    Com` ([issue 26]).
  - MPD monitor: updated to libmpd 1.5. New fields `ppos` (playlist
    position) and `remaining` (remaining time). New configuration
    options to specify MPD's host, user name and password.
  - Battery monitor: new `watts` and `timeleft` fields (Petr Rockai),
    and specific arguments to control coloring and thresholds of the
    former.
  - MultiCPU monitor: new `auto*` fields that automatically detect all
    present CPUs (Ben Boeckel).
  - CpuFreq monitor uses just one decimal digit for GHz values (Petr
    Rockai).
  - Mail plugin expands paths starting with "~/" (Ben Boeckel). Ditto
    MBox.
  - Weather monitor now skips not retrieved fields, instead of
    displaying a long error message.
  - New compilation flag, `all_extensions`.
  - Documentation and website updates.

_Bug fixes_

  - [issue 23] Wireless monitor is now compatible with iwlib 29.
  - [issue 24] Swap monitor's used ratio display fixed.
  - [issue 25] Percentages only include `%` if requested using `-P`.
  - [issue 31] MPD monitor now respects `-W` argument.
  - Fixes in CPU frequency formatting, string alignment and colour
    boxes in monitors (Norbert Zeh).
  - TopMem and TopProc now use the `-L` and `-H` options correctly for
    memory template fields.
  - MBox skips non-existent mbox paths instead of hanging.

[issue 23]: http://code.google.com/p/xmobar/issues/detail?id=23
[issue 24]: http://code.google.com/p/xmobar/issues/detail?id=24
[issue 25]: http://code.google.com/p/xmobar/issues/detail?id=25
[issue 26]: http://code.google.com/p/xmobar/issues/detail?id=26
[issue 31]: http://code.google.com/p/xmobar/issues/detail?id=31
