{-# LANGUAGE QuasiQuotes #-}
module Css
    ( Theme
    , layout, theme
    , theme_dark, theme_wrong
    , connectome
    ) where

import Data.Text (Text)
import NeatInterpolation

layout :: Text
layout = [text|
body {
    display: flex;
    flex-direction: column;
    height: 100vh; width: 100vw;
    margin: 0px;
    overflow: hidden;
}

body > nav {
    flex: 0 0 auto;
    display: flex;
    flex-direction: row;
    align-items: center;
    padding: 0.3em 1em 0px;
}
h1 {
    margin: 0px;
}

.spacer {
    flex: 1 0 0px;
}

#editor {
    flex: 1 1 auto;
    display: flex;
    flex-direction: column;
    padding: 0.5em 0.7em;
}
#editor > ul.tabs {
    flex: 0 0 auto;
    padding: 0px;
    margin: 0px;
}
#editor > ul.tabs > li {
    display: inline;
    list-style-type: none;
    padding: 0.1em 0.5em 0px;
    cursor: pointer;
}
#editor > div.tab {
    flex: 1 1 auto;
    position: relative;
    margin: 0px;
    padding: 0px 0.5em;
    overflow-y: auto;
}

button.compile {
    margin: 0px;
    border: thin rgba(0,0,0,0) solid;
    border-radius: $borderRadius;
    background: none;
    padding: 0px 3px;
    font: inherit;
    font-size: calc(100% - 2px);
}
textarea.compile {
    font-family: monospace;
}

/* Aspect Ratio using CSS from: http://www.mademyday.de/css-height-equals-width-with-pure-css.html */
.aspect-9-16 {
    position: relative;
}
.aspect-9-16:before {
    content: "";
    display: block;
    padding-top: 56.25%;
}
.aspect-9-16 > * {
    position: absolute;
    top: 0px;
}

.connectome {
    width: calc(96% - 2px);
    border: thin rgba(0,0,0,0) solid;
    overflow: auto;
    display: flex;
    flex-direction: row;
    align-items: center;
    justify-content: center;
}
.connectome > * {
    flex: 0 0 auto;
}
|]

type Theme = ((Text, Text, Text), (Text, Text), (Text, Text, Text))

theme :: Theme -> Text
theme ((bgndHeavy, bgnd, bgndChill), (ctnt, ctntBright), (accent, activate, warning)) = [text|
body {
    background: $bgnd;
    color: $ctnt
}

ul.tabs > li {
    border: thin $bgndChill solid;
    border-bottom: none;
    border-radius: $borderRadius $borderRadius 0px 0px;
    color: $bgndChill;
}
ul.tabs > li.active {
    border-color: $ctntBright;
    color: $ctntBright;
}
div.tab {
    border: thin $ctntBright solid;
    border-radius: $borderRadius;
    border-top-left-radius: 0px;
}
button.compile {
    border-color: $activate;
    color: $activate;
}

#netlist {
    border-color: $bgndHeavy;
}
|]

theme_dark, theme_wrong :: Theme
theme_dark = ((base03, base02, base01), (base0, base1), (blue, green, orange))
theme_wrong = ((base3, base2, base1), (base01, base00), (blue, magenta, orange))


connectome :: Theme -> Text
connectome ((bgndHeavy, bgnd, bgndChill), (ctnt, ctntBright), (accent, activate, warning)) = [text|
.connectome svg {
    stroke: $ctnt;
    fill: none;
    stroke-linecap: round;
    stroke-linejoin: round;
}

.connectome .components {
    stroke-width: 7;
}
.connectome .components .pins {
    stroke-width: 5;
}
.connectome .nets {
    stroke-width: 2.5;
}
.connectome .components > [data-instance-name]:hover,
.connectome .nets > [data-net-name]:hover {
    stroke: $accent;
}
|]


borderRadius = "5px"

--        mine        solarized
base03  = "#1A2529" {-"#243236"-}
base02  = "#283336" {-"#323F42"-}
base01  = "#51595C" {-"#586e75"-}
base00  = "#767F83" {-"#657b83"-}
base0   = "#909696" {-"#839496"-}
base1   = "#9EA1A1" {-"#93a1a1"-}
base2   = "#EEE9D9" {-"#eee8d5"-}
base3   = "#FDF9EE" {-"#fdf6e3"-}
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"
