definition module saplinterface

import StdEnv, syntax
from partition import ::Component

gensaplfiles :: !Files {#DclModule} {#{#CheckedTypeDef}} !*{!Component} !*{# FunDef} CommonDefs {#CommonDefs} Ident  [IndexRange] !String
                -> (!Files,!*{!Component}, !*{# FunDef})
