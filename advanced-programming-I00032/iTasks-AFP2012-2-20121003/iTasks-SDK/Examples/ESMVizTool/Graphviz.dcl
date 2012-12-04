// Peter Divianszky, 2007
// Code extended and adapted by Peter Achten, 2007, Pieter Koopman 2010

definition module Graphviz

from   StdOverloaded import class toString
import StdMaybe
import GenEq_NG

// A digraph contains a title and a list of node definitions
:: Digraph 
    = Digraph String [GraphAttribute] [NodeDef] (Maybe SelectedItem)
:: SelectedItem
	= SelectedItem Int

digraphTitle		:: !Digraph -> String
digraphAtts			:: !Digraph -> [GraphAttribute]
digraphNodes		:: !Digraph -> [NodeDef]
digraphSelectedItem	:: !Digraph -> Maybe SelectedItem

// A node definition contains a unique identifier (an integer), a list of node attributes and a list of edge definitions.
// An edge definition contains an identifier (the id of the end node and edge attributes).
:: NodeDef 
    = NodeDef !Int ![NodeState] ![NodeAttribute] [EdgeDef]
:: EdgeDef
    :== (!Int,![EdgeAttribute])
:: NodeState
	= NStAllEdgesFound !Bool		// all edges of this node are known

// Convert digraph into list of strings.
// The strings are lines of the graphviz representation of the graph.
printDigraph :: !Digraph -> [String]

:: GraphAttribute
	= GAtt_Damping        Real
	| GAtt_K              Real
	| GAtt_URL            String
	| GAtt_bb             Rect
	| GAtt_bgcolor        Color
	| GAtt_center         Bool
	| GAtt_charset        String
	| GAtt_clusterrank    ClusterMode
	| GAtt_colorscheme    String
	| GAtt_comment        String
	| GAtt_compound       Bool
	| GAtt_concentrate    Bool
	| GAtt_defaultdist    Real
	| GAtt_dim            Int
//	| GAtt_diredgeconstraints ... PA: ignored, neato only
	| GAtt_dpi            Real
	| GAtt_epsilon        Real
	| GAtt_esep           Real
	| GAtt_fontcolor      Color
	| GAtt_fontname       String
	| GAtt_fontnames      String
	| GAtt_fontpath       String
	| GAtt_fontsize       Real
	| GAtt_label          String
	| GAtt_labeljust      String
	| GAtt_labelloc       String
	| GAtt_landscape      Bool
	| GAtt_layers         LayerList
	| GAtt_layersep       String
	| GAtt_levelsgap      Real
	| GAtt_lp             DotPoint
	| GAtt_margin         Margin
	| GAtt_maxiter        Int
	| GAtt_mclimit        Real
	| GAtt_mindist        Real
	| GAtt_mode           String
	| GAtt_model          String
	| GAtt_mosek          Bool
	| GAtt_nodesep        Real
	| GAtt_nojustify      Bool
	| GAtt_normalize      Bool
	| GAtt_nslimit        Real
	| GAtt_nslimit1       Real
	| GAtt_ordering       String
	| GAtt_orientation    String
	| GAtt_outputorder    OutputMode
	| GAtt_pad            Pad
	| GAtt_page           Pointf
	| GAtt_pagedir        PageDir
	| GAtt_quantum        Real
	| GAtt_rank           RankType
	| GAtt_rankdir        RankDir
	| GAtt_ranksep        Real
	| GAtt_ratio          Ratio
	| GAtt_remincross     Bool
	| GAtt_resolution     Real
	| GAtt_root           String
	| GAtt_rotate         Int
	| GAtt_searchsize     Int
	| GAtt_showboxes      Int
	| GAtt_size           Sizef //Pointf		// PA++
//	| GAtt_splines        PA: skipped for the time being
	| GAtt_start          StartType
	| GAtt_stylesheet     String
	| GAtt_target         String
	| GAtt_truecolor      Bool
	| GAtt_viewport       ViewPort
	| GAtt_voro_margin    Real
:: NodeAttribute
    = NAtt_URL            String
    | NAtt_color          Color
    | NAtt_colorscheme    String
    | NAtt_comment        String
    | NAtt_distortion     Real
    | NAtt_fillcolor      Color
    | NAtt_fixedsize      Bool
    | NAtt_fontcolor      Color
    | NAtt_fontname       String
    | NAtt_fontsize       Real
    | NAtt_group          String
    | NAtt_height         Real
    | NAtt_label          String
    | NAtt_layer          LayerRange
    | NAtt_margin         Margin
    | NAtt_nojustify      Bool
    | NAtt_orientation    Real
    | NAtt_peripheries    Int
    | NAtt_pin            Bool
//  | NAtt_pos ...        PA: ignored for the time being
    | NAtt_rects          Rect
    | NAtt_regular        Bool
    | NAtt_samplepoints   Int
    | NAtt_shape          NodeShape
    | NAtt_shapefile      String
    | NAtt_showboxes      Int
    | NAtt_sides          Int
    | NAtt_skew           Real
    | NAtt_style          NodeStyle
    | NAtt_target         String
    | NAtt_tooltip        String
    | NAtt_width          Real
    | NAtt_z              Real
:: EdgeAttribute
    = EAtt_URL            String
    | EAtt_arrowhead      ArrowType
    | EAtt_arrowsize      Real
    | EAtt_arrowtail      ArrowType
    | EAtt_color          Color
    | EAtt_colorscheme    String
    | EAtt_comment        String
    | EAtt_constraint     Bool
    | EAtt_decorate       Bool
    | EAtt_dir            DirType
    | EAtt_edgeURL        String
    | EAtt_edgehref       String
    | EAtt_edgetarget     String
    | EAtt_edgetooltip    String
    | EAtt_fontcolor      Color
    | EAtt_fontname       String
    | EAtt_fontsize       Real
    | EAtt_headURL        String
    | EAtt_headclip       Bool
    | EAtt_headhref       String
    | EAtt_headlabel      String
    | EAtt_headport       PortPos
    | EAtt_headtarget     String
    | EAtt_headtooltip    String
    | EAtt_href           String
    | EAtt_label          String
    | EAtt_labelURL       String
    | EAtt_labelangle     Real
    | EAtt_labeldistance  Real
    | EAtt_labelfloat     Bool
    | EAtt_labelfontcolor Color
    | EAtt_labelfontname  String
    | EAtt_labelfontsize  Real
    | EAtt_labelhref      String
    | EAtt_labeltarget    String
    | EAtt_labeltooltip   String
    | EAtt_layer          LayerRange
    | EAtt_len            Real
    | EAtt_lhead          String
    | EAtt_lp             DotPoint
    | EAtt_ltail          String
    | EAtt_minlen         Int
    | EAtt_nojustify      Bool
//  | EAtt_pos			PA: ignored for the time being
	| EAtt_samehead       String
	| EAtt_sametail       String
	| EAtt_showboxes      Int
	| EAtt_style          EdgeStyle
	| EAtt_tailURL        String
	| EAtt_tailclip       Bool
	| EAtt_tailhref       String
	| EAtt_taillabel      String
	| EAtt_tailport       PortPos
	| EAtt_tailtarget     String
	| EAtt_tailtooltip    String
	| EAtt_target         String
	| EAtt_tooltip        String
	| EAtt_weight         Real
:: ClusterMode
	= CM_local | CM_global | CM_none
:: CompassPoint
	= CP_n | CP_ne | CP_e | CP_se | CP_s | CP_sw | CP_w | CP_nw
:: DotPoint
	= DotPoint Real Real Bool
:: LayerId
    = LayerAll
    | LayerNr   Int
    | LayerName String
:: LayerList
	= LayerList [String]
:: LayerRange
    = LayerRange LayerId [LayerId]
:: Margin
    = SingleMargin Real
    | DoubleMargin Real Real
:: OutputMode
	= OM_breadthfirst | OM_nodesfirst | OM_edgesfirst
:: Pad
	= SinglePad Real
	| DoublePad Real Real
:: PageDir
	= PD_BL | PD_BR | PD_TL | PD_TR | PD_RB | PD_RT | PD_LB | PD_LT
:: Pointf
	= Pointf Real Real
:: PortPos				// PA: for now only compass points are supported
	:== CompassPoint
:: RankDir
	= RD_TB | RD_LR | RD_BT | RD_RL
:: RankType
	= RT_same | RT_min | RT_source | RT_max | RT_sink
:: Ratio
	= AspectRatio Real
	| R_fill
	| R_compress
	| R_expand
	| R_auto
:: Rect
    = {llx :: Int,lly :: Int, urx :: Int, ury :: Int}
:: Sizef		// PA++
	= Sizef Real Real Bool
:: StartStyle
	= SS_regular | SS_self | SS_random
:: StartType
	= { startStyle :: Maybe StartStyle
	  , startSeed  :: Maybe Int
	  }
:: ViewPort
	= { vp_W       :: Real
	  , vp_H       :: Real
	  , vp_Z       :: Maybe Real
	  , vp_xy      :: Maybe Pointf
	  }

pointNode           :: [NodeAttribute] // attributes of a point-shaped node
hiddenNode          :: [NodeAttribute] // attributes of a hidden node


:: NodeShape 
    = NShape_box
    | NShape_circle 
    | NShape_diamond
    | NShape_doublecircle
    | NShape_doubleoctagon
    | NShape_egg
    | NShape_ellipse 
    | NShape_hexagon
    | NShape_house
    | NShape_invtriangle
    | NShape_invtrapezium
    | NShape_invhouse
    | NShape_octagon
    | NShape_Mdiamond
    | NShape_Msquare
    | NShape_Mcircle
    | NShape_parallelogram 
    | NShape_pentagon
    | NShape_plainText
    | NShape_polygon
    | NShape_point 
    | NShape_rect
    | NShape_rectangle
    | NShape_septagon
    | NShape_trapezium
    | NShape_triangle
    | NShape_tripleoctagon
    | NShape_none
instance toString NodeShape
instance ==       NodeShape
derive gEq NodeShape // PK++

::  NodeStyle
    = NStyle_filled
    | NStyle_invis
    | NStyle_diagonals
    | NStyle_rounded
    | NStyle_dashed
    | NStyle_dotted
    | NStyle_solid
    | NStyle_bold
instance toString NodeStyle
instance ==       NodeStyle
derive gEq NodeStyle // PK++

:: EdgeStyle 
    = EStyle_solid 
    | EStyle_bold
    | EStyle_dashed
    | EStyle_dotted
    | EStyle_invis
instance toString EdgeStyle
instance ==       EdgeStyle
derive gEq EdgeStyle // PK++

:: Color 
    = RGB   Int  Int  Int
    | HSV   Real Real Real
    | Color String          // X11 1.2 color names; see rgb.txt

C_black   :== Color "black"
C_white   :== Color "white"
C_gray    :== Color "gray"
C_red     :== Color "red"
C_green   :== Color "green"
C_blue    :== Color "blue"
C_yellow  :== Color "yellow"

instance toString Color
instance ==       Color
derive gEq Color // PK++

:: ArrowType =
    { closest       :: Arrow
    , furthest      :: Maybe Arrow
    }
:: Arrow =
    { open          :: Bool
    , side          :: Maybe Side
    , shape         :: ArrowShape
    }
:: Side
    = Side_l
    | Side_r
:: ArrowShape
    = AShape_box
    | AShape_crow
    | AShape_diamond
    | AShape_dot
    | AShape_inv
    | AShape_none
    | AShape_normal
    | AShape_tee
    | AShape_vee
instance toString ArrowType
instance ==       ArrowType
derive gEq ArrowType // PK++

// direction of the edge
:: DirType
    = DT_forward 
    | DT_back 
    | DT_both 
    | DT_none
instance toString DirType
instance ==       DirType
derive gEq DirType // PK++

layersep :== ":\t"

