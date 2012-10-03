definition module EngineWrapperStandalone
/**
* This module wraps the iTasks engine in a simple
* standalone web server. This allows for easy testing and playing
* with the system
*/
import Engine

/**
* Starts the task engine with a list of published task definitions.
*
* @param Tasks to start
* @param The world
* @return The world
*/
startEngine :: a !*World -> *World | Publishable a
