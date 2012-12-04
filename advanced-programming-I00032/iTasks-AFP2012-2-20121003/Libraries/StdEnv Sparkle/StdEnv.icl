implementation	module	StdEnv

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1995 University of Nijmegen
//
// This is a modified version of the Standard Environment for Clean 2.0.
// The modifications were made to make it easier to prove properties of programs
// using Sparkle.
//
// The modifications do not change the semantics of the Standard Environment
// (except for drop and take on negative arguments and all functions that
//  depend on them), but do effect the efficiency of programs. It is therefore
// safe to use this standard environment, but programs may run slower.
//
// Additionally, several functions have been added to the Standard Environment.
// These functions are all defined in the module StdSparkle.
//
// All modifications to the standard environment are marked with the comment
// tags 'Sparkle'.
//
// Maarten de Mol.
// 8 January 2002
// ****************************************************************************************

